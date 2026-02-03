#include "Common/Constant/Constant.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Common/Printer/Printer.h"
#include "Utility/Disassembler/Disassembler.h"
#include "Interpreter/Runtime/MidoriRuntime.h"
#include "Library/DynamicFFIRegistry/DynamicFFIRegistry.h"
#include "VirtualMachine.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#endif



#include <algorithm>
#include <bit>
#include <cmath>
#include <cstring>
#include <execution>
#include <format>
#include <numeric>
#include <ranges>

using namespace std::string_literals;

namespace
{
	bool IsModuleBootstrapName(const MidoriText& name) noexcept
	{
		std::string_view view(name.GetCString());
		return view.starts_with(MODULE_BOOTSTRAP_PREFIX);
	}
}

VirtualMachine::VirtualMachine(MidoriRuntime& runtime) noexcept
	: m_runtime(&runtime)
{
	m_executable = &runtime.GetExecutable();
	m_global_vars = runtime.GetGlobalsPtr();
	m_string_literal_cache.resize(m_executable->GetStringPool().size(), nullptr);

	InitializeStacks();

	constexpr int runtime_startup_proc_index = 0;
	m_instruction_pointer = &*m_executable->GetBytecodeStream(runtime_startup_proc_index).cbegin();
}

VirtualMachine::VirtualMachine(MidoriRuntime& runtime, const MidoriClosure& entry_closure) noexcept
	: m_runtime(&runtime)
{
	m_executable = &runtime.GetExecutable();
	m_global_vars = runtime.GetGlobalsPtr();
	m_string_literal_cache.resize(m_executable->GetStringPool().size(), nullptr);

	InitializeStacks();

	MidoriTraceable* closure_traceable = AllocateTraceable(MidoriClosure{.m_cell_values = entry_closure.m_cell_values, .m_proc_index = entry_closure.m_proc_index});
	m_curr_closure_traceable = closure_traceable;
	m_curr_environment = &closure_traceable->GetTraceable<MidoriClosure>().m_cell_values;

	m_instruction_pointer = &*m_executable->GetBytecodeStream(entry_closure.m_proc_index).cbegin();
}


void VirtualMachine::InitializeStacks() noexcept
{
#ifdef _WIN32
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	size_t page_size = si.dwPageSize;

	size_t value_stack_bytes = s_value_stack_size * sizeof(MidoriValue);
	size_t value_total_pages = (value_stack_bytes + page_size - 1u) / page_size + 1u;
	size_t value_total_size = value_total_pages * page_size;

	m_value_stack_region = VirtualAlloc(nullptr, value_total_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	if (m_value_stack_region)
	{
		m_value_stack_begin = static_cast<MidoriValue*>(m_value_stack_region);

		void* value_guard_page = static_cast<char*>(m_value_stack_region) + (value_total_size - page_size);
		DWORD old_protect;
		VirtualProtect(value_guard_page, page_size, PAGE_NOACCESS, &old_protect);
	}

	size_t call_stack_bytes = s_call_stack_size * sizeof(CallFrame);
	size_t call_total_pages = (call_stack_bytes + page_size - 1u) / page_size + 1u;
	size_t call_total_size = call_total_pages * page_size;

	m_call_stack_region = VirtualAlloc(nullptr, call_total_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	if (m_call_stack_region)
	{
		m_call_stack_begin = static_cast<CallFrame*>(m_call_stack_region);

		void* call_guard_page = static_cast<char*>(m_call_stack_region) + (call_total_size - page_size);
		DWORD old_protect;
		VirtualProtect(call_guard_page, page_size, PAGE_NOACCESS, &old_protect);
	}
#else
	m_value_stack_begin = static_cast<MidoriValue*>(std::malloc(s_value_stack_size * sizeof(MidoriValue)));
	m_call_stack_begin = static_cast<CallFrame*>(std::malloc(s_call_stack_size * sizeof(CallFrame)));
#endif

	m_value_stack_base_pointer = m_value_stack_begin;
	m_value_stack_pointer = m_value_stack_base_pointer;
	m_call_stack_pointer = m_call_stack_begin;
}

MidoriValue VirtualMachine::GetAsyncResult() const noexcept
{
	return m_async_result;
}

VirtualMachine::~VirtualMachine()
{
	GarbageCollector::GarbageCollectionRoots roots;
	m_gc.ReclaimMemory(roots, m_allocator, true);

#ifdef _WIN32
	if (m_value_stack_region)
	{
		VirtualFree(m_value_stack_region, 0, MEM_RELEASE);
	}
	if (m_call_stack_region)
	{
		VirtualFree(m_call_stack_region, 0, MEM_RELEASE);
	}
#else
	std::free(m_value_stack_begin);
	std::free(m_call_stack_begin);
#endif
}

int VirtualMachine::TerminateExecution(std::string_view message) noexcept
{
	Printer::Print<Printer::Color::RED>(message);
	return EXIT_FAILURE;
}

int VirtualMachine::GetLine() noexcept
{
	for (int i : std::views::iota(0, m_executable->GetProcedureCount()))
	{
		const BytecodeStream& bytecode = m_executable->GetBytecodeStream(i);
		const OpCode* start = &*bytecode.cbegin();
		const OpCode* end = start + bytecode.GetByteCodeSize();

		if (m_instruction_pointer >= start && m_instruction_pointer < end)
		{
			return m_executable->GetLine(static_cast<int>(m_instruction_pointer - start), i);
		}
	}

	return TerminateExecution(GenerateRuntimeError("Invalid instruction pointer.", 0));
}

std::string VirtualMachine::GenerateRuntimeError(std::string_view message, int line) noexcept
{
	std::string stack_trace = GenerateStackTrace();
	return MidoriError::GenerateRuntimeError(message, line)
		.append("\n")
		.append(stack_trace);
}

int VirtualMachine::GetProcedureIndexFromIP(InstructionPointer ip) noexcept
{
	for (int i : std::views::iota(0, m_executable->GetProcedureCount()))
	{
		const BytecodeStream& bytecode = m_executable->GetBytecodeStream(i);
		const OpCode* start = &*bytecode.cbegin();
		const OpCode* end = start + bytecode.GetByteCodeSize();

		if (ip >= start && ip < end)
		{
			return i;
		}
	}
	return -1;
}

int VirtualMachine::GetLineFromIP(InstructionPointer ip, int proc_index) noexcept
{
	if (proc_index < 0 || proc_index >= m_executable->GetProcedureCount())
	{
		return 0;
	}

	const BytecodeStream& bytecode = m_executable->GetBytecodeStream(proc_index);
	const OpCode* start = &*bytecode.cbegin();
	int offset = static_cast<int>(ip - start);

	return m_executable->GetLine(offset, proc_index);
}

std::string VirtualMachine::GenerateStackTrace() noexcept
{
	std::string trace = std::string(STACK_TRACE_HEADER);
	std::string_view file_name = m_executable->GetFileName();
	std::string_view function_color = Printer::Detail::GetColorCode(Printer::Color::BRIGHT_YELLOW);
	std::string_view reset = "\033[0m";

	// Current frame (where error occurred)
	int current_proc = GetProcedureIndexFromIP(m_instruction_pointer);
	int current_line = GetLineFromIP(m_instruction_pointer, current_proc);

	if (current_proc >= 0 && current_proc < static_cast<int>(m_executable->m_procedure_names.size()))
	{
		if (!IsModuleBootstrapName(m_executable->m_procedure_names[current_proc]))
		{
			trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, m_executable->m_procedure_names[current_proc].GetCString(), reset, file_name, current_line));
		}
	}
	else
	{
		trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, ANONYMOUS_FUNCTION, reset, file_name, current_line));
	}

	// Walk the call stack
	CallStackPointer frame_ptr = m_call_stack_pointer - 1;
	int frame_count = 0;
	int total_frames = static_cast<int>(m_call_stack_pointer - m_call_stack_begin);

	while (frame_ptr >= m_call_stack_begin && frame_count < s_max_stack_trace_depth - 1)
	{
		const CallFrame& frame = *frame_ptr;

		int proc_index = GetProcedureIndexFromIP(frame.return_ip);
		int line = GetLineFromIP(frame.return_ip, proc_index);

		bool should_skip = false;
		if (proc_index >= 0 && proc_index < static_cast<int>(m_executable->m_procedure_names.size()))
		{
			if (!IsModuleBootstrapName(m_executable->m_procedure_names[proc_index]))
			{
				trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, m_executable->m_procedure_names[proc_index].GetCString(), reset, file_name, line));
			}
			else
			{
				should_skip = true;
			}
		}
		else
		{
			trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, ANONYMOUS_FUNCTION, reset, file_name, line));
		}

		--frame_ptr;
		if (!should_skip)
		{
			++frame_count;
		}
	}

	// Add truncation message if there are more frames
	if (frame_count >= s_max_stack_trace_depth - 1 && total_frames > s_max_stack_trace_depth)
	{
		int remaining = total_frames - s_max_stack_trace_depth;
		trace.append(std::format("  ... ({} more frame{})\n", remaining, remaining == 1 ? "" : "s"));
	}

	return trace;
}

void VirtualMachine::PromoteCells() noexcept
{
	if (m_cells_to_promote.empty())
	{
		return;
	}
	else
	{
		for (MidoriCellValue* cell : m_cells_to_promote)
		{
			if (!cell->m_is_on_heap && cell->GetStackPointer() >= m_value_stack_base_pointer)
			{
				cell->m_is_on_heap = true;
				cell->m_data = *cell->GetStackPointer();
			}
		}
		m_cells_to_promote.clear();
	}
}

int VirtualMachine::CheckIndexBounds(MidoriValue index, MidoriInteger size) noexcept
{
	MidoriInteger val = index.GetInteger();
	if (val < 0ll || val >= size)
	{
		return TerminateExecution(GenerateRuntimeError(std::format("Index out of bounds at index: {}.", val), GetLine()));
	}
	else
	{
		return 0;
	}
}

int VirtualMachine::CheckNewArraySize(MidoriInteger size) noexcept
{
	if (size < 0)
	{
		return TerminateExecution(GenerateRuntimeError("Array size cannot be negative.", GetLine()));
	}
	else if (size > MAX_ARRAY_SIZE)
	{
		return TerminateExecution(GenerateRuntimeError("Array size exceeds maximum array size.", GetLine()));
	}
	return 0;
}

int VirtualMachine::CheckArrayPopResult(const std::optional<MidoriValue>& result) noexcept
{
	if (!result.has_value())
	{
		return TerminateExecution(GenerateRuntimeError("Cannot pop from an empty array.", GetLine()));
	}
	return 0;
}

void VirtualMachine::BuildGarbageCollectionRoots(GarbageCollector::GarbageCollectionRoots& roots) const noexcept
{
	roots.clear();

	size_t stack_count = 0uz;
	if (m_value_stack_begin != nullptr && m_value_stack_pointer != nullptr)
	{
		stack_count = static_cast<size_t>(m_value_stack_pointer - m_value_stack_begin);
	}

	size_t global_count = 0uz;
	if (m_global_vars != nullptr)
	{
		global_count = m_global_vars->size();
	}

	roots.reserve(stack_count + global_count + m_string_literal_cache.size() + m_small_string_pool.size() + 1uz);

	if (stack_count > 0uz)
	{
		for (MidoriValue* it = m_value_stack_begin; it != m_value_stack_pointer; ++it)
		{
			MidoriTraceable* ptr = it->GetPointer();
			if (ptr && m_gc.Contains(ptr))
			{
				roots.emplace_back(ptr);
			}
		}
	}

	if (m_global_vars != nullptr)
	{
		for (const MidoriValue& val : *m_global_vars)
		{
			MidoriTraceable* ptr = val.GetPointer();
			if (ptr != nullptr && m_gc.Contains(ptr))
			{
				roots.emplace_back(ptr);
			}
		}
	}

	if (m_curr_closure_traceable != nullptr)
	{
		roots.emplace_back(m_curr_closure_traceable);
	}

	for (MidoriTraceable* cached_string : m_string_literal_cache)
	{
		if (cached_string)
		{
			roots.emplace_back(cached_string);
		}
	}

	for (const auto& [key, value] : m_small_string_pool)
	{
		if (value)
		{
			roots.emplace_back(value);
		}
	}
}

GarbageCollector::GarbageCollectionRoots VirtualMachine::GetGarbageCollectionRoots() const noexcept
{
	GarbageCollector::GarbageCollectionRoots roots;
	BuildGarbageCollectionRoots(roots);
	return roots;
}

MidoriTraceable* VirtualMachine::InternSmallString(const MidoriText& text) noexcept
{
	constexpr int SMALL_STRING_THRESHOLD = 4;

	int byte_length = text.GetByteLength();
	if (byte_length > SMALL_STRING_THRESHOLD)
	{
		return nullptr;
	}

	std::string_view key(text.GetCString(), static_cast<size_t>(byte_length));
	std::unordered_map<std::string_view, MidoriTraceable*>::iterator it = m_small_string_pool.find(key);
	if (it != m_small_string_pool.end())
	{
		return it->second;
	}

	MidoriText text_copy(text);
	MidoriTraceable* interned = AllocateTraceable(std::move(text_copy));
	m_small_string_pool[std::string_view(interned->GetTraceable<MidoriText>().GetCString(), static_cast<size_t>(byte_length))] = interned;
	return interned;
}

int VirtualMachine::ExecuteLoop() noexcept
{
	while (true)
	{

#if MIDORI_ENABLE_STACK_TRACE
		Printer::Print("          ");
#ifdef __EMSCRIPTEN__
		std::for_each
		(
			m_value_stack_begin,
			m_value_stack_base_pointer - 1 < m_value_stack_begin ? m_value_stack_begin : m_value_stack_base_pointer - 1,
			[](MidoriValue value) -> void
			{
				Printer::Print<Printer::Color::YELLOW>(("[ "s + value.ToText().GetCString() + " ]"s));
			}
		);
		std::for_each
		(
			m_value_stack_base_pointer,
			m_value_stack_pointer,
			[](MidoriValue value) -> void
			{
				Printer::Print<Printer::Color::GREEN>(("[ "s + value.ToText().GetCString() + " ]"s));
			}
		);
#else
		std::for_each
		(
			std::execution::seq,
			m_value_stack_begin,
			m_value_stack_base_pointer - 1 < m_value_stack_begin ? m_value_stack_begin : m_value_stack_base_pointer - 1,
			[](MidoriValue value) -> void
			{
				Printer::Print<Printer::Color::YELLOW>(("[ "s + value.ToText().GetCString() + " ]"s));
			}
		);
		std::for_each
		(
			std::execution::seq,
			m_value_stack_base_pointer,
			m_value_stack_pointer,
			[](MidoriValue value) -> void
			{
				Printer::Print<Printer::Color::GREEN>(("[ "s + value.ToText().GetCString() + " ]"s));
			}
		);
#endif
		Printer::Print("\n");
		int dbg_instruction_pointer = -1;
		int dbg_proc_index = -1;

		for (int i : std::views::iota(0, m_executable->GetProcedureCount()))
		{
			const BytecodeStream& bytecode = m_executable->GetBytecodeStream(i);
			const OpCode* start = &*bytecode.cbegin();
			const OpCode* end = start + bytecode.GetByteCodeSize();

			if (m_instruction_pointer >= start && m_instruction_pointer < end)
			{
				dbg_proc_index = i;
				dbg_instruction_pointer = static_cast<int>(m_instruction_pointer - start);
			}
		}
#if MIDORI_ENABLE_DISASSEMBLY
		Disassembler::DisassembleInstruction(*m_executable, dbg_proc_index, dbg_instruction_pointer);
#endif
#endif
		OpCode instruction = ReadByte();

		switch (instruction)
		{
		case OpCode::LOAD_STRING:
		{
			size_t index = static_cast<size_t>(ReadByte());
			if (index >= m_string_literal_cache.size() || !m_string_literal_cache[index])
			{
				if (index >= m_string_literal_cache.size())
				{
					m_string_literal_cache.resize(index + 1, nullptr);
				}
				m_string_literal_cache[index] = AllocateTraceable(m_executable->GetStringPool()[index].data());
			}
			MidoriText& cached_text = m_string_literal_cache[index]->GetTraceable<MidoriText>();
			MidoriText text_copy(cached_text);
			MidoriTraceable* new_string = AllocateTraceable(std::move(text_copy));
			Push(new_string);
			break;
		}
		case OpCode::LOAD_STRING_WIDE:
		{
			size_t index = static_cast<size_t>(ReadShort());
			if (index >= m_string_literal_cache.size() || !m_string_literal_cache[index])
			{
				if (index >= m_string_literal_cache.size())
				{
					m_string_literal_cache.resize(index + 1, nullptr);
				}
				m_string_literal_cache[index] = AllocateTraceable(m_executable->GetStringPool()[index].data());
			}
			MidoriText& cached_text = m_string_literal_cache[index]->GetTraceable<MidoriText>();
			MidoriText text_copy(cached_text);
			MidoriTraceable* new_string = AllocateTraceable(std::move(text_copy));
			Push(new_string);
			break;
		}
		case OpCode::INTEGER_CONSTANT:
		{
			Push(ReadIntegerConstant());
			break;
		}
		case OpCode::FLOAT_CONSTANT:
		{
			Push(ReadFloatConstant());
			break;
		}
		case OpCode::BYTE_CONSTANT:
		{
			Push(ReadByteConstant());
			break;
		}
		case OpCode::WORD_CONSTANT:
		{
			Push(ReadWordConstant());
			break;
		}
		case OpCode::OP_UNIT:
		{
			Push(MidoriValue());
			break;
		}
		case OpCode::OP_TRUE:
		{
			Push(true);
			break;
		}
		case OpCode::OP_FALSE:
		{
			Push(false);
			break;
		}
		case OpCode::INT_MINUS_1:
		{
			Push(-1LL);
			break;
		}
		case OpCode::INT_0:
		{
			Push(0LL);
			break;
		}
		case OpCode::INT_1:
		{
			Push(1LL);
			break;
		}
		case OpCode::INT_2:
		{
			Push(2LL);
			break;
		}
		case OpCode::INT_3:
		{
			Push(3LL);
			break;
		}
		case OpCode::INT_4:
		{
			Push(4LL);
			break;
		}
		case OpCode::INT_5:
		{
			Push(5LL);
			break;
		}
		case OpCode::INT_10:
		{
			Push(10LL);
			break;
		}
		case OpCode::CREATE_ARRAY:
		{
			int count = ReadThreeBytes();
			MidoriArray arr(count);

			for (int i = count - 1; i >= 0; i -= 1)
			{
				arr[i] = Pop();
			}

			Push(AllocateTraceable(std::move(arr)));
			break;
		}
		case OpCode::GET_ARRAY:
		{
			int num_indices = static_cast<int>(ReadByte());
			MidoriArray indices(num_indices);

			for (int i = num_indices - 1; i >= 0; i -= 1)
			{
				indices[i] = Pop();
			}

			MidoriValue arr = Pop();
			MidoriArray& arr_ref = arr.GetPointer()->GetTraceable<MidoriArray>();
			MidoriInteger arr_size = static_cast<MidoriInteger>(arr_ref.GetLength());

			for (int i = 0; i < num_indices; i += 1)
			{
				MidoriValue& index = indices[i];
				int return_code = CheckIndexBounds(index, arr_size);
				if (return_code != 0)
				{
					return return_code;
				}

				MidoriValue& next_val = arr_ref[static_cast<int>(index.GetInteger())];

				if (i != num_indices - 1)
				{
					arr_ref = next_val.GetPointer()->GetTraceable<MidoriArray>();
				}
				else
				{
					Push(next_val);
				}
			}

			break;
		}
		case OpCode::SET_ARRAY:
		{
			int num_indices = static_cast<int>(ReadByte());
			MidoriValue value_to_set = Pop();
			MidoriArray indices(num_indices);

			for (int i = num_indices - 1; i >= 0; i -= 1)
			{
				indices[i] = Pop();
			}

			MidoriValue arr = Pop();
			MidoriArray& arr_ref = arr.GetPointer()->GetTraceable<MidoriArray>();
			MidoriInteger arr_size = static_cast<MidoriInteger>(arr_ref.GetLength());

			for (int i = 0; i < num_indices; i += 1)
			{
				MidoriValue& index = indices[i];
				int return_code = CheckIndexBounds(index, arr_size);
				if (return_code != 0)
				{
					return return_code;
				}
				MidoriValue& next_val = arr_ref[static_cast<int>(index.GetInteger())];
				if (i != num_indices - 1)
				{
					arr_ref = next_val.GetPointer()->GetTraceable<MidoriArray>();
				}
				else
				{
					next_val = value_to_set;
				}
			}

			Push(value_to_set);
			break;
		}
		case OpCode::DUP_ARRAY:
		{
			MidoriValue size_val = Pop();
			MidoriValue arr_val = Pop();
			MidoriArray& arr_ref = arr_val.GetPointer()->GetTraceable<MidoriArray>();

			MidoriInteger original_size = arr_ref.GetLength();
			MidoriInteger repeat_count = size_val.GetInteger();
			MidoriInteger new_size = repeat_count * original_size;

			int return_code = CheckNewArraySize(new_size);
			if (return_code != 0)
			{
				return return_code;
			}

			MidoriArray new_arr(static_cast<int>(new_size));

			for (int i = 0; i < static_cast<int>(new_size); i += 1)
			{
				new_arr[i] = arr_ref[i % original_size];
			}

			Push(AllocateTraceable(std::move(new_arr)));
			break;
		}
		case OpCode::ADD_BACK_ARRAY:
		{
			MidoriValue val = Pop();
			MidoriValue& arr = Peek();

			MidoriArray& arr_ref = arr.GetPointer()->GetTraceable<MidoriArray>();
			arr_ref.AddBack(val);

			break;
		}
		case OpCode::ADD_FRONT_ARRAY:
		{
			MidoriValue arr = Pop();
			MidoriValue& val = Peek();

			MidoriArray& arr_ref = arr.GetPointer()->GetTraceable<MidoriArray>();
			arr_ref.AddFront(val);

			val = arr;

			break;
		}
		case OpCode::GET_ARRAY_LENGTH:
		{
			MidoriValue arr = Pop();
			MidoriArray& arr_ref = arr.GetPointer()->GetTraceable<MidoriArray>();
			MidoriInteger length = static_cast<MidoriInteger>(arr_ref.GetLength());
			Push(length);
			break;
		}
		case OpCode::CREATE_INT_RANGE:
		{
			MidoriValue end = Pop();
			MidoriValue step = Pop();
			MidoriValue start = Pop();

			MidoriIntRange range(start.GetInteger(), end.GetInteger(), step.GetInteger());

			Push(AllocateTraceable(std::move(range)));
			break;
		}
		case OpCode::CREATE_FLOAT_RANGE:
		{
			MidoriValue end = Pop();
			MidoriValue step = Pop();
			MidoriValue start = Pop();

			MidoriFloatRange range(start.GetFloat(), end.GetFloat(), step.GetFloat());

			Push(AllocateTraceable(std::move(range)));
			break;
		}
		case OpCode::GET_RANGE_START:
		{
			MidoriValue range_ptr = Pop();
			MidoriTraceable* ptr = range_ptr.GetPointer();
			if (ptr->IsTraceable<MidoriIntRange>())
			{
				Push(ptr->GetTraceable<MidoriIntRange>().GetStart());
			}
			else
			{
				Push(ptr->GetTraceable<MidoriFloatRange>().GetStart());
			}
			break;
		}
		case OpCode::GET_RANGE_END:
		{
			MidoriValue range_ptr = Pop();
			MidoriTraceable* ptr = range_ptr.GetPointer();
			if (ptr->IsTraceable<MidoriIntRange>())
			{
				Push(ptr->GetTraceable<MidoriIntRange>().GetEnd());
			}
			else
			{
				Push(ptr->GetTraceable<MidoriFloatRange>().GetEnd());
			}
			break;
		}
		case OpCode::GET_RANGE_STEP:
		{
			MidoriValue range_ptr = Pop();
			MidoriTraceable* ptr = range_ptr.GetPointer();
			if (ptr->IsTraceable<MidoriIntRange>())
			{
				Push(ptr->GetTraceable<MidoriIntRange>().GetStep());
			}
			else
			{
				Push(ptr->GetTraceable<MidoriFloatRange>().GetStep());
			}
			break;
		}
		case OpCode::INT_TO_FLOAT:
		{
			Peek() = static_cast<MidoriFloat>(Peek().GetInteger());
			break;
		}
		case OpCode::TEXT_TO_FLOAT:
		{
			Peek() = static_cast<MidoriFloat>(Peek().GetPointer()->GetTraceable<MidoriText>().ToFloat());
			break;
		}
		case OpCode::FLOAT_TO_INT:
		{
			Peek() = static_cast<MidoriInteger>(Peek().GetFloat());
			break;
		}
		case OpCode::TEXT_TO_INT:
		{
			Peek() = static_cast<MidoriInteger>(Peek().GetPointer()->GetTraceable<MidoriText>().ToInteger());
			break;
		}
		case OpCode::FLOAT_TO_TEXT:
		{
			Peek() = AllocateTraceable(MidoriText::FromFloat(Peek().GetFloat()));
			break;
		}
		case OpCode::INT_TO_TEXT:
		{
			Peek() = AllocateTraceable(MidoriText::FromInteger(Peek().GetInteger()));
			break;
		}
		case OpCode::BYTE_TO_INT:
		{
			Peek() = static_cast<MidoriInteger>(Peek().GetByte());
			break;
		}
		case OpCode::INT_TO_BYTE:
		{
			Peek() = static_cast<MidoriByte>(Peek().GetInteger() & 0xFF);
			break;
		}
		case OpCode::BYTE_TO_WORD:
		{
			Peek() = static_cast<MidoriWord>(Peek().GetByte());
			break;
		}
		case OpCode::WORD_TO_BYTE:
		{
			Peek() = static_cast<MidoriByte>(Peek().GetWord() & 0xFF);
			break;
		}
		case OpCode::WORD_TO_INT:
		{
			Peek() = static_cast<MidoriInteger>(Peek().GetWord());
			break;
		}
		case OpCode::INT_TO_WORD:
		{
			Peek() = static_cast<MidoriWord>(Peek().GetInteger());
			break;
		}
		case OpCode::BYTE_TO_FLOAT:
		{
			Peek() = static_cast<MidoriFloat>(Peek().GetByte());
			break;
		}
		case OpCode::FLOAT_TO_BYTE:
		{
			Peek() = static_cast<MidoriByte>(Peek().GetFloat());
			break;
		}
		case OpCode::WORD_TO_FLOAT:
		{
			Peek() = static_cast<MidoriFloat>(Peek().GetWord());
			break;
		}
		case OpCode::FLOAT_TO_WORD:
		{
			Peek() = static_cast<MidoriWord>(Peek().GetFloat());
			break;
		}
		case OpCode::LEFT_SHIFT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() << right.GetInteger();
			break;
		}
		case OpCode::RIGHT_SHIFT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() >> right.GetInteger();

			break;
		}
		case OpCode::LEFT_SHIFT_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() << right.GetByte());

			break;
		}
		case OpCode::RIGHT_SHIFT_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() >> right.GetByte());

			break;
		}
		case OpCode::LEFT_SHIFT_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() << right.GetWord();

			break;
		}
		case OpCode::RIGHT_SHIFT_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() >> right.GetWord();

			break;
		}
		case OpCode::BITWISE_AND:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() & right.GetInteger();

			break;
		}
		case OpCode::BITWISE_OR:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() | right.GetInteger();

			break;
		}
		case OpCode::BITWISE_XOR:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() ^ right.GetInteger();

			break;
		}
		case OpCode::BITWISE_NOT:
		{
			MidoriValue& right = Peek();

			right = ~right.GetInteger();

			break;
		}
		case OpCode::ADD_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() + right.GetFloat();

			break;
		}
		case OpCode::SUBTRACT_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() - right.GetFloat();

			break;
		}
		case OpCode::MULTIPLY_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() * right.GetFloat();

			break;
		}
		case OpCode::DIVIDE_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() / right.GetFloat();

			break;
		}
		case OpCode::MODULO_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = std::fmod(left.GetFloat(), right.GetFloat());

			break;
		}
		case OpCode::ADD_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() + right.GetInteger();

			break;
		}
		case OpCode::SUBTRACT_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() - right.GetInteger();

			break;
		}
		case OpCode::MULTIPLY_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() * right.GetInteger();

			break;
		}
		case OpCode::DIVIDE_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() / right.GetInteger();

			break;
		}
		case OpCode::MODULO_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() % right.GetInteger();

			break;
		}
		case OpCode::ADD_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() + right.GetByte());

			break;
		}
		case OpCode::SUBTRACT_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() - right.GetByte());

			break;
		}
		case OpCode::MULTIPLY_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() * right.GetByte());

			break;
		}
		case OpCode::DIVIDE_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() / right.GetByte());

			break;
		}
		case OpCode::MODULO_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = static_cast<MidoriByte>(left.GetByte() % right.GetByte());

			break;
		}
		case OpCode::ADD_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() + right.GetWord();

			break;
		}
		case OpCode::SUBTRACT_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() - right.GetWord();

			break;
		}
		case OpCode::MULTIPLY_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() * right.GetWord();

			break;
		}
		case OpCode::DIVIDE_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() / right.GetWord();

			break;
		}
		case OpCode::MODULO_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() % right.GetWord();

			break;
		}
		case OpCode::CONCAT_ARRAY:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			MidoriArray& left_value_vector_ref = left.GetPointer()->GetTraceable<MidoriArray>();
			MidoriArray& right_value_vector_ref = right.GetPointer()->GetTraceable<MidoriArray>();
			MidoriArray result = MidoriArray::Concatenate(left_value_vector_ref, right_value_vector_ref);

			left = AllocateTraceable(std::move(result));
			TryCollect();
			break;
		}
		case OpCode::CONCAT_TEXT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			MidoriText& left_value_string_ref = left.GetPointer()->GetTraceable<MidoriText>();
			MidoriText& right_value_string_ref = right.GetPointer()->GetTraceable<MidoriText>();

			MidoriText result = MidoriText::Concatenate(left_value_string_ref, right_value_string_ref);

			left = AllocateTraceable(std::move(result));
			TryCollect();
			break;
		}
		case OpCode::APPEND_ARRAY:
		{
			MidoriValue value = Pop();
			MidoriValue& array = Peek();

			MidoriArray& array_ref = array.GetPointer()->GetTraceable<MidoriArray>();
			array_ref.AddBack(value);
			TryCollect();
			break;
		}
		case OpCode::EXTEND_ARRAY:
		{
			MidoriValue value = Pop();
			MidoriValue& array = Peek();

			MidoriArray& array_ref = array.GetPointer()->GetTraceable<MidoriArray>();
			MidoriArray& other_ref = value.GetPointer()->GetTraceable<MidoriArray>();
			array_ref.Extend(other_ref);
			TryCollect();
			break;
		}
		case OpCode::PREPEND_ARRAY:
		{
			MidoriValue value = Pop();
			MidoriValue& array = Peek();

			MidoriArray& array_ref = array.GetPointer()->GetTraceable<MidoriArray>();
			array_ref.AddFront(value);
			TryCollect();
			break;
		}
		case OpCode::APPEND_TEXT:
		{
			MidoriValue value = Pop();
			MidoriValue& text = Peek();

			MidoriText& text_ref = text.GetPointer()->GetTraceable<MidoriText>();
			MidoriText& value_text = value.GetPointer()->GetTraceable<MidoriText>();
			text_ref.Append(value_text);
			TryCollect();
			break;
		}
		case OpCode::PREPEND_TEXT:
		{
			MidoriValue value = Pop();
			MidoriValue& text = Peek();

			MidoriText& text_ref = text.GetPointer()->GetTraceable<MidoriText>();
			MidoriText& value_text = value.GetPointer()->GetTraceable<MidoriText>();
			text_ref.Prepend(value_text);
			TryCollect();
			break;
		}
		case OpCode::ADD_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() + value.GetInteger();
			break;
		}
		case OpCode::ADD_ASSIGN_FLOAT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetFloat() + value.GetFloat();
			break;
		}
		case OpCode::SUB_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() - value.GetInteger();
			break;
		}
		case OpCode::SUB_ASSIGN_FLOAT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetFloat() - value.GetFloat();
			break;
		}
		case OpCode::MUL_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() * value.GetInteger();
			break;
		}
		case OpCode::MUL_ASSIGN_FLOAT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetFloat() * value.GetFloat();
			break;
		}
		case OpCode::DIV_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() / value.GetInteger();
			break;
		}
		case OpCode::DIV_ASSIGN_FLOAT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetFloat() / value.GetFloat();
			break;
		}
		case OpCode::MOD_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() % value.GetInteger();
			break;
		}
		case OpCode::MOD_ASSIGN_FLOAT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = std::fmod(var.GetFloat(), value.GetFloat());
			break;
		}
		case OpCode::AND_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() & value.GetInteger();
			break;
		}
		case OpCode::OR_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() | value.GetInteger();
			break;
		}
		case OpCode::XOR_ASSIGN_INT:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() ^ value.GetInteger();
			break;
		}
		case OpCode::LEFT_SHIFT_ASSIGN:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() << value.GetInteger();
			break;
		}
		case OpCode::RIGHT_SHIFT_ASSIGN:
		{
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			var = var.GetInteger() >> value.GetInteger();
			break;
		}
		case OpCode::EQUAL_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() == right.GetFloat();

			break;
		}
		case OpCode::NOT_EQUAL_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() != right.GetFloat();

			break;
		}
		case OpCode::GREATER_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() > right.GetFloat();

			break;
		}
		case OpCode::GREATER_EQUAL_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() >= right.GetFloat();

			break;
		}
		case OpCode::LESS_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() < right.GetFloat();

			break;
		}
		case OpCode::LESS_EQUAL_FLOAT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetFloat() <= right.GetFloat();

			break;
		}
		case OpCode::EQUAL_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() == right.GetInteger();

			break;
		}
		case OpCode::NOT_EQUAL_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() != right.GetInteger();

			break;
		}
		case OpCode::GREATER_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() > right.GetInteger();

			break;
		}
		case OpCode::GREATER_EQUAL_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() >= right.GetInteger();

			break;
		}
		case OpCode::LESS_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() < right.GetInteger();

			break;
		}
		case OpCode::LESS_EQUAL_INTEGER:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetInteger() <= right.GetInteger();

			break;
		}
		case OpCode::EQUAL_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetByte() == right.GetByte();

			break;
		}
		case OpCode::NOT_EQUAL_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetByte() != right.GetByte();

			break;
		}
		case OpCode::GREATER_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetByte() > right.GetByte();

			break;
		}
		case OpCode::GREATER_EQUAL_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetByte() >= right.GetByte();

			break;
		}
		case OpCode::LESS_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetByte() < right.GetByte();

			break;
		}
		case OpCode::LESS_EQUAL_BYTE:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetByte() <= right.GetByte();

			break;
		}
		case OpCode::EQUAL_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() == right.GetWord();

			break;
		}
		case OpCode::NOT_EQUAL_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() != right.GetWord();

			break;
		}
		case OpCode::GREATER_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() > right.GetWord();

			break;
		}
		case OpCode::GREATER_EQUAL_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() >= right.GetWord();

			break;
		}
		case OpCode::LESS_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() < right.GetWord();

			break;
		}
		case OpCode::LESS_EQUAL_WORD:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetWord() <= right.GetWord();

			break;
		}
		case OpCode::EQUAL_TEXT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			left = left.GetPointer()->GetTraceable<MidoriText>() == right.GetPointer()->GetTraceable<MidoriText>();

			break;
		}
		case OpCode::NOT:
		{
			MidoriValue& value = Peek();
			value = !value.GetBool();
			break;
		}
		case OpCode::NEGATE_FLOAT:
		{
			MidoriValue& value = Peek();
			value = -value.GetFloat();
			break;
		}
		case OpCode::NEGATE_INTEGER:
		{
			MidoriValue& value = Peek();
			value = -value.GetInteger();
			break;
		}
		case OpCode::JUMP_IF_FALSE:
		{
			MidoriValue value = Peek();

			int offset = ReadShort();
			if (!value.GetBool())
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::JUMP_IF_TRUE:
		{
			MidoriValue value = Peek();

			int offset = ReadShort();
			if (value.GetBool())
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::JUMP:
		{
			int offset = ReadShort();
			m_instruction_pointer += offset;
			break;
		}
		case OpCode::JUMP_BACK:
		{
			int offset = ReadShort();
			m_instruction_pointer -= offset;
			break;
		}
		case OpCode::IF_INTEGER_LESS:
		{
			int offset = ReadShort();
			MidoriInteger right = Pop().GetInteger();
			MidoriInteger left = Pop().GetInteger();

			if (!(left < right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_INTEGER_LESS_EQUAL:
		{
			int offset = ReadShort();
			MidoriInteger right = Pop().GetInteger();
			MidoriInteger left = Pop().GetInteger();

			if (!(left <= right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_INTEGER_GREATER:
		{
			int offset = ReadShort();
			MidoriInteger right = Pop().GetInteger();
			MidoriInteger left = Pop().GetInteger();

			if (!(left > right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_INTEGER_GREATER_EQUAL:
		{
			int offset = ReadShort();
			MidoriInteger right = Pop().GetInteger();
			MidoriInteger left = Pop().GetInteger();

			if (!(left >= right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_INTEGER_EQUAL:
		{
			int offset = ReadShort();
			MidoriInteger right = Pop().GetInteger();
			MidoriInteger left = Pop().GetInteger();

			if (!(left == right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_INTEGER_NOT_EQUAL:
		{
			int offset = ReadShort();
			MidoriInteger right = Pop().GetInteger();
			MidoriInteger left = Pop().GetInteger();

			if (!(left != right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_FLOAT_LESS:
		{
			int offset = ReadShort();
			MidoriFloat right = Pop().GetFloat();
			MidoriFloat left = Pop().GetFloat();

			if (!(left < right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_FLOAT_LESS_EQUAL:
		{
			int offset = ReadShort();
			MidoriFloat right = Pop().GetFloat();
			MidoriFloat left = Pop().GetFloat();

			if (!(left <= right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_FLOAT_GREATER:
		{
			int offset = ReadShort();
			MidoriFloat right = Pop().GetFloat();
			MidoriFloat left = Pop().GetFloat();

			if (!(left > right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_FLOAT_GREATER_EQUAL:
		{
			int offset = ReadShort();
			MidoriFloat right = Pop().GetFloat();
			MidoriFloat left = Pop().GetFloat();

			if (!(left >= right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_FLOAT_EQUAL:
		{
			int offset = ReadShort();
			MidoriFloat right = Pop().GetFloat();
			MidoriFloat left = Pop().GetFloat();

			if (!(left == right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::IF_FLOAT_NOT_EQUAL:
		{
			int offset = ReadShort();
			MidoriFloat right = Pop().GetFloat();
			MidoriFloat left = Pop().GetFloat();

			if (!(left != right))
			{
				m_instruction_pointer += offset;
			}
			break;
		}
		case OpCode::BREAK:
		{
			MidoriValue value = Pop();
			int offset = ReadShort();
			m_instruction_pointer += offset;
			Push(value);
			break;
		}
		case OpCode::LOAD_TAG:
		{
			MidoriValue union_val = Pop();
			MidoriUnion& union_ref = union_val.GetPointer()->GetTraceable<MidoriUnion>();

			for (int i = 0; i < union_ref.m_values.GetLength(); i += 1)
			{
				Push(union_ref.m_values[i]);
			}

			Push(static_cast<MidoriInteger>(union_ref.m_index));
			break;
		}
		case OpCode::GET_TAG:
		{
			MidoriValue union_val = Pop();
			MidoriUnion& union_ref = union_val.GetPointer()->GetTraceable<MidoriUnion>();
			Push(static_cast<MidoriInteger>(union_ref.m_index));
			break;
		}
		case OpCode::SET_TAG:
		{
			int tag = static_cast<int>(ReadByte());
			MidoriUnion& union_ref = Peek().GetPointer()->GetTraceable<MidoriUnion>();
			union_ref.m_index = tag;
			break;
		}
		case OpCode::MATCH_JUMP_TABLE:
		{
			MidoriInteger tag = Peek().GetInteger();
			int case_count = static_cast<int>(ReadByte());

			// Read jump table offsets and jump to the matching case
			if (tag >= 0 && tag < case_count)
			{
				int tag_int = static_cast<int>(tag);

				// Skip to the offset for this tag
				for (int i = 0; i < tag_int; i += 1)
				{
					ReadShort(); // Skip offsets for previous cases
				}

				// Read the offset for our case
				int offset = ReadShort();

				// Skip remaining offsets
				for (int i = tag_int + 1; i < case_count; i += 1)
				{
					ReadShort();
				}

				// Jump to the case body
				m_instruction_pointer += offset;
			}
			else
			{
				// Invalid tag: skip all offsets
				for (int i = 0; i < case_count; i += 1)
				{
					ReadShort();
				}
			}
			break;
		}
		case OpCode::CALL_FOREIGN:
		{
			MidoriValue foreign_function_name = Pop();
			int arity = static_cast<int>(ReadByte());
			uint8_t return_type = static_cast<uint8_t>(ReadByte());

#if MIDORI_DEBUG_FULL
			if (!foreign_function_name.IsPointer())
			{
				return TerminateExecution(GenerateRuntimeError(std::format("Type error: expected function name (Text), but got {}.", foreign_function_name.ToText().GetCString()), GetLine()));
			}
#endif

			MidoriText& foreign_function_name_ref = foreign_function_name.GetPointer()->GetTraceable<MidoriText>();

			FFIFunction proc = nullptr;
			std::optional<size_t> ffi_idx = MidoriFFIRegistry::FindIndex(foreign_function_name_ref.GetCString());
			if (ffi_idx.has_value())
			{
				proc = m_ffi_table[ffi_idx.value()];
			}
			else
			{
				DynamicFFIRegistry& dynamic_registry = DynamicFFIRegistry::GetInstance();
				std::optional<FFIFunction> dynamic_func = dynamic_registry.FindFunction(foreign_function_name_ref.GetCString());
				if (dynamic_func.has_value())
				{
					proc = dynamic_func.value();
				}
			}

			if (proc == nullptr)
			{
				return TerminateExecution(GenerateRuntimeError(std::format("Failed to load foreign function '{}'.", foreign_function_name_ref.GetCString()), GetLine()));
			}

			m_ffi_array_args.clear();
			if (m_ffi_array_args.capacity() < static_cast<size_t>(arity))
			{
				m_ffi_array_args.reserve(static_cast<size_t>(arity));
			}
			for (int i = arity - 1; i >= 0; i -= 1)
			{
				size_t idx = static_cast<size_t>(i);
				MidoriValue arg = Pop();

				if (m_gc.Contains(arg.GetPointer()))
				{
					MidoriTraceable* ptr = arg.GetPointer();
					if (ptr->IsTraceable<MidoriText>())
					{
						m_ffi_args[static_cast<size_t>(idx)] = (void*)ptr->GetTraceable<MidoriText>().GetCString();
					}
					else if (ptr->IsTraceable<MidoriArray>())
					{
						MidoriArray& array = ptr->GetTraceable<MidoriArray>();
						FFIArrayArgument array_arg;
						array_arg.data = &array[0u];
						array_arg.length = array.GetLength();
						m_ffi_array_args.push_back(array_arg);
						m_ffi_args[static_cast<size_t>(idx)] = &m_ffi_array_args.back();
					}
					else
					{
						m_ffi_args[static_cast<size_t>(idx)] = nullptr;
					}
				}
				else
				{
					std::memcpy(&m_ffi_args[idx], arg.GetRawDataPtr(), sizeof(double));
				}
			}

			MidoriValue return_val;
			proc(m_ffi_args.data(), reinterpret_cast<void*>(&return_val));

			if (return_type == 1)
			{
				int64_t ptr_val = return_val.GetInteger();
				if (ptr_val == 0)
				{
					Push(AllocateTraceable(""));
				}
				else
				{
					char* ffi_string = reinterpret_cast<char*>(ptr_val);
					Push(AllocateTraceable(ffi_string));
					std::free(ffi_string);
				}
			}
			else if (return_type == 2)
			{
				struct FFIArray
				{
					void* data;
					int length;
				};

				int64_t ptr_val = return_val.GetInteger();
				if (ptr_val == 0)
				{
					Push(AllocateTraceable(MidoriArray()));
				}
				else
				{
					FFIArray* ffi_array = reinterpret_cast<FFIArray*>(ptr_val);
					MidoriValue* ffi_array_data = static_cast<MidoriValue*>(ffi_array->data);
					int length = ffi_array->length;

					MidoriArray wrapped_array = MidoriArray::FromFFI(ffi_array_data, length);
					Push(AllocateTraceable(std::move(wrapped_array)));

					std::free(ffi_array);
				}
			}
			else
			{
				Push(return_val);
			}

			break;
		}
		case OpCode::CALL_FOREIGN_INDEXED:
		{
			uint8_t ffi_index = static_cast<uint8_t>(ReadByte());
			int arity = static_cast<int>(ReadByte());
			uint8_t return_type = static_cast<uint8_t>(ReadByte());

			FFIFunction proc = m_ffi_table[ffi_index];

			m_ffi_array_args.clear();
			if (m_ffi_array_args.capacity() < static_cast<size_t>(arity))
			{
				m_ffi_array_args.reserve(static_cast<size_t>(arity));
			}
			for (int i = arity - 1; i >= 0; i -= 1)
			{
				size_t idx = static_cast<size_t>(i);
				MidoriValue arg = Pop();

				if (m_gc.Contains(arg.GetPointer()))
				{
					MidoriTraceable* ptr = arg.GetPointer();
					if (ptr->IsTraceable<MidoriText>())
					{
						m_ffi_args[idx] = (void*)ptr->GetTraceable<MidoriText>().GetCString();
					}
					else if (ptr->IsTraceable<MidoriArray>())
					{
						MidoriArray& array = ptr->GetTraceable<MidoriArray>();
						FFIArrayArgument array_arg;
						array_arg.data = &array[0u];
						array_arg.length = array.GetLength();
						m_ffi_array_args.push_back(array_arg);
						m_ffi_args[idx] = &m_ffi_array_args.back();
					}
					else
					{
						m_ffi_args[idx] = nullptr;
					}
				}
				else
				{
					std::memcpy(&m_ffi_args[idx], arg.GetRawDataPtr(), sizeof(double));
				}
			}

			MidoriValue return_val;
			proc(m_ffi_args.data(), reinterpret_cast<void*>(&return_val));

			if (return_type == 1)
			{
				int64_t ptr_val = return_val.GetInteger();
				if (ptr_val == 0)
				{
					Push(AllocateTraceable(""));
				}
				else
				{
					char* ffi_string = reinterpret_cast<char*>(ptr_val);
					Push(AllocateTraceable(ffi_string));
					std::free(ffi_string);
				}
			}
			else if (return_type == 2)
			{
				struct FFIArray
				{
					void* data;
					int length;
				};

				int64_t ptr_val = return_val.GetInteger();
				if (ptr_val == 0)
				{
					Push(AllocateTraceable(MidoriArray()));
				}
				else
				{
					FFIArray* ffi_array = reinterpret_cast<FFIArray*>(ptr_val);
					MidoriValue* ffi_array_data = static_cast<MidoriValue*>(ffi_array->data);
					int length = ffi_array->length;

					MidoriArray wrapped_array = MidoriArray::FromFFI(ffi_array_data, length);
					Push(AllocateTraceable(std::move(wrapped_array)));

					std::free(ffi_array);
				}
			}
			else
			{
				Push(return_val);
			}

			break;
		}
		case OpCode::CALL:
		{
			MidoriValue callable = Pop();
			int arity = static_cast<int>(ReadByte());

#if MIDORI_DEBUG_FULL
			if (!callable.IsPointer())
			{
				return TerminateExecution(GenerateRuntimeError(std::format("Type error: expected callable (function/closure), but got {}.", callable.ToText().GetCString()), GetLine()));
			}
#endif

			// Save caller's frame before switching to callee
			PushCallFrame(m_value_stack_base_pointer, m_instruction_pointer, m_curr_environment);

			MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();
			m_curr_environment = &closure.m_cell_values;

			m_instruction_pointer = m_executable->GetBytecodeStream(closure.m_proc_index)[0u];
			m_value_stack_base_pointer = m_value_stack_pointer - arity;

			break;
		}
		case OpCode::CALL_0:
		case OpCode::CALL_1:
		case OpCode::CALL_2:
		case OpCode::CALL_3:
		{
			MidoriValue callable = Pop();
			int arity = static_cast<int>(instruction) - static_cast<int>(OpCode::CALL_0);

#if MIDORI_DEBUG_FULL
			if (!callable.IsPointer())
			{
				return TerminateExecution(GenerateRuntimeError(std::format("Type error: expected callable (function/closure), but got {}.", callable.ToText().GetCString()), GetLine()));
			}
#endif

			// Save caller's frame before switching to callee
			PushCallFrame(m_value_stack_base_pointer, m_instruction_pointer, m_curr_environment);

			MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();
			m_curr_environment = &closure.m_cell_values;

			m_instruction_pointer = m_executable->GetBytecodeStream(closure.m_proc_index)[0u];
			m_value_stack_base_pointer = m_value_stack_pointer - arity;

			break;
		}
		case OpCode::CALL_PROC:
		{
			int proc_index = static_cast<int>(ReadByte());
			int arity = static_cast<int>(ReadByte());

			PushCallFrame(m_value_stack_base_pointer, m_instruction_pointer, m_curr_environment);

			// Static functions have no captures, so no environment needed
			m_curr_environment = nullptr;
			m_instruction_pointer = m_executable->GetBytecodeStream(proc_index)[0u];
			m_value_stack_base_pointer = m_value_stack_pointer - arity;

			break;
		}
		case OpCode::CALL_PROC_0:
		case OpCode::CALL_PROC_1:
		case OpCode::CALL_PROC_2:
		case OpCode::CALL_PROC_3:
		{
			int proc_index = static_cast<int>(ReadByte());
			int arity = static_cast<int>(instruction) - static_cast<int>(OpCode::CALL_PROC_0);

			PushCallFrame(m_value_stack_base_pointer, m_instruction_pointer, m_curr_environment);

			// Static functions have no captures, so no environment needed
			m_curr_environment = nullptr;
			m_instruction_pointer = m_executable->GetBytecodeStream(proc_index)[0u];
			m_value_stack_base_pointer = m_value_stack_pointer - arity;

			break;
		}
		case OpCode::TAIL_CALL:
		{
			MidoriValue callable = Pop();
			int arity = static_cast<int>(ReadByte());

#if MIDORI_DEBUG_FULL
			if (!callable.IsPointer())
			{
				return TerminateExecution(GenerateRuntimeError(std::format("Type error: expected callable (function/closure), but got {}.", callable.ToText().GetCString()), GetLine()));
			}
#endif

			// Move arguments down to base pointer
			MidoriValue* args_source = m_value_stack_pointer - arity;
			std::memmove(m_value_stack_base_pointer, args_source, arity * sizeof(MidoriValue));
			m_value_stack_pointer = m_value_stack_base_pointer + arity;

			MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();
			m_curr_environment = &closure.m_cell_values;

			// Jump to the start of the function without creating a new call frame
			m_instruction_pointer = m_executable->GetBytecodeStream(closure.m_proc_index)[0u];

			break;
		}
		case OpCode::CONSTRUCT_STRUCT:
		{
			MidoriTraceable* new_struct = AllocateTraceable(MidoriStruct());
			int size = static_cast<int>(ReadByte());
			MidoriTuple args(size);

			for (int i = size - 1; i >= 0; i -= 1)
			{
				args[i] = Pop();
			}

			MidoriTuple& members = new_struct->GetTraceable<MidoriStruct>().m_values;
			members = std::move(args);

			Push(new_struct);
			break;
		}
		case OpCode::CONSTRUCT_UNION:
		{
			MidoriTraceable* new_union = AllocateTraceable(MidoriUnion());

			int size = static_cast<int>(ReadByte());
			MidoriTuple args(size);

			for (int i = size - 1; i >= 0; i -= 1)
			{
				args[i] = Pop();
			}

			MidoriTuple& members = new_union->GetTraceable<MidoriUnion>().m_values;
			members = std::move(args);

			Push(new_union);
			break;
		}
		case OpCode::MAKE_CLOSURE:
		{
			int proc_index = static_cast<int>(ReadByte());
			Push(AllocateTraceable(MidoriClosure{ .m_cell_values = MidoriTuple(), .m_proc_index = proc_index }));
			break;
		}
		case OpCode::MAKE_FUNCTION:
		{
			int proc_index = static_cast<int>(ReadByte());

			std::unordered_map<int, MidoriTraceable*>::iterator it = m_static_closure_cache.find(proc_index);
			if (m_static_closure_cache.contains(proc_index))
			{
				Push(it->second);
			}
			else
			{
				MidoriTraceable* closure = AllocateTraceable(MidoriClosure{.m_cell_values = MidoriTuple(), .m_proc_index = proc_index});
				m_static_closure_cache[proc_index] = closure;
				Push(closure);
			}

			break;
		}
		case OpCode::BIND_CAPTURES:
		{
			int total_count = static_cast<int>(ReadByte());

			MidoriTuple& closure_env = (m_value_stack_pointer - 1)->GetPointer()->GetTraceable<MidoriClosure>().m_cell_values;
			
			int parent_count = m_curr_environment ? m_curr_environment->GetLength() : 0;
			int local_capture_count = total_count - parent_count;
			
			MidoriTuple new_env(total_count);

			// Copy parent environment
			if (m_curr_environment)
			{
				for (int i = 0; i < parent_count; i += 1)
				{
					new_env[i] = (*m_curr_environment)[i];
				}
			}

			// Capture local variables
			for (int i = 0; i < local_capture_count; i += 1)
			{
				MidoriValue& value = *(m_value_stack_base_pointer + i);
				MidoriValue* stack_value_ref = &value;
				MidoriValue cell_value = AllocateTraceable(MidoriCellValue(stack_value_ref));
				
				new_env[parent_count + i] = cell_value;
				m_cells_to_promote.emplace_back(&cell_value.GetPointer()->GetTraceable<MidoriCellValue>());
			}

			closure_env = std::move(new_env);
			break;
		}
		case OpCode::DEFINE_GLOBAL:
		{
			MidoriValue value = Pop();
			int global_idx = ReadGlobalVariable();
			MidoriValue& var = (*m_global_vars)[global_idx];
			var = value;
			break;
		}
		case OpCode::GET_GLOBAL:
		{
			int global_idx = ReadGlobalVariable();
			Push((*m_global_vars)[global_idx]);
			break;
		}
		case OpCode::SET_GLOBAL:
		{
			int global_idx = ReadGlobalVariable();
			MidoriValue& var = (*m_global_vars)[global_idx];
			var = Peek();
			break;
		}
		case OpCode::GET_LOCAL:
		{
			int offset = static_cast<int>(ReadByte());
			Push(*(m_value_stack_base_pointer + offset));
			break;
		}
		case OpCode::SET_LOCAL:
		{
			int offset = static_cast<int>(ReadByte());
			MidoriValue& var = *(m_value_stack_base_pointer + offset);

			MidoriValue& value = Peek();
			var = value;
			break;
		}
		case OpCode::GET_LOCAL_0:
		case OpCode::GET_LOCAL_1:
		case OpCode::GET_LOCAL_2:
		case OpCode::GET_LOCAL_3:
		{
			int offset = static_cast<int>(instruction) - static_cast<int>(OpCode::GET_LOCAL_0);
			Push(*(m_value_stack_base_pointer + offset));
			break;
		}
		case OpCode::SET_LOCAL_0:
		case OpCode::SET_LOCAL_1:
		case OpCode::SET_LOCAL_2:
		case OpCode::SET_LOCAL_3:
		{
			int offset = static_cast<int>(instruction) - static_cast<int>(OpCode::SET_LOCAL_0);
			MidoriValue& var = *(m_value_stack_base_pointer + offset);

			MidoriValue& value = Peek();
			var = value;
			break;
		}
		case OpCode::GET_CELL:
		{
			int offset = static_cast<int>(ReadByte());
#if MIDORI_DEBUG_FULL
			if (!m_curr_environment)
			{
				return TerminateExecution(GenerateRuntimeError("GET_CELL called with null environment - function has captures but was called via CALL_PROC", GetLine()));
			}
#endif
			MidoriValue cell_value = (*m_curr_environment)[offset].GetPointer()->GetTraceable<MidoriCellValue>().GetValue();
			Push(cell_value);
			break;
		}
		case OpCode::SET_CELL:
		{
			int offset = static_cast<int>(ReadByte());
			MidoriValue& cell_value = (*m_curr_environment)[offset].GetPointer()->GetTraceable<MidoriCellValue>().GetValue();
			cell_value = Peek();
			break;
		}
		case OpCode::DEFINE_GLOBAL_WIDE:
		{
			MidoriValue value = Pop();
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int global_idx = (high_byte << 8) | low_byte;
			MidoriValue& var = (*m_global_vars)[global_idx];
			var = value;
			break;
		}
		case OpCode::GET_GLOBAL_WIDE:
		{
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int global_idx = (high_byte << 8) | low_byte;
			Push((*m_global_vars)[global_idx]);
			break;
		}
		case OpCode::SET_GLOBAL_WIDE:
		{
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int global_idx = (high_byte << 8) | low_byte;
			MidoriValue& var = (*m_global_vars)[global_idx];
			var = Peek();
			break;
		}
		case OpCode::GET_LOCAL_WIDE:
		{
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int offset = (high_byte << 8) | low_byte;
			Push(*(m_value_stack_base_pointer + offset));
			break;
		}
		case OpCode::SET_LOCAL_WIDE:
		{
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int offset = (high_byte << 8) | low_byte;
			MidoriValue& var = *(m_value_stack_base_pointer + offset);
			MidoriValue& value = Peek();
			var = value;
			break;
		}
		case OpCode::GET_CELL_WIDE:
		{
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int offset = (high_byte << 8) | low_byte;
			MidoriValue cell_value = (*m_curr_environment)[offset].GetPointer()->GetTraceable<MidoriCellValue>().GetValue();
			Push(cell_value);
			break;
		}
		case OpCode::SET_CELL_WIDE:
		{
			int high_byte = static_cast<int>(ReadByte());
			int low_byte = static_cast<int>(ReadByte());
			int offset = (high_byte << 8) | low_byte;
			MidoriValue& cell_value = (*m_curr_environment)[offset].GetPointer()->GetTraceable<MidoriCellValue>().GetValue();
			cell_value = Peek();
			break;
		}
		case OpCode::GET_MEMBER:
		{
			int index = static_cast<int>(ReadByte());
			MidoriValue value = Pop();
			Push(value.GetPointer()->GetTraceable<MidoriStruct>().m_values[index]);
			break;
		}
		case OpCode::SET_MEMBER:
		{
			int index = static_cast<int>(ReadByte());
			MidoriValue value = Pop();
			MidoriValue& var = Peek();
			MidoriValue& member = var.GetPointer()->GetTraceable<MidoriStruct>().m_values[index];
			member = value;
			break;
		}
		case OpCode::POP:
		{
			--m_value_stack_pointer;
			break;
		}
		case OpCode::DUP:
		{
			Push(Peek());
			break;
		}
		case OpCode::SWAP:
		{
			MidoriValue first = Pop();
			MidoriValue second = Pop();
			Push(first);
			Push(second);
			break;
		}
		case OpCode::POP_LOCAL_SCOPE:
		{
			// on scope exit, promote all cells to heap
			PromoteCells();

			m_value_stack_pointer -= static_cast<int>(ReadByte());
			break;
		}
		case OpCode::POP_VALUES:
		{
			m_value_stack_pointer -= static_cast<int>(ReadByte());
			break;
		}
		case OpCode::POP_BLOCK_SCOPE:
		{
			// on scope exit, promote all cells to heap
			PromoteCells();

			MidoriValue final_value = Pop();
			m_value_stack_pointer -= static_cast<int>(ReadByte());
			Push(final_value);
			break;
		}
		case OpCode::POP_MATCH_SCOPE:
		{
			MidoriValue final_value = Pop();
			m_value_stack_pointer -= static_cast<int>(ReadByte());
			Push(final_value);
			break;
		}
		case OpCode::RETURN:
		{
			// on return, promote all cells to heap
			PromoteCells();

			MidoriValue value = Pop();
			--m_call_stack_pointer;
			const CallFrame& frame = *m_call_stack_pointer;

			// Callee's bp points to where args started, which is our return point
			ValueStackPointer return_point = m_value_stack_base_pointer;

			m_value_stack_base_pointer = frame.return_bp;
			m_value_stack_pointer = return_point;
			m_instruction_pointer = frame.return_ip;
			m_curr_environment = frame.closure_ptr;

			Push(value);

			break;
		}
		case OpCode::HALT:
		{
			return 0;
		}
		case OpCode::PUSH_PLACEHOLDER:
		{
			Push(MidoriValue());
			break;
		}
		case OpCode::UPDATE_PLACEHOLDER:
		{
			// assign the second slot (block final value) to the first slot (block value placeholder)
			Peek() = Pop();
			break;
		}
		case OpCode::SPAWN_ASYNC:
		{
			// Ensure captured locals live on the heap before worker threads read them.
			PromoteCells();

			MidoriValue callable = Pop();
			MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();

			MidoriFuture future_val(MidoriClosure{.m_cell_values = closure.m_cell_values, .m_proc_index = closure.m_proc_index});
			MidoriTraceable* future_ptr = AllocateTraceable(std::move(future_val));
			MidoriFuture* future = &future_ptr->GetTraceable<MidoriFuture>();

			if (m_runtime)
			{
				m_runtime->SpawnTask(future, closure);
			}
			else
			{
				return TerminateExecution(GenerateRuntimeError("Async tasks require MidoriRuntime.", GetLine()));
			}

			Push(future_ptr);
			break;
		}

		case OpCode::AWAIT_FUTURE:
		{
			MidoriValue future_val = Pop();
			MidoriFuture& future = future_val.GetPointer()->GetTraceable<MidoriFuture>();

			MidoriValue result = future.Get();

			if (future.m_has_error.load(std::memory_order_acquire))
			{
				return TerminateExecution(GenerateRuntimeError("Async task error", GetLine()));
			}

			Push(result);
			break;
		}
		case OpCode::ASYNC_RETURN:
		{
			m_async_result = Pop();
			return EXIT_SUCCESS;
		}
		default:
		{
			MIDORI_UNREACHABLE();
		}
		}
	}
}

#ifdef _WIN32
struct ExceptionInfo
{
	ULONG_PTR exception_address;
	ULONG_PTR fault_address;
	bool captured;
};

static int CaptureExceptionFilter(EXCEPTION_POINTERS* ex_info, ExceptionInfo* out_info)
{
	if (ex_info->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION)
	{
		out_info->exception_address = (ULONG_PTR)ex_info->ExceptionRecord->ExceptionAddress;
		out_info->fault_address = ex_info->ExceptionRecord->ExceptionInformation[1];
		out_info->captured = true;
		return EXCEPTION_EXECUTE_HANDLER;
	}
	return EXCEPTION_CONTINUE_SEARCH;
}
#endif

int VirtualMachine::Execute() noexcept
{
	// Initialize FFI table with statically linked functions
	const std::array<FFIEntry, MidoriFFIRegistry::BUILTIN_COUNT>& registry = MidoriFFIRegistry::GetTable();
	for (size_t i = 0u; i < MidoriFFIRegistry::BUILTIN_COUNT; i += 1u)
	{
		m_ffi_table[i] = registry[i].m_function;
	}

#ifdef _WIN32
	// Structured exception handling for guard page access violations (stack overflow)
	ExceptionInfo ex_info = { 0, 0, false };

	__try
	{
		return ExecuteLoop();
	}
	__except (CaptureExceptionFilter(GetExceptionInformation(), &ex_info))
	{
		// Determine if this is a stack overflow or other memory corruption
		bool is_stack_overflow = false;
		if (ex_info.captured && m_value_stack_region != nullptr)
		{
			ULONG_PTR stack_start = (ULONG_PTR)m_value_stack_begin;
			ULONG_PTR stack_end = stack_start + (s_value_stack_size * sizeof(MidoriValue));

			// Check if fault address is within or just beyond the stack region
			if (ex_info.fault_address >= stack_start && ex_info.fault_address <= stack_end + 4096)
			{
				is_stack_overflow = true;
			}
		}

		// Print error header
		Printer::Print<Printer::Color::BRIGHT_RED>("Runtime Error");
		Printer::Print(" at ");
		Printer::Print<Printer::Color::BRIGHT_CYAN>("line ");
		Printer::PrintFormatted("{}\n", GetLine());

		// Print specific error message
		if (is_stack_overflow)
		{
			Printer::Print<Printer::Color::BRIGHT_WHITE>("Stack overflow - exceeded maximum stack depth\n");
		}
		else
		{
			Printer::Print<Printer::Color::BRIGHT_WHITE>("Memory access violation - possible bytecode corruption or invalid operation\n");
			if (ex_info.captured)
			{
				Printer::Print<Printer::Color::BRIGHT_WHITE>("(Exception at 0x");
				Printer::PrintFormatted("{:X}, fault address 0x{:X})\n", ex_info.exception_address, ex_info.fault_address);
			}
		}

		// Print stack trace
		Printer::Print(STACK_TRACE_HEADER.data());
		std::string_view file_name = m_executable->GetFileName();

		// Current frame
		int current_proc = GetProcedureIndexFromIP(m_instruction_pointer);
		int current_line = GetLineFromIP(m_instruction_pointer, current_proc);

		if (current_proc >= 0 && current_proc < static_cast<int>(m_executable->m_procedure_names.size()))
		{
			Printer::Print("  at ");
			Printer::Print<Printer::Color::BRIGHT_YELLOW>(m_executable->m_procedure_names[current_proc].GetCString());
			Printer::PrintFormatted(" in {} (line {})\n", file_name, current_line);
		}
		else
		{
			Printer::PrintFormatted("  at {} in {} (line {})\n", ANONYMOUS_FUNCTION, file_name, current_line);
		}

		// Walk the call stack
		CallStackPointer frame_ptr = m_call_stack_pointer - 1;
		int frame_count = 0;
		int total_frames = static_cast<int>(m_call_stack_pointer - m_call_stack_begin);

		while (frame_ptr >= m_call_stack_begin && frame_count < s_max_stack_trace_depth - 1)
		{
			const CallFrame& frame = *frame_ptr;

			int proc_index = GetProcedureIndexFromIP(frame.return_ip);
			int line = GetLineFromIP(frame.return_ip, proc_index);

			if (proc_index >= 0 && proc_index < static_cast<int>(m_executable->m_procedure_names.size()))
			{
				Printer::Print("  at ");
				Printer::Print<Printer::Color::BRIGHT_YELLOW>(m_executable->m_procedure_names[proc_index].GetCString());
				Printer::PrintFormatted(" in {} (line {})\n", file_name, line);
			}
			else
			{
				Printer::PrintFormatted("  at {} in {} (line {})\n", ANONYMOUS_FUNCTION, file_name, line);
			}

			--frame_ptr;
			++frame_count;
		}

		// Add truncation message if there are more frames
		if (frame_count >= s_max_stack_trace_depth - 1 && total_frames > s_max_stack_trace_depth)
		{
			int remaining = total_frames - s_max_stack_trace_depth;
			Printer::PrintFormatted("  ... ({} more frame{})\n", remaining, remaining == 1 ? "" : "s");
		}

		return EXIT_FAILURE;
	}
#else
	return ExecuteLoop();
#endif
}

