#include "Common/Constant/Constant.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Common/Printer/Printer.h"
#include "Utility/Disassembler/Disassembler.h"
#include "VirtualMachine.h"

#ifdef __EMSCRIPTEN__
#include "Library/MidoriStdLib.h"
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

VirtualMachine::VirtualMachine(MidoriExecutable&& executable) noexcept : m_executable(std::move(executable))
{
#ifdef _WIN32
	// Use VirtualAlloc with guard pages for zero-overhead stack overflow detection
	SYSTEM_INFO si;
	GetSystemInfo(&si);
	size_t page_size = si.dwPageSize;

	// Allocate value stack with guard page
	size_t value_stack_bytes = s_value_stack_size * sizeof(MidoriValue);
	size_t value_total_pages = (value_stack_bytes + page_size - 1u) / page_size + 1u; // +1 for guard page
	size_t value_total_size = value_total_pages * page_size;

	m_value_stack_region = VirtualAlloc(nullptr, value_total_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	if (m_value_stack_region)
	{
		m_value_stack_begin = static_cast<MidoriValue*>(m_value_stack_region);

		// Set guard page at the end
		void* value_guard_page = static_cast<char*>(m_value_stack_region) + (value_total_size - page_size);
		DWORD old_protect;
		VirtualProtect(value_guard_page, page_size, PAGE_NOACCESS, &old_protect);
	}

	// Allocate call stack with guard page
	size_t call_stack_bytes = s_call_stack_size * sizeof(CallFrame);
	size_t call_total_pages = (call_stack_bytes + page_size - 1u) / page_size + 1u; // +1 for guard page
	size_t call_total_size = call_total_pages * page_size;

	m_call_stack_region = VirtualAlloc(nullptr, call_total_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
	if (m_call_stack_region)
	{
		m_call_stack_begin = static_cast<CallFrame*>(m_call_stack_region);

		// Set guard page at the end
		void* call_guard_page = static_cast<char*>(m_call_stack_region) + (call_total_size - page_size);
		DWORD old_protect;
		VirtualProtect(call_guard_page, page_size, PAGE_NOACCESS, &old_protect);
	}
#else
	// Fallback to malloc for non-Windows platforms (future Linux support)
	m_value_stack_begin = static_cast<MidoriValue*>(std::malloc(s_value_stack_size * sizeof(MidoriValue)));
	m_call_stack_begin = static_cast<CallFrame*>(std::malloc(s_call_stack_size * sizeof(CallFrame)));
#endif

	// Initialize stack pointers
	m_value_stack_base_pointer = m_value_stack_begin;
	m_value_stack_pointer = m_value_stack_base_pointer;
	m_call_stack_pointer = m_call_stack_begin;

	// Initialize other members
	m_global_vars.resize(static_cast<size_t>(m_executable.GetGlobalVariableCount()));
	constexpr int runtime_startup_proc_index = 0;
	m_instruction_pointer = &*m_executable.GetBytecodeStream(runtime_startup_proc_index).cbegin();
}

VirtualMachine::~VirtualMachine()
{
	m_garbage_collector.CleanUp();
#ifdef _WIN32
	if (m_library_handle)
	{
		FreeLibrary(m_library_handle);
	}

	// Free guard-protected stacks
	if (m_value_stack_region)
	{
		VirtualFree(m_value_stack_region, 0, MEM_RELEASE);
	}
	if (m_call_stack_region)
	{
		VirtualFree(m_call_stack_region, 0, MEM_RELEASE);
	}
#else
	if (m_library_handle)
	{
		dlclose(m_library_handle);
	}

	// Fallback malloc cleanup
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
	for (int i : std::views::iota(0, m_executable.GetProcedureCount()))
	{
		const BytecodeStream& bytecode = m_executable.GetBytecodeStream(i);
		const OpCode* start = &*bytecode.cbegin();
		const OpCode* end = start + bytecode.GetByteCodeSize();

		if (m_instruction_pointer >= start && m_instruction_pointer < end)
		{
			return m_executable.GetLine(static_cast<int>(m_instruction_pointer - start), i);
		}
	}

	return TerminateExecution(GenerateRuntimeError("Invalid instruction pointer.", 0));
}

OpCode VirtualMachine::ReadByte() noexcept
{
	OpCode op_code = *m_instruction_pointer;
	++m_instruction_pointer;
	return op_code;
}

#if defined(MIDORI_LITTLE_ENDIAN)
int VirtualMachine::ReadShort() noexcept
{
	const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
	const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
	int value = static_cast<int>(static_cast<uint16_t>(b0) | (static_cast<uint16_t>(b1) << 8));
	m_instruction_pointer += 2;
	return value;
}

int VirtualMachine::ReadThreeBytes() noexcept
{
	const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
	const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
	const uint8_t b2 = static_cast<uint8_t>(m_instruction_pointer[2u]);
	int value = static_cast<int>(static_cast<uint32_t>(b0) | (static_cast<uint32_t>(b1) << 8) | (static_cast<uint32_t>(b2) << 16));
	m_instruction_pointer += 3;
	return value;
}
#elif defined(MIDORI_BIG_ENDIAN)
int VirtualMachine::ReadShort() noexcept
{
	const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
	const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
	int value = static_cast<int>((static_cast<uint16_t>(b0) << 8) | static_cast<uint16_t>(b1));
	m_instruction_pointer += 2;
	return value;
}

int VirtualMachine::ReadThreeBytes() noexcept
{
	const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
	const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
	const uint8_t b2 = static_cast<uint8_t>(m_instruction_pointer[2u]);
	int value = static_cast<int>((static_cast<uint32_t>(b0) << 16) | (static_cast<uint32_t>(b1) << 8) | static_cast<uint32_t>(b2));
	m_instruction_pointer += 3;
	return value;
}
#else
#error "Endianness not defined!"
#endif

MidoriInteger VirtualMachine::ReadIntegerConstant() noexcept
{
	uint64_t bits = 0u;
	std::memcpy(&bits, m_instruction_pointer, sizeof(bits));
#if defined(MIDORI_BIG_ENDIAN)
	bits = std::byteswap(bits);
#endif
	m_instruction_pointer += sizeof(MidoriInteger);
	return static_cast<MidoriInteger>(bits);
}

MidoriFloat VirtualMachine::ReadFloatConstant() noexcept
{
	uint64_t bits = 0u;
	std::memcpy(&bits, m_instruction_pointer, sizeof(bits));
#if defined(MIDORI_BIG_ENDIAN)
	bits = std::byteswap(bits);
#endif
	m_instruction_pointer += sizeof(MidoriFloat);
	return std::bit_cast<MidoriFloat>(bits);
}

int VirtualMachine::ReadGlobalVariable() noexcept
{
	int index = static_cast<int>(ReadByte());
	return index;
}

std::string VirtualMachine::GenerateRuntimeError(std::string_view message, int line) noexcept
{
	m_garbage_collector.CleanUp();
	std::string stack_trace = GenerateStackTrace();
	return MidoriError::GenerateRuntimeError(message, line)
		.append("\n")
		.append(stack_trace);
}

int VirtualMachine::GetProcedureIndexFromIP(InstructionPointer ip) noexcept
{
	for (int i : std::views::iota(0, m_executable.GetProcedureCount()))
	{
		const BytecodeStream& bytecode = m_executable.GetBytecodeStream(i);
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
	if (proc_index < 0 || proc_index >= m_executable.GetProcedureCount())
	{
		return 0;
	}

	const BytecodeStream& bytecode = m_executable.GetBytecodeStream(proc_index);
	const OpCode* start = &*bytecode.cbegin();
	int offset = static_cast<int>(ip - start);

	return m_executable.GetLine(offset, proc_index);
}

std::string VirtualMachine::GenerateStackTrace() noexcept
{
	std::string trace = std::string(STACK_TRACE_HEADER);
	std::string_view file_name = m_executable.GetFileName();
	std::string_view function_color = Printer::Detail::GetColorCode(Printer::Color::BRIGHT_YELLOW);
	std::string_view reset = "\033[0m";

	// Current frame (where error occurred)
	int current_proc = GetProcedureIndexFromIP(m_instruction_pointer);
	int current_line = GetLineFromIP(m_instruction_pointer, current_proc);

	if (current_proc >= 0 && current_proc < static_cast<int>(m_executable.m_procedure_names.size()))
	{
		trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, m_executable.m_procedure_names[current_proc].GetCString(), reset, file_name, current_line));
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
		auto [return_bp, return_sp, return_ip, closure_ptr] = *frame_ptr;

		int proc_index = GetProcedureIndexFromIP(return_ip);
		int line = GetLineFromIP(return_ip, proc_index);

		if (proc_index >= 0 && proc_index < static_cast<int>(m_executable.m_procedure_names.size()))
		{
			trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, m_executable.m_procedure_names[proc_index].GetCString(), reset, file_name, line));
		}
		else
		{
			trace.append(std::format("  at {}{}{} in {} (line {})\n", function_color, ANONYMOUS_FUNCTION, reset, file_name, line));
		}

		--frame_ptr;
		++frame_count;
	}

	// Add truncation message if there are more frames
	if (frame_count >= s_max_stack_trace_depth - 1 && total_frames > s_max_stack_trace_depth)
	{
		int remaining = total_frames - s_max_stack_trace_depth;
		trace.append(std::format("  ... ({} more frame{})\n", remaining, remaining == 1 ? "" : "s"));
	}

	return trace;
}

void VirtualMachine::PushCallFrame(ValueStackPointer return_bp, ValueStackPointer return_sp, InstructionPointer return_ip, MidoriArray* closure_ptr) noexcept
{
	*m_call_stack_pointer = std::make_tuple(return_bp, return_sp, return_ip, closure_ptr);

	m_call_stack_pointer++;
}

MidoriValue& VirtualMachine::Peek() noexcept
{
	return *(m_value_stack_pointer - 1);
}

MidoriValue VirtualMachine::Pop() noexcept
{
	return *(--m_value_stack_pointer);
}

void VirtualMachine::PromoteCells() noexcept
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

int VirtualMachine::CheckIndexBounds(MidoriValue index, MidoriInteger size) noexcept
{
	MidoriInteger val = index.GetInteger();
	if (val < 0ll || val >= size)
	{
		return TerminateExecution(GenerateRuntimeError(std::format("Index out of bounds at index: {}.", val), GetLine()));
	}
	return 0;
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

GarbageCollector::GarbageCollectionRoots VirtualMachine::GetGlobalTableGarbageCollectionRoots() const noexcept
{
	GarbageCollector::GarbageCollectionRoots roots;
	roots.reserve(m_global_vars.size());

	for (MidoriValue val : m_global_vars)
	{
		MidoriTraceable* ptr = val.GetPointer();
		if (ptr && m_garbage_collector.Contains(ptr))
		{
			roots.emplace_back(ptr);
		}
	}

	return roots;
}

GarbageCollector::GarbageCollectionRoots VirtualMachine::GetValueStackGarbageCollectionRoots() const noexcept
{
	GarbageCollector::GarbageCollectionRoots roots;
	roots.reserve((m_value_stack_pointer - m_value_stack_begin));

	for (MidoriValue* it = m_value_stack_begin; it != m_value_stack_pointer; ++it)
	{
		MidoriTraceable* ptr = it->GetPointer();
		if (ptr && m_garbage_collector.Contains(ptr))
		{
			roots.emplace_back(ptr);
		}
	}

	return roots;
}

GarbageCollector::GarbageCollectionRoots VirtualMachine::GetGarbageCollectionRoots() const noexcept
{
	GarbageCollector::GarbageCollectionRoots stack_roots = GetValueStackGarbageCollectionRoots();
	GarbageCollector::GarbageCollectionRoots global_roots = GetGlobalTableGarbageCollectionRoots();

	stack_roots.insert(stack_roots.end(), global_roots.cbegin(), global_roots.cend());
	return stack_roots;
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

		for (int i : std::views::iota(0, m_executable.GetProcedureCount()))
		{
			const BytecodeStream& bytecode = m_executable.GetBytecodeStream(i);
			const OpCode* start = &*bytecode.cbegin();
			const OpCode* end = start + bytecode.GetByteCodeSize();

			if (m_instruction_pointer >= start && m_instruction_pointer < end)
			{
				dbg_proc_index = i;
				dbg_instruction_pointer = static_cast<int>(m_instruction_pointer - start);
			}
		}
#if MIDORI_ENABLE_DISASSEMBLY
		Disassembler::DisassembleInstruction(m_executable, dbg_proc_index, dbg_instruction_pointer);
#endif
#endif
		OpCode instruction = ReadByte();

		switch (instruction)
		{
		case OpCode::LOAD_STRING:
		{
			size_t index = static_cast<size_t>(ReadByte());
			Push(m_garbage_collector.AllocateTraceable(m_executable.GetStringPool()[index].data(), PointerTag::TEXT));
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

			Push(m_garbage_collector.AllocateTraceable(std::move(arr), PointerTag::ARRAY));
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

			Push(m_garbage_collector.AllocateTraceable(std::move(new_arr), PointerTag::ARRAY));
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
		case OpCode::CREATE_INT_RANGE:
		{
			MidoriValue end = Pop();
			MidoriValue step = Pop();
			MidoriValue start = Pop();

			MidoriRange range(start, end, step, false);

			Push(m_garbage_collector.AllocateTraceable(std::move(range), PointerTag::RANGE));
			break;
		}
		case OpCode::CREATE_FLOAT_RANGE:
		{
			MidoriValue end = Pop();
			MidoriValue step = Pop();
			MidoriValue start = Pop();

			MidoriRange range(start, end, step, true);

			Push(m_garbage_collector.AllocateTraceable(std::move(range), PointerTag::RANGE));
			break;
		}
		case OpCode::GET_RANGE_START:
		{
			MidoriValue range_ptr = Pop();
			MidoriRange& range = range_ptr.GetPointer()->GetTraceable<MidoriRange>();
			Push(range.GetStart());
			break;
		}
		case OpCode::GET_RANGE_END:
		{
			MidoriValue range_ptr = Pop();
			MidoriRange& range = range_ptr.GetPointer()->GetTraceable<MidoriRange>();
			Push(range.GetEnd());
			break;
		}
		case OpCode::GET_RANGE_STEP:
		{
			MidoriValue range_ptr = Pop();
			MidoriRange& range = range_ptr.GetPointer()->GetTraceable<MidoriRange>();
			Push(range.GetStep());
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
			Peek() = m_garbage_collector.AllocateTraceable(MidoriText::FromFloat(Peek().GetFloat()), PointerTag::TEXT);
			break;
		}
		case OpCode::INT_TO_TEXT:
		{
			Peek() = m_garbage_collector.AllocateTraceable(MidoriText::FromInteger(Peek().GetInteger()), PointerTag::TEXT);
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
		case OpCode::CONCAT_ARRAY:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			MidoriArray& left_value_vector_ref = left.GetPointer()->GetTraceable<MidoriArray>();
			MidoriArray& right_value_vector_ref = right.GetPointer()->GetTraceable<MidoriArray>();
			MidoriArray result = MidoriArray::Concatenate(left_value_vector_ref, right_value_vector_ref);

			left = m_garbage_collector.AllocateTraceable(std::move(result), PointerTag::ARRAY);

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
			break;
		}
		case OpCode::CONCAT_TEXT:
		{
			MidoriValue right = Pop();
			MidoriValue& left = Peek();

			MidoriText& left_value_string_ref = left.GetPointer()->GetTraceable<MidoriText>();
			MidoriText& right_value_string_ref = right.GetPointer()->GetTraceable<MidoriText>();

			MidoriText result = MidoriText::Concatenate(left_value_string_ref, right_value_string_ref);

			left = m_garbage_collector.AllocateTraceable(std::move(result), PointerTag::TEXT);

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
			break;
		}
		case OpCode::APPEND_ARRAY:
		{
			MidoriValue value = Pop();
			MidoriValue& array = Peek();

			MidoriArray& array_ref = array.GetPointer()->GetTraceable<MidoriArray>();
			array_ref.AddBack(value);

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
			break;
		}
		case OpCode::PREPEND_ARRAY:
		{
			MidoriValue value = Pop();
			MidoriValue& array = Peek();

			MidoriArray& array_ref = array.GetPointer()->GetTraceable<MidoriArray>();
			array_ref.AddFront(value);

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
			break;
		}
		case OpCode::APPEND_TEXT:
		{
			MidoriValue value = Pop();
			MidoriValue& text = Peek();

			MidoriText& text_ref = text.GetPointer()->GetTraceable<MidoriText>();
			MidoriText& value_text = value.GetPointer()->GetTraceable<MidoriText>();
			text_ref.Append(value_text);

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
			break;
		}
		case OpCode::PREPEND_TEXT:
		{
			MidoriValue value = Pop();
			MidoriValue& text = Peek();

			MidoriText& text_ref = text.GetPointer()->GetTraceable<MidoriText>();
			MidoriText& value_text = value.GetPointer()->GetTraceable<MidoriText>();
			text_ref.Prepend(value_text);

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
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
			MidoriText& foreign_function_name_ref = foreign_function_name.GetPointer()->GetTraceable<MidoriText>();

#ifdef __EMSCRIPTEN__
			void* proc = reinterpret_cast<void*>(MidoriStdLib::GetFunction(foreign_function_name_ref.GetCString()));
#else
			// Platform-specific dynamic loading from DLL
#ifdef _WIN32
			FARPROC proc = GetProcAddress(m_library_handle, foreign_function_name_ref.GetCString());
#else
			void* proc = dlsym(m_library_handle, foreign_function_name_ref.GetCString());
#endif
#endif
			if (proc == nullptr)
			{
				return TerminateExecution(GenerateRuntimeError(std::format("Failed to load foreign function '{}'.", foreign_function_name_ref.GetCString()), GetLine()));
			}

			void* args[UINT8_MAX];
			for (int i = arity - 1; i >= 0; i -= 1)
			{
				size_t idx = static_cast<size_t>(i);
				MidoriValue arg = Pop();

				if (m_garbage_collector.Contains(arg.GetPointer()))
				{
					MidoriTraceable* ptr = arg.GetPointer();
					if (ptr->IsTraceable<MidoriText>())
					{
						args[static_cast<size_t>(idx)] = (void*)ptr->GetTraceable<MidoriText>().GetCString();
					}
				}
				else
				{
					std::memcpy(&args[idx], &arg, MidoriValue::DATA_BUFFER_SIZE);
				}
			}

			MidoriValue return_val;
			void(*ffi)(void**, void*) = reinterpret_cast<void(*)(void**, void*)>(proc);
			ffi(args, reinterpret_cast<void*>(&return_val));

			Push(return_val);

			break;
		}
		case OpCode::CALL_DEFINED:
		{
			MidoriValue callable = Pop();
			int arity = static_cast<int>(ReadByte());

			// Return address: pop all the arguments and the callee
			PushCallFrame(m_value_stack_base_pointer, m_value_stack_pointer - arity, m_instruction_pointer, m_curr_environment);

			MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();
			m_curr_environment = &closure.m_cell_values;

			m_instruction_pointer = m_executable.GetBytecodeStream(closure.m_proc_index)[0u];
			m_value_stack_base_pointer = m_value_stack_pointer - arity;

			break;
		}
		case OpCode::TAIL_CALL:
		{
			MidoriValue callable = Pop();
			int arity = static_cast<int>(ReadByte());

			// Move arguments down to base pointer
			MidoriValue* args_source = m_value_stack_pointer - arity;
			std::memmove(m_value_stack_base_pointer, args_source, arity * sizeof(MidoriValue));
			m_value_stack_pointer = m_value_stack_base_pointer + arity;

			MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();
			m_curr_environment = &closure.m_cell_values;

			// Jump to the start of the function without creating a new call frame
			m_instruction_pointer = m_executable.GetBytecodeStream(closure.m_proc_index)[0u];

			break;
		}
		case OpCode::CONSTRUCT_STRUCT:
		{
			MidoriTraceable* new_struct = m_garbage_collector.AllocateTraceable(MidoriStruct(), PointerTag::STRUCT);
			int size = static_cast<int>(ReadByte());
			MidoriArray args(size);

			for (int i = size - 1; i >= 0; i -= 1)
			{
				args[i] = Pop();
			}

			MidoriArray& members = new_struct->GetTraceable<MidoriStruct>().m_values;
			members = std::move(args);

			Push(new_struct);
			break;
		}
		case OpCode::CONSTRUCT_UNION:
		{
			MidoriTraceable* new_union = m_garbage_collector.AllocateTraceable(MidoriUnion(), PointerTag::UNION);

			int size = static_cast<int>(ReadByte());
			MidoriArray args(size);

			for (int i = size - 1; i >= 0; i -= 1)
			{
				args[i] = Pop();
			}

			MidoriArray& members = new_union->GetTraceable<MidoriUnion>().m_values;
			members = std::move(args);

			Push(new_union);
			break;
		}
		case OpCode::ALLOCATE_CLOSURE:
		{
			int proc_index = static_cast<int>(ReadByte());
			Push(m_garbage_collector.AllocateTraceable(MidoriClosure{ .m_cell_values = MidoriArray(), .m_proc_index = proc_index }, PointerTag::FUNCTION));
			break;
		}
		case OpCode::CONSTRUCT_CLOSURE:
		{
			int captured_count = static_cast<int>(ReadByte());

			if (captured_count == 0)
			{
				break;
			}

			MidoriArray& captured_variables = (m_value_stack_pointer - 1)->GetPointer()->GetTraceable<MidoriClosure>().m_cell_values;

			captured_variables = *m_curr_environment;
			captured_count -= captured_variables.GetLength();

#ifdef __EMSCRIPTEN__
			std::for_each_n
			(
				m_value_stack_base_pointer,
				captured_count,
				[&captured_variables, this](MidoriValue& value)
				{
					MidoriValue* stack_value_ref = &value;
					MidoriValue cell_value = m_garbage_collector.AllocateTraceable(MidoriCellValue(stack_value_ref), PointerTag::CELL);
					captured_variables.AddBack(cell_value);
					m_cells_to_promote.emplace_back(&cell_value.GetPointer()->GetTraceable<MidoriCellValue>());
				}
			);
#else
			std::for_each_n
			(
				std::execution::seq,
				m_value_stack_base_pointer,
				captured_count,
				[&captured_variables, this](MidoriValue& value)
				{
					MidoriValue* stack_value_ref = &value;
					MidoriValue cell_value = m_garbage_collector.AllocateTraceable(MidoriCellValue(stack_value_ref), PointerTag::CELL);
					captured_variables.AddBack(cell_value);
					m_cells_to_promote.emplace_back(&cell_value.GetPointer()->GetTraceable<MidoriCellValue>());
				}
			);
#endif

			if (m_garbage_collector.ShouldCollect())
			{
				m_garbage_collector.ReclaimMemory(GetGarbageCollectionRoots());
			}
			break;
		}
		case OpCode::DEFINE_GLOBAL:
		{
			MidoriValue value = Pop();
			int global_idx = ReadGlobalVariable();
			MidoriValue& var = m_global_vars[global_idx];
			var = value;
			break;
		}
		case OpCode::GET_GLOBAL:
		{
			int global_idx = ReadGlobalVariable();
			Push(m_global_vars[global_idx]);
			break;
		}
		case OpCode::SET_GLOBAL:
		{
			int global_idx = ReadGlobalVariable();
			MidoriValue& var = m_global_vars[global_idx];
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
		case OpCode::GET_CELL:
		{
			int offset = static_cast<int>(ReadByte());
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
			var.GetPointer()->GetTraceable<MidoriStruct>().m_values[index] = value;
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
			std::tie(m_value_stack_base_pointer, m_value_stack_pointer, m_instruction_pointer, m_curr_environment) = *m_call_stack_pointer;

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
		default:
		{
#ifdef _MSC_VER
			__assume(0);
#else
			__builtin_unreachable();
#endif
		}
		}
	}
}

int VirtualMachine::Execute() noexcept
{
#ifndef __EMSCRIPTEN__
	// Skip library loading in WASM - no DLL support in browser
#ifdef _WIN32
	m_library_handle = LoadLibrary(STDLIB_DLL_PATH);
#else
	m_library_handle = dlopen(STDLIB_SO_PATH, RTLD_LAZY);
#endif

	if (m_library_handle == NULL) [[unlikely]]
	{
#ifdef _WIN32
		FreeLibrary(m_library_handle);
#else
		dlclose(m_library_handle);
#endif
		return TerminateExecution(STDLIB_LOAD_ERROR.data());
	}
#endif

#ifdef _WIN32
	// Structured exception handling for guard page access violations (stack overflow)
	__try
	{
		return ExecuteLoop();
	}
	__except (GetExceptionCode() == EXCEPTION_ACCESS_VIOLATION ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH)
	{
		// Print error header
		Printer::Print<Printer::Color::BRIGHT_RED>("Runtime Error");
		Printer::Print(" at ");
		Printer::Print<Printer::Color::BRIGHT_CYAN>("line ");
		Printer::PrintFormatted("{}\n", GetLine());
		Printer::Print<Printer::Color::BRIGHT_WHITE>("Stack overflow - exceeded maximum stack depth\n");

		// Print stack trace
		Printer::Print(STACK_TRACE_HEADER.data());
		std::string_view file_name = m_executable.GetFileName();

		// Current frame
		int current_proc = GetProcedureIndexFromIP(m_instruction_pointer);
		int current_line = GetLineFromIP(m_instruction_pointer, current_proc);

		if (current_proc >= 0 && current_proc < static_cast<int>(m_executable.m_procedure_names.size()))
		{
			Printer::Print("  at ");
			Printer::Print<Printer::Color::BRIGHT_YELLOW>(m_executable.m_procedure_names[current_proc].GetCString());
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
			auto [return_bp, return_sp, return_ip, closure_ptr] = *frame_ptr;

			int proc_index = GetProcedureIndexFromIP(return_ip);
			int line = GetLineFromIP(return_ip, proc_index);

			if (proc_index >= 0 && proc_index < static_cast<int>(m_executable.m_procedure_names.size()))
			{
				Printer::Print("  at ");
				Printer::Print<Printer::Color::BRIGHT_YELLOW>(m_executable.m_procedure_names[proc_index].GetCString());
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
