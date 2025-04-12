#include <algorithm>
#include <cmath>
#include <execution>
#include <format>
#include <numeric>

#include "Common/Constant/Constant.h"
#include "Common/Printer/Printer.h"
#include "Utility/Disassembler/Disassembler.h"
#include "VirtualMachine.h"

using namespace std::string_literals;

VirtualMachine::VirtualMachine(MidoriExecutable&& executable) noexcept : m_executable(std::move(executable)), m_garbage_collector(m_executable.GetConstantRoots())
{
	m_value_stack_begin = static_cast<MidoriValue*>(std::malloc(s_value_stack_size * sizeof(MidoriValue)));
	m_call_stack_begin = static_cast<CallFrame*>(std::malloc(s_call_stack_size * sizeof(CallFrame)));

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
	FreeLibrary(m_library_handle);
#else
	dlclose(m_library_handle);
#endif

	std::free(m_value_stack_begin);
	std::free(m_call_stack_begin);
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
		if (m_instruction_pointer >= &*m_executable.GetBytecodeStream(i).cbegin() && m_instruction_pointer <= &*std::prev(m_executable.GetBytecodeStream(i).cend()))
		{
			return m_executable.GetLine(static_cast<int>(m_instruction_pointer - &*m_executable.GetBytecodeStream(i).cbegin()), i);
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
	int value = static_cast<int>(*reinterpret_cast<const uint16_t*>(m_instruction_pointer));
	m_instruction_pointer += 2;
	return value;
}

int VirtualMachine::ReadThreeBytes() noexcept
{
	int value = static_cast<int>(*reinterpret_cast<const uint16_t*>(m_instruction_pointer)) |
		(static_cast<int>(m_instruction_pointer[2]) << 16);
	m_instruction_pointer += 3;
	return value;
}
#elif defined(MIDORI_BIG_ENDIAN)
int VirtualMachine::ReadShort() noexcept
{
	int value = (static_cast<int>(m_instruction_pointer[0]) << 8) |
		static_cast<int>(*reinterpret_cast<const uint8_t*>(m_instruction_pointer + 1));
	m_instruction_pointer += 2;
	return value;
}

int VirtualMachine::ReadThreeBytes() noexcept
{
	int value = (static_cast<int>(m_instruction_pointer[0]) << 16) |
		(static_cast<int>(m_instruction_pointer[1]) << 8) |
		static_cast<int>(*reinterpret_cast<const uint8_t*>(m_instruction_pointer + 2));
	m_instruction_pointer += 3;
	return value;
}
#else
#error "Endianness not defined!"
#endif

MidoriInteger VirtualMachine::ReadIntegerConstant() noexcept
{
	MidoriInteger value = *reinterpret_cast<const MidoriInteger*>(m_instruction_pointer);
	m_instruction_pointer += sizeof(MidoriInteger);
	return value;
}

MidoriFraction VirtualMachine::ReadFractionConstant() noexcept
{
	MidoriFraction value = *reinterpret_cast<const MidoriFraction*>(m_instruction_pointer);
	m_instruction_pointer += sizeof(MidoriFraction);
	return value;
}

MidoriValue VirtualMachine::ReadConstant(OpCode operand_length) noexcept
{
	int index = 0;

	switch (operand_length)
	{
	case OpCode::LOAD_CONSTANT:
	{
		index = static_cast<int>(ReadByte());
		break;
	}
	case OpCode::LOAD_CONSTANT_LONG:
	{
		index = static_cast<int>(ReadByte()) |
			(static_cast<int>(ReadByte()) << 8);
		break;
	}
	case OpCode::LOAD_CONSTANT_LONG_LONG:
	{
		index = static_cast<int>(ReadByte()) |
			(static_cast<int>(ReadByte()) << 8) |
			(static_cast<int>(ReadByte()) << 16);
		break;
	}
	default:
	{
		break; // unreachable
	}
	}

	return m_executable.GetConstant(index);
}

int VirtualMachine::ReadGlobalVariable() noexcept
{
	int index = static_cast<int>(ReadByte());
	return index;
}

std::string VirtualMachine::GenerateRuntimeError(std::string_view message, int line) noexcept
{
	m_garbage_collector.CleanUp();
	return MidoriError::GenerateRuntimeError(message, line);
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
		if (!cell->m_is_on_heap && cell->m_value.m_stack_value_ref >= m_value_stack_base_pointer)
		{
			cell->m_is_on_heap = true;
			cell->m_value.m_heap_value = *cell->m_value.m_stack_value_ref;
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

MidoriTraceable::GarbageCollectionRoots VirtualMachine::GetGlobalTableGarbageCollectionRoots() const noexcept
{
	MidoriTraceable::GarbageCollectionRoots roots;
	roots.reserve(m_global_vars.size());

	std::ranges::for_each
	(
		m_global_vars,
		[&roots](MidoriValue val) -> void
		{
			if (MidoriTraceable::s_traceables.contains(val.GetPointer()))
			{
				roots.emplace(val.GetPointer());
			}
		}
	);

	return roots;
}

MidoriTraceable::GarbageCollectionRoots VirtualMachine::GetValueStackGarbageCollectionRoots() const noexcept
{
	MidoriTraceable::GarbageCollectionRoots roots;
	roots.reserve((m_value_stack_pointer - m_value_stack_begin) + (m_call_stack_pointer - m_call_stack_begin));

	std::for_each_n
	(
		std::execution::seq,
		m_value_stack_begin,
		m_value_stack_pointer - m_value_stack_begin,
		[&roots](MidoriValue value) -> void
		{
			if (MidoriTraceable::s_traceables.contains(value.GetPointer()))
			{
				roots.emplace(value.GetPointer());
			}
		}
	);

	return roots;
}

MidoriTraceable::GarbageCollectionRoots VirtualMachine::GetGarbageCollectionRoots() const noexcept
{
	MidoriTraceable::GarbageCollectionRoots stack_roots = GetValueStackGarbageCollectionRoots();
	MidoriTraceable::GarbageCollectionRoots global_roots = GetGlobalTableGarbageCollectionRoots();

	stack_roots.insert(global_roots.cbegin(), global_roots.cend());
	return stack_roots;
}

void VirtualMachine::CollectGarbage() noexcept
{
	if (MidoriTraceable::s_total_bytes_allocated - MidoriTraceable::s_static_bytes_allocated < s_garbage_collection_threshold)
	{
		return;
	}

	MidoriTraceable::GarbageCollectionRoots roots = GetGarbageCollectionRoots();
	if (roots.empty()) [[unlikely]]
		{
			return;
		}
	else [[likely]]
		{
#ifdef DEBUG
			Printer::Print<Printer::Color::BLUE>("\nBefore garbage collection:");
			m_garbage_collector.PrintMemoryTelemetry();
#endif
			m_garbage_collector.ReclaimMemory(std::move(roots));
#ifdef DEBUG
			Printer::Print<Printer::Color::BLUE>("\nAfter garbage collection:");
			m_garbage_collector.PrintMemoryTelemetry();
#endif
		}
}

int VirtualMachine::Execute() noexcept
{
#ifdef _WIN32
	m_library_handle = LoadLibrary("./MidoriStdLib.dll");
#else
	m_library_handle = dlopen("./libMidoriStdLib.so", RTLD_LAZY);
#endif

	if (m_library_handle == NULL) [[unlikely]]
		{
#ifdef _WIN32
			FreeLibrary(m_library_handle);
#else
			dlclose(m_library_handle);
#endif
			return TerminateExecution("Failed to load the standard library.");
		}

			while (true)
			{
#ifdef DEBUG
				Printer::Print("          ");
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
				Printer::Print("\n");
				int dbg_instruction_pointer = -1;
				int dbg_proc_index = -1;

				for (int i : std::views::iota(0, m_executable.GetProcedureCount()))
				{
					if (&*m_instruction_pointer >= &*m_executable.GetBytecodeStream(i).cbegin() && &*m_instruction_pointer <= &*std::prev(m_executable.GetBytecodeStream(i).cend()))
					{
						dbg_proc_index = i;
						dbg_instruction_pointer = static_cast<int>(m_instruction_pointer - &*m_executable.GetBytecodeStream(i).cbegin());
					}
				}

				Disassembler::DisassembleInstruction(m_executable, dbg_proc_index, dbg_instruction_pointer);
#endif
				OpCode instruction = ReadByte();

				switch (instruction)
				{
				case OpCode::LOAD_CONSTANT:
				case OpCode::LOAD_CONSTANT_LONG:
				case OpCode::LOAD_CONSTANT_LONG_LONG:
				{
					Push(ReadConstant(instruction));
					break;
				}
				case OpCode::INTEGER_CONSTANT:
				{
					Push(ReadIntegerConstant());
					break;
				}
				case OpCode::FRACTION_CONSTANT:
				{
					Push(ReadFractionConstant());
					break;
				}
				case OpCode::OP_UNIT:
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
				case OpCode::CREATE_ARRAY:
				{
					int count = ReadThreeBytes();
					MidoriArray arr(count);

					for (int i = count - 1; i >= 0; i -= 1)
					{
						arr[i] = Pop();
					}

					Push(MidoriTraceable::AllocateTraceable(std::move(arr), PointerTag::ARRAY));
					CollectGarbage();
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

					Push(MidoriTraceable::AllocateTraceable(std::move(new_arr), PointerTag::ARRAY));
					CollectGarbage();
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
				case OpCode::INT_TO_FRAC:
				{
					Peek() = static_cast<MidoriFraction>(Peek().GetInteger());
					break;
				}
				case OpCode::TEXT_TO_FRAC:
				{
					Peek() = static_cast<MidoriFraction>(Peek().GetPointer()->GetTraceable<MidoriText>().ToFraction());
					break;
				}
				case OpCode::FRAC_TO_INT:
				{
					Peek() = static_cast<MidoriInteger>(Peek().GetFraction());
					break;
				}
				case OpCode::TEXT_TO_INT:
				{
					Peek() = static_cast<MidoriInteger>(Peek().GetPointer()->GetTraceable<MidoriText>().ToInteger());
					break;
				}
				case OpCode::FRAC_TO_TEXT:
				{
					Peek() = MidoriTraceable::AllocateTraceable(MidoriText::FromFraction(Peek().GetFraction()), PointerTag::TEXT);
					CollectGarbage();
					break;
				}
				case OpCode::INT_TO_TEXT:
				{
					Peek() = MidoriTraceable::AllocateTraceable(MidoriText::FromInteger(Peek().GetInteger()), PointerTag::TEXT);
					CollectGarbage();
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
				case OpCode::ADD_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() + right.GetFraction();

					break;
				}
				case OpCode::SUBTRACT_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() - right.GetFraction();

					break;
				}
				case OpCode::MULTIPLY_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() * right.GetFraction();

					break;
				}
				case OpCode::DIVIDE_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() / right.GetFraction();

					break;
				}
				case OpCode::MODULO_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = std::fmod(left.GetFraction(), right.GetFraction());

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

					left = MidoriTraceable::AllocateTraceable(std::move(result), PointerTag::ARRAY);
					CollectGarbage();
					break;
				}
				case OpCode::CONCAT_TEXT:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					MidoriText& left_value_string_ref = left.GetPointer()->GetTraceable<MidoriText>();
					MidoriText& right_value_string_ref = right.GetPointer()->GetTraceable<MidoriText>();

					MidoriText result = MidoriText::Concatenate(left_value_string_ref, right_value_string_ref);

					left = MidoriTraceable::AllocateTraceable(std::move(result), PointerTag::TEXT);
					CollectGarbage();
					break;
				}
				case OpCode::EQUAL_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() == right.GetFraction();

					break;
				}
				case OpCode::NOT_EQUAL_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() != right.GetFraction();

					break;
				}
				case OpCode::GREATER_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() > right.GetFraction();

					break;
				}
				case OpCode::GREATER_EQUAL_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() >= right.GetFraction();

					break;
				}
				case OpCode::LESS_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() < right.GetFraction();

					break;
				}
				case OpCode::LESS_EQUAL_FRACTION:
				{
					MidoriValue right = Pop();
					MidoriValue& left = Peek();

					left = left.GetFraction() <= right.GetFraction();

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
				case OpCode::NEGATE_FRACTION:
				{
					MidoriValue& value = Peek();
					value = -value.GetFraction();
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
				case OpCode::IF_FRACTION_LESS:
				{
					int offset = ReadShort();
					MidoriFraction right = Pop().GetFraction();
					MidoriFraction left = Pop().GetFraction();

					if (!(left < right))
					{
						m_instruction_pointer += offset;
					}
					break;
				}
				case OpCode::IF_FRACTION_LESS_EQUAL:
				{
					int offset = ReadShort();
					MidoriFraction right = Pop().GetFraction();
					MidoriFraction left = Pop().GetFraction();

					if (!(left <= right))
					{
						m_instruction_pointer += offset;
					}
					break;
				}
				case OpCode::IF_FRACTION_GREATER:
				{
					int offset = ReadShort();
					MidoriFraction right = Pop().GetFraction();
					MidoriFraction left = Pop().GetFraction();

					if (!(left > right))
					{
						m_instruction_pointer += offset;
					}
					break;
				}
				case OpCode::IF_FRACTION_GREATER_EQUAL:
				{
					int offset = ReadShort();
					MidoriFraction right = Pop().GetFraction();
					MidoriFraction left = Pop().GetFraction();

					if (!(left >= right))
					{
						m_instruction_pointer += offset;
					}
					break;
				}
				case OpCode::IF_FRACTION_EQUAL:
				{
					int offset = ReadShort();
					MidoriFraction right = Pop().GetFraction();
					MidoriFraction left = Pop().GetFraction();

					if (!(left == right))
					{
						m_instruction_pointer += offset;
					}
					break;
				}
				case OpCode::IF_FRACTION_NOT_EQUAL:
				{
					int offset = ReadShort();
					MidoriFraction right = Pop().GetFraction();
					MidoriFraction left = Pop().GetFraction();

					if (!(left != right))
					{
						m_instruction_pointer += offset;
					}
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
				case OpCode::CALL_FOREIGN:
				{
					MidoriValue foreign_function_name = Pop();
					int arity = static_cast<int>(ReadByte());
					MidoriText& foreign_function_name_ref = foreign_function_name.GetPointer()->GetTraceable<MidoriText>();

					// Platform-specific function loading
#ifdef _WIN32
					FARPROC proc = GetProcAddress(m_library_handle, foreign_function_name_ref.GetCString());
#else
					void* proc = dlsym(m_library_handle, foreign_function_name_ref.c_str());
#endif
					if (proc == nullptr)
					{
						return TerminateExecution(GenerateRuntimeError(std::format("Failed to load foreign function '{}'.", foreign_function_name_ref.GetCString()), GetLine()));
					}

					void* args[UINT8_MAX];
					for (int i = arity - 1; i >= 0; i -= 1) 
					{
						MidoriValue arg = Pop();

						if (MidoriTraceable::s_traceables.contains(arg.GetPointer()))
						{
							args[i] = (void*)arg.GetPointer()->GetTraceable<MidoriText>().GetCString();
						}
						else 
						{
							std::memcpy(&args[i], &arg, sizeof(MidoriValue));
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

					// Return address := pop all the arguments and the callee
					PushCallFrame(m_value_stack_base_pointer, m_value_stack_pointer - arity, m_instruction_pointer, m_curr_environment);

					MidoriClosure& closure = callable.GetPointer()->GetTraceable<MidoriClosure>();
					m_curr_environment = &closure.m_cell_values;

					m_instruction_pointer = m_executable.GetBytecodeStream(closure.m_proc_index)[0u];
					m_value_stack_base_pointer = m_value_stack_pointer - arity;

					break;
				}
				case OpCode::CONSTRUCT_STRUCT:
				{
					MidoriTraceable* new_struct = MidoriTraceable::AllocateTraceable(MidoriStruct(), PointerTag::STRUCT);
					int size = static_cast<int>(ReadByte());
					MidoriArray args(size);

					for (int i = size - 1; i >= 0; i -= 1)
					{
						args[i] = Pop();
					}

					MidoriArray& members = new_struct->GetTraceable<MidoriStruct>().m_values;
					members = std::move(args);

					Push(new_struct);
					CollectGarbage();
					break;
				}
				case OpCode::CONSTRUCT_UNION:
				{
					MidoriTraceable* new_union = MidoriTraceable::AllocateTraceable(MidoriUnion(), PointerTag::UNION);

					int size = static_cast<int>(ReadByte());
					MidoriArray args(size);

					for (int i = size - 1; i >= 0; i -= 1)
					{
						args[i] = Pop();
					}

					MidoriArray& members = new_union->GetTraceable<MidoriUnion>().m_values;
					members = std::move(args);

					Push(new_union);
					CollectGarbage();
					break;
				}
				case OpCode::ALLOCATE_CLOSURE:
				{
					int proc_index = static_cast<int>(ReadByte());
					Push(MidoriTraceable::AllocateTraceable(MidoriClosure{ .m_cell_values = MidoriArray(), .m_proc_index = proc_index}, PointerTag::FUNCTION));
					CollectGarbage();
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

					std::for_each_n
					(
						std::execution::seq,
						m_value_stack_base_pointer,
						captured_count,
						[&captured_variables, this](MidoriValue& value)
						{
							MidoriValue* stack_value_ref = &value;
							MidoriValue cell_value = MidoriTraceable::AllocateTraceable(MidoriCellValue(stack_value_ref), PointerTag::CELL);
							captured_variables.AddBack(cell_value);
							m_cells_to_promote.emplace_back(& cell_value.GetPointer()->GetTraceable<MidoriCellValue>());
						}
					);

					CollectGarbage();
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
				case OpCode::POP_SCOPE:
				{
					// on scope exit, promote all cells to heap
					PromoteCells();

					m_value_stack_pointer -= static_cast<int>(ReadByte());
					break;
				}
				case OpCode::POP_MULTIPLE:
				{
					m_value_stack_pointer -= static_cast<int>(ReadByte());
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