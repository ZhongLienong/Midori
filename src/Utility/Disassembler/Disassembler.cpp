#include <iomanip>
#include <sstream>
#include <string>

#include "Common/Executable/Executable.h"
#include "Common/Printer/Printer.h"
#include "Disassembler.h"

#if MIDORI_ENABLE_DISASSEMBLY

namespace
{
	constexpr int address_width = 6;
	constexpr int instr_width = 25;
	constexpr int operand_width = 12;
	constexpr int comment_width = 30;

	void SimpleInstruction(std::string_view name, int& offset)
	{
		offset += 1;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void PopMultipleInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void NumericConstantInstruction(bool is_integer, std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		std::byte operand_bytes[8];
		for (int i = 0; i < 8; i += 1)
		{
			operand_bytes[i] = static_cast<std::byte>(executable.ReadByteCode(offset + 1 + i, proc_index));
		}
		offset += 9;

		std::ostringstream formated_str;
		MidoriFloat as_float = *reinterpret_cast<MidoriFloat*>(operand_bytes);
		MidoriInteger as_integer = *reinterpret_cast<MidoriInteger*>(operand_bytes);

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		if (is_integer)
		{
			formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(as_integer));
			formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// " + std::to_string(as_integer));
		}
		else
		{
			formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(as_float));
			formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// " + std::to_string(as_float));
		}
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void ByteConstantInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		MidoriByte operand = static_cast<MidoriByte>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;

		std::ostringstream formated_str;
		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(static_cast<unsigned int>(operand)));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// 0x" + (std::ostringstream() << std::hex << std::uppercase << static_cast<unsigned int>(operand)).str());
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void WordConstantInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		std::byte operand_bytes[8];
		for (int i = 0; i < 8; i += 1)
		{
			operand_bytes[i] = static_cast<std::byte>(executable.ReadByteCode(offset + 1 + i, proc_index));
		}
		offset += 9;

		std::ostringstream formated_str;
		MidoriWord operand = *reinterpret_cast<MidoriWord*>(operand_bytes);

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// 0x" + (std::ostringstream() << std::hex << std::uppercase << operand).str());
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void LoadStringInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int index = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(index));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// string pool index");
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void JumpInstruction(std::string_view name, int sign, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index)) |
			(static_cast<int>(executable.ReadByteCode(offset + 2, proc_index)) << 8);
		offset += 3;
		std::ostringstream formated_str;

		int destination = offset + sign * operand;
		std::ostringstream dest_str;
		dest_str << "-> 0x" << std::hex << std::setfill('0') << std::setw(address_width) << destination;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_YELLOW>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>(dest_str.str());
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void GlobalVariableInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// " + std::string(executable.GetGlobalVariable(operand).GetCString()));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void LocalOrCellVariableInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// offset " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void ArrayInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// number of indices: " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void ArrayCreateInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index)) |
			(static_cast<int>(executable.ReadByteCode(offset + 2, proc_index)) << 8) |
			(static_cast<int>(executable.ReadByteCode(offset + 3, proc_index)) << 16);
		offset += 4;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// array length: " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void ClosureCreateInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int captured_count = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(captured_count));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// number of captured variables: " + std::to_string(captured_count));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void AllocateClosureInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int index = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(index));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// code index: " + std::to_string(index));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void CallInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// number of parameters: " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void MemberInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// member index: " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void DataInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// data size: " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void SetTagInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int operand = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		offset += 2;
		std::ostringstream formated_str;

		formated_str << Printer::Colored<Printer::Color::BRIGHT_WHITE>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(operand));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// union tag: " + std::to_string(operand));
		formated_str << '\n';
		Printer::Print(formated_str.str());
	}

	void MatchJumpTableInstruction(std::string_view name, const MidoriExecutable& executable, int proc_index, int& offset)
	{
		int case_count = static_cast<int>(executable.ReadByteCode(offset + 1, proc_index));
		int table_start = offset + 2;

		std::ostringstream formated_str;
		formated_str << Printer::Colored<Printer::Color::BRIGHT_YELLOW>(std::string(name));
		formated_str << " " << Printer::Colored<Printer::Color::CYAN>(std::to_string(case_count));
		formated_str << "  " << Printer::Colored<Printer::Color::DARK_GRAY>("// " + std::to_string(case_count) + " cases");
		formated_str << '\n';
		Printer::Print(formated_str.str());

		// Advance offset past opcode and case count
		offset += 2;

		// Print each jump table entry
		for (int i = 0; i < case_count; i += 1)
		{
			int jump_offset = static_cast<int>(executable.ReadByteCode(offset, proc_index)) |
				(static_cast<int>(executable.ReadByteCode(offset + 1, proc_index)) << 8);

			int destination = table_start + case_count * 2 + jump_offset;

			std::ostringstream entry_str;
			entry_str << "    [" << Printer::Colored<Printer::Color::CYAN>(std::to_string(i)) << "] ";
			entry_str << "-> 0x" << std::hex << std::setfill('0') << std::setw(::address_width) << destination;
			entry_str << Printer::Colored<Printer::Color::DARK_GRAY>(" (offset +" + std::to_string(jump_offset) + ")");
			entry_str << '\n';
			Printer::Print(entry_str.str());

			offset += 2;
		}
	}
}

namespace Disassembler
{
	// Forward declaration
	void DisassembleInstruction(const MidoriExecutable& executable, int proc_index, int& offset);

	void DisassembleBytecodeStream(const MidoriExecutable& executable, int proc_index, std::string_view proc_name)
	{
		std::ostringstream header;
		header << std::string(95, '=') << "\n";
		header << " " << Printer::Colored<Printer::Color::BRIGHT_CYAN>(std::string(proc_name)) << "\n";
		header << std::string(95, '=') << "\n";
		Printer::Print(header.str());

		int offset = 0;
		while (offset < executable.GetByteCodeSize(proc_index))
		{
			DisassembleInstruction(executable, proc_index, offset);
		}

		std::ostringstream footer;
		footer << Printer::Colored<Printer::Color::DARK_GRAY>(std::string(95, '-')) << "\n\n";
		Printer::Print(footer.str());
	}

	void DisassembleInstruction(const MidoriExecutable& executable, int proc_index, int& offset)
	{
		std::ostringstream formated_str;
		formated_str << '[' << std::right << std::setfill('0') << std::setw(::address_width) << std::hex << offset << "] " << std::setfill(' ');

		formated_str << std::setw(::address_width) << std::left;
		if (offset > 0 && executable.GetLine(offset, proc_index) == executable.GetLine(offset - 1, proc_index))
		{
			formated_str << "|" << std::setfill(' ');
		}
		else
		{
			formated_str << std::dec << executable.GetLine(offset, proc_index) << std::setfill(' ');
		}
		formated_str << std::right << ' ';
		Printer::Print(formated_str.str());

		OpCode instruction = executable.ReadByteCode(offset, proc_index);
		switch (instruction) 
		{
		case OpCode::LOAD_STRING:
			LoadStringInstruction("LOAD_STRING", executable, proc_index, offset);
			break;
		case OpCode::INTEGER_CONSTANT:
			NumericConstantInstruction(true, "INTEGER_CONSTANT", executable, proc_index, offset);
			break;
		case OpCode::FLOAT_CONSTANT:
			NumericConstantInstruction(false, "FLOAT_CONSTANT", executable, proc_index, offset);
			break;
		case OpCode::BYTE_CONSTANT:
			ByteConstantInstruction("BYTE_CONSTANT", executable, proc_index, offset);
			break;
		case OpCode::WORD_CONSTANT:
			WordConstantInstruction("WORD_CONSTANT", executable, proc_index, offset);
			break;
		case OpCode::OP_UNIT:
			SimpleInstruction("OP_UNIT", offset);
			break;
		case OpCode::OP_TRUE:
			SimpleInstruction("OP_TRUE", offset);
			break;
		case OpCode::OP_FALSE:
			SimpleInstruction("OP_FALSE", offset);
			break;
		case OpCode::INT_MINUS_1:
			SimpleInstruction("INT_MINUS_1", offset);
			break;
		case OpCode::INT_0:
			SimpleInstruction("INT_0", offset);
			break;
		case OpCode::INT_1:
			SimpleInstruction("INT_1", offset);
			break;
		case OpCode::INT_2:
			SimpleInstruction("INT_2", offset);
			break;
		case OpCode::INT_3:
			SimpleInstruction("INT_3", offset);
			break;
		case OpCode::INT_4:
			SimpleInstruction("INT_4", offset);
			break;
		case OpCode::INT_5:
			SimpleInstruction("INT_5", offset);
			break;
		case OpCode::INT_10:
			SimpleInstruction("INT_10", offset);
			break;
		case OpCode::CREATE_ARRAY:
			ArrayCreateInstruction("CREATE_ARRAY", executable, proc_index, offset);
			break;
		case OpCode::GET_ARRAY:
			ArrayInstruction("GET_ARRAY", executable, proc_index, offset);
			break;
		case OpCode::SET_ARRAY:
			ArrayInstruction("SET_ARRAY", executable, proc_index, offset);
			break;
		case OpCode::DUP_ARRAY:
			SimpleInstruction("DUP_ARRAY", offset);
			break;
		case OpCode::ADD_BACK_ARRAY:
			SimpleInstruction("ADD_BACK_ARRAY", offset);
			break;
		case OpCode::ADD_FRONT_ARRAY:
			SimpleInstruction("ADD_FRONT_ARRAY", offset);
			break;
		case OpCode::CREATE_INT_RANGE:
			SimpleInstruction("CREATE_INT_RANGE", offset);
			break;
		case OpCode::CREATE_FLOAT_RANGE:
			SimpleInstruction("CREATE_FLOAT_RANGE", offset);
			break;
		case OpCode::GET_RANGE_START:
			SimpleInstruction("GET_RANGE_START", offset);
			break;
		case OpCode::GET_RANGE_END:
			SimpleInstruction("GET_RANGE_END", offset);
			break;
		case OpCode::GET_RANGE_STEP:
			SimpleInstruction("GET_RANGE_STEP", offset);
			break;
		case OpCode::INT_TO_FLOAT:
			SimpleInstruction("INT_TO_FLOAT", offset);
			break;
		case OpCode::TEXT_TO_FLOAT:
			SimpleInstruction("TEXT_TO_FLOAT", offset);
			break;
		case OpCode::FLOAT_TO_INT:
			SimpleInstruction("FLOAT_TO_INT", offset);
			break;
		case OpCode::TEXT_TO_INT:
			SimpleInstruction("TEXT_TO_INT", offset);
			break;
		case OpCode::FLOAT_TO_TEXT:
			SimpleInstruction("FLOAT_TO_TEXT", offset);
			break;
		case OpCode::INT_TO_TEXT:
			SimpleInstruction("INT_TO_TEXT", offset);
			break;
		case OpCode::BYTE_TO_INT:
			SimpleInstruction("BYTE_TO_INT", offset);
			break;
		case OpCode::INT_TO_BYTE:
			SimpleInstruction("INT_TO_BYTE", offset);
			break;
		case OpCode::BYTE_TO_WORD:
			SimpleInstruction("BYTE_TO_WORD", offset);
			break;
		case OpCode::WORD_TO_BYTE:
			SimpleInstruction("WORD_TO_BYTE", offset);
			break;
		case OpCode::WORD_TO_INT:
			SimpleInstruction("WORD_TO_INT", offset);
			break;
		case OpCode::INT_TO_WORD:
			SimpleInstruction("INT_TO_WORD", offset);
			break;
		case OpCode::BYTE_TO_FLOAT:
			SimpleInstruction("BYTE_TO_FLOAT", offset);
			break;
		case OpCode::FLOAT_TO_BYTE:
			SimpleInstruction("FLOAT_TO_BYTE", offset);
			break;
		case OpCode::WORD_TO_FLOAT:
			SimpleInstruction("WORD_TO_FLOAT", offset);
			break;
		case OpCode::FLOAT_TO_WORD:
			SimpleInstruction("FLOAT_TO_WORD", offset);
			break;
		case OpCode::LEFT_SHIFT:
			SimpleInstruction("LEFT_SHIFT", offset);
			break;
		case OpCode::RIGHT_SHIFT:
			SimpleInstruction("RIGHT_SHIFT", offset);
			break;
		case OpCode::LEFT_SHIFT_BYTE:
			SimpleInstruction("LEFT_SHIFT_BYTE", offset);
			break;
		case OpCode::RIGHT_SHIFT_BYTE:
			SimpleInstruction("RIGHT_SHIFT_BYTE", offset);
			break;
		case OpCode::LEFT_SHIFT_WORD:
			SimpleInstruction("LEFT_SHIFT_WORD", offset);
			break;
		case OpCode::RIGHT_SHIFT_WORD:
			SimpleInstruction("RIGHT_SHIFT_WORD", offset);
			break;
		case OpCode::BITWISE_AND:
			SimpleInstruction("BITWISE_AND", offset);
			break;
		case OpCode::BITWISE_OR:
			SimpleInstruction("BITWISE_OR", offset);
			break;
		case OpCode::BITWISE_XOR:
			SimpleInstruction("BITWISE_XOR", offset);
			break;
		case OpCode::BITWISE_NOT:
			SimpleInstruction("BITWISE_NOT", offset);
			break;
		case OpCode::ADD_FLOAT:
			SimpleInstruction("ADD_FLOAT", offset);
			break;
		case OpCode::SUBTRACT_FLOAT:
			SimpleInstruction("SUBTRACT_FLOAT", offset);
			break;
		case OpCode::MULTIPLY_FLOAT:
			SimpleInstruction("MULTIPLY_FLOAT", offset);
			break;
		case OpCode::DIVIDE_FLOAT:
			SimpleInstruction("DIVIDE_FLOAT", offset);
			break;
		case OpCode::MODULO_FLOAT:
			SimpleInstruction("MODULO_FLOAT", offset);
			break;
		case OpCode::ADD_INTEGER:
			SimpleInstruction("ADD_INTEGER", offset);
			break;
		case OpCode::SUBTRACT_INTEGER:
			SimpleInstruction("SUBTRACT_INTEGER", offset);
			break;
		case OpCode::MULTIPLY_INTEGER:
			SimpleInstruction("MULTIPLY_INTEGER", offset);
			break;
		case OpCode::DIVIDE_INTEGER:
			SimpleInstruction("DIVIDE_INTEGER", offset);
			break;
		case OpCode::MODULO_INTEGER:
			SimpleInstruction("MODULO_INTEGER", offset);
			break;
		case OpCode::ADD_BYTE:
			SimpleInstruction("ADD_BYTE", offset);
			break;
		case OpCode::SUBTRACT_BYTE:
			SimpleInstruction("SUBTRACT_BYTE", offset);
			break;
		case OpCode::MULTIPLY_BYTE:
			SimpleInstruction("MULTIPLY_BYTE", offset);
			break;
		case OpCode::DIVIDE_BYTE:
			SimpleInstruction("DIVIDE_BYTE", offset);
			break;
		case OpCode::MODULO_BYTE:
			SimpleInstruction("MODULO_BYTE", offset);
			break;
		case OpCode::ADD_WORD:
			SimpleInstruction("ADD_WORD", offset);
			break;
		case OpCode::SUBTRACT_WORD:
			SimpleInstruction("SUBTRACT_WORD", offset);
			break;
		case OpCode::MULTIPLY_WORD:
			SimpleInstruction("MULTIPLY_WORD", offset);
			break;
		case OpCode::DIVIDE_WORD:
			SimpleInstruction("DIVIDE_WORD", offset);
			break;
		case OpCode::MODULO_WORD:
			SimpleInstruction("MODULO_WORD", offset);
			break;
		case OpCode::CONCAT_ARRAY:
			SimpleInstruction("CONCAT_ARRAY", offset);
			break;
		case OpCode::CONCAT_TEXT:
			SimpleInstruction("CONCAT_TEXT", offset);
			break;
		case OpCode::APPEND_ARRAY:
			SimpleInstruction("APPEND_ARRAY", offset);
			break;
		case OpCode::PREPEND_ARRAY:
			SimpleInstruction("PREPEND_ARRAY", offset);
			break;
		case OpCode::APPEND_TEXT:
			SimpleInstruction("APPEND_TEXT", offset);
			break;
		case OpCode::PREPEND_TEXT:
			SimpleInstruction("PREPEND_TEXT", offset);
			break;
		case OpCode::ADD_ASSIGN_INT:
			SimpleInstruction("ADD_ASSIGN_INT", offset);
			break;
		case OpCode::ADD_ASSIGN_FLOAT:
			SimpleInstruction("ADD_ASSIGN_FLOAT", offset);
			break;
		case OpCode::SUB_ASSIGN_INT:
			SimpleInstruction("SUB_ASSIGN_INT", offset);
			break;
		case OpCode::SUB_ASSIGN_FLOAT:
			SimpleInstruction("SUB_ASSIGN_FLOAT", offset);
			break;
		case OpCode::MUL_ASSIGN_INT:
			SimpleInstruction("MUL_ASSIGN_INT", offset);
			break;
		case OpCode::MUL_ASSIGN_FLOAT:
			SimpleInstruction("MUL_ASSIGN_FLOAT", offset);
			break;
		case OpCode::DIV_ASSIGN_INT:
			SimpleInstruction("DIV_ASSIGN_INT", offset);
			break;
		case OpCode::DIV_ASSIGN_FLOAT:
			SimpleInstruction("DIV_ASSIGN_FLOAT", offset);
			break;
		case OpCode::MOD_ASSIGN_INT:
			SimpleInstruction("MOD_ASSIGN_INT", offset);
			break;
		case OpCode::MOD_ASSIGN_FLOAT:
			SimpleInstruction("MOD_ASSIGN_FLOAT", offset);
			break;
		case OpCode::AND_ASSIGN_INT:
			SimpleInstruction("AND_ASSIGN_INT", offset);
			break;
		case OpCode::OR_ASSIGN_INT:
			SimpleInstruction("OR_ASSIGN_INT", offset);
			break;
		case OpCode::XOR_ASSIGN_INT:
			SimpleInstruction("XOR_ASSIGN_INT", offset);
			break;
		case OpCode::LEFT_SHIFT_ASSIGN:
			SimpleInstruction("LEFT_SHIFT_ASSIGN", offset);
			break;
		case OpCode::RIGHT_SHIFT_ASSIGN:
			SimpleInstruction("RIGHT_SHIFT_ASSIGN", offset);
			break;
		case OpCode::EQUAL_FLOAT:
			SimpleInstruction("EQUAL_FLOAT", offset);
			break;
		case OpCode::NOT_EQUAL_FLOAT:
			SimpleInstruction("NOT_EQUAL_FLOAT", offset);
			break;
		case OpCode::GREATER_FLOAT:
			SimpleInstruction("GREATER_FLOAT", offset);
			break;
		case OpCode::GREATER_EQUAL_FLOAT:
			SimpleInstruction("GREATER_EQUAL_FLOAT", offset);
			break;
		case OpCode::LESS_FLOAT:
			SimpleInstruction("LESS_FLOAT", offset);
			break;
		case OpCode::LESS_EQUAL_FLOAT:
			SimpleInstruction("LESS_EQUAL_FLOAT", offset);
			break;
		case OpCode::EQUAL_INTEGER:
			SimpleInstruction("EQUAL_INTEGER", offset);
			break;
		case OpCode::NOT_EQUAL_INTEGER:
			SimpleInstruction("NOT_EQUAL_INTEGER", offset);
			break;
		case OpCode::GREATER_INTEGER:
			SimpleInstruction("GREATER_INTEGER", offset);
			break;
		case OpCode::GREATER_EQUAL_INTEGER:
			SimpleInstruction("GREATER_EQUAL_INTEGER", offset);
			break;
		case OpCode::LESS_INTEGER:
			SimpleInstruction("LESS_INTEGER", offset);
			break;
		case OpCode::LESS_EQUAL_INTEGER:
			SimpleInstruction("LESS_EQUAL_INTEGER", offset);
			break;
		case OpCode::EQUAL_BYTE:
			SimpleInstruction("EQUAL_BYTE", offset);
			break;
		case OpCode::NOT_EQUAL_BYTE:
			SimpleInstruction("NOT_EQUAL_BYTE", offset);
			break;
		case OpCode::GREATER_BYTE:
			SimpleInstruction("GREATER_BYTE", offset);
			break;
		case OpCode::GREATER_EQUAL_BYTE:
			SimpleInstruction("GREATER_EQUAL_BYTE", offset);
			break;
		case OpCode::LESS_BYTE:
			SimpleInstruction("LESS_BYTE", offset);
			break;
		case OpCode::LESS_EQUAL_BYTE:
			SimpleInstruction("LESS_EQUAL_BYTE", offset);
			break;
		case OpCode::EQUAL_WORD:
			SimpleInstruction("EQUAL_WORD", offset);
			break;
		case OpCode::NOT_EQUAL_WORD:
			SimpleInstruction("NOT_EQUAL_WORD", offset);
			break;
		case OpCode::GREATER_WORD:
			SimpleInstruction("GREATER_WORD", offset);
			break;
		case OpCode::GREATER_EQUAL_WORD:
			SimpleInstruction("GREATER_EQUAL_WORD", offset);
			break;
		case OpCode::LESS_WORD:
			SimpleInstruction("LESS_WORD", offset);
			break;
		case OpCode::LESS_EQUAL_WORD:
			SimpleInstruction("LESS_EQUAL_WORD", offset);
			break;
		case OpCode::EQUAL_TEXT:
			SimpleInstruction("EQUAL_TEXT", offset);
			break;
		case OpCode::NOT:
			SimpleInstruction("NOT", offset);
			break;
		case OpCode::NEGATE_FLOAT:
			SimpleInstruction("NEGATE_FLOAT", offset);
			break;
		case OpCode::NEGATE_INTEGER:
			SimpleInstruction("NEGATE_INTEGER", offset);
			break;
		case OpCode::JUMP_IF_FALSE:
			JumpInstruction("JUMP_IF_FALSE", 1, executable, proc_index, offset);
			break;
		case OpCode::JUMP_IF_TRUE:
			JumpInstruction("JUMP_IF_TRUE", 1, executable, proc_index, offset);
			break;
		case OpCode::JUMP:
			JumpInstruction("JUMP", 1, executable, proc_index, offset);
			break;
		case OpCode::JUMP_BACK:
			JumpInstruction("JUMP_BACK", -1, executable, proc_index, offset);
			break;
		case OpCode::IF_INTEGER_LESS:
			JumpInstruction("IF_INTEGER_LESS", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_INTEGER_LESS_EQUAL:
			JumpInstruction("IF_INTEGER_LESS_EQUAL", 1, executable, proc_index, offset);
				break;
		case OpCode::IF_INTEGER_GREATER:
			JumpInstruction("IF_INTEGER_GREATER", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_INTEGER_GREATER_EQUAL:
			JumpInstruction("IF_INTEGER_GREATER_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_INTEGER_EQUAL:
			JumpInstruction("IF_INTEGER_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_INTEGER_NOT_EQUAL:
			JumpInstruction("IF_INTEGER_NOT_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_FLOAT_LESS:
			JumpInstruction("IF_FLOAT_LESS", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_FLOAT_LESS_EQUAL:
			JumpInstruction("IF_FLOAT_LESS_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_FLOAT_GREATER:
			JumpInstruction("IF_FLOAT_GREATER", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_FLOAT_GREATER_EQUAL:
			JumpInstruction("IF_FLOAT_GREATER_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_FLOAT_EQUAL:
			JumpInstruction("IF_FLOAT_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::IF_FLOAT_NOT_EQUAL:
			JumpInstruction("IF_FLOAT_NOT_EQUAL", 1, executable, proc_index, offset);
			break;
		case OpCode::BREAK:
			JumpInstruction("BREAK", 1, executable, proc_index, offset);
			break;
		case OpCode::LOAD_TAG:
			SimpleInstruction("LOAD_TAG", offset);
			break;
		case OpCode::SET_TAG:
			SetTagInstruction("SET_TAG", executable, proc_index, offset);
			break;
		case OpCode::MATCH_JUMP_TABLE:
			MatchJumpTableInstruction("MATCH_JUMP_TABLE", executable, proc_index, offset);
			break;
		case OpCode::CALL_FOREIGN:
			CallInstruction("CALL_FOREIGN", executable, proc_index, offset);
			break;
		case OpCode::CALL_DEFINED:
			CallInstruction("CALL_DEFINED", executable, proc_index, offset);
			break;
		case OpCode::TAIL_CALL:
			CallInstruction("TAIL_CALL", executable, proc_index, offset);
			break;
		case OpCode::CONSTRUCT_STRUCT:
			DataInstruction("CONSTRUCT_STRUCT", executable, proc_index, offset);
			break;
		case OpCode::CONSTRUCT_UNION:
			DataInstruction("CONSTRUCT_UNION", executable, proc_index, offset);
			break;
		case OpCode::ALLOCATE_CLOSURE:
			AllocateClosureInstruction("ALLOCATE_CLOSURE", executable, proc_index, offset);
			break;
		case OpCode::CONSTRUCT_CLOSURE:
			ClosureCreateInstruction("CONSTRUCT_CLOSURE", executable, proc_index, offset);
			break;
		case OpCode::DEFINE_GLOBAL:
			GlobalVariableInstruction("DEFINE_GLOBAL", executable, proc_index, offset);
			break;
		case OpCode::GET_GLOBAL:
			GlobalVariableInstruction("GET_GLOBAL", executable, proc_index, offset);
			break;
		case OpCode::SET_GLOBAL:
			GlobalVariableInstruction("SET_GLOBAL", executable, proc_index, offset);
			break;
		case OpCode::GET_LOCAL:
			LocalOrCellVariableInstruction("GET_LOCAL", executable, proc_index, offset);
			break;
		case OpCode::SET_LOCAL:
			LocalOrCellVariableInstruction("SET_LOCAL", executable, proc_index, offset);
			break;
		case OpCode::GET_CELL:
			LocalOrCellVariableInstruction("GET_CELL", executable, proc_index, offset);
			break;
		case OpCode::SET_CELL:
			LocalOrCellVariableInstruction("SET_CELL", executable, proc_index, offset);
			break;
		case OpCode::GET_MEMBER:
			MemberInstruction("GET_MEMBER", executable, proc_index, offset);
			break;
		case OpCode::SET_MEMBER:
			MemberInstruction("SET_MEMBER", executable, proc_index, offset);
			break;
		case OpCode::POP:
			SimpleInstruction("POP", offset);
			break;
		case OpCode::DUP:
			SimpleInstruction("DUP", offset);
			break;
		case OpCode::POP_LOCAL_SCOPE:
			PopMultipleInstruction("POP_LOCAL_SCOPE", executable, proc_index, offset);
			break;
		case OpCode::POP_VALUES:
			PopMultipleInstruction("POP_VALUES", executable, proc_index, offset);
			break;
		case OpCode::POP_BLOCK_SCOPE:
			PopMultipleInstruction("POP_BLOCK_SCOPE", executable, proc_index, offset);
			break;
		case OpCode::POP_MATCH_SCOPE:
			PopMultipleInstruction("POP_MATCH_SCOPE", executable, proc_index, offset);
			break;
		case OpCode::RETURN:
			SimpleInstruction("RETURN", offset);
			break;
		case OpCode::HALT:
			SimpleInstruction("HALT", offset);
			break;
		case OpCode::PUSH_PLACEHOLDER:
			SimpleInstruction("PUSH_PLACEHOLDER", offset);
			break;
		case OpCode::UPDATE_PLACEHOLDER:
			SimpleInstruction("UPDATE_PLACEHOLDER", offset);
			break;
		default:
#ifdef _MSC_VER
			__assume(0);
#else
			__builtin_unreachable();
#endif
		}
	}
}
#endif