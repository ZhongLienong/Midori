#include <format>

#include "CodeGenerator.h"
#include "Common/Constant/Constant.h"

using namespace std::string_literals;

void CodeGenerator::EmitByte(OpCode byte, int line)
{
	m_last_opcode = byte;
	m_procedures[m_current_procedure_index].AddByteCode(byte, line);
}

void CodeGenerator::AddError(std::string&& error)
{
	m_errors.append(error);
	m_errors.push_back('\n');
}

void CodeGenerator::PopByte(int line)
{
	m_procedures[m_current_procedure_index].PopByteCode(line);
}

void CodeGenerator::EmitTextConstant(std::string_view data, int line)
{
	if (m_string_pool_index + 1 >= MAX_SIZE_OP_CONSTANT)
	{
		AddError(MidoriError::GenerateCodeGeneratorError("Too many text constants.", line));
		return;
	}

	m_string_pool.emplace_back(data);
	EmitByte(OpCode::LOAD_STRING, line);
	EmitByte(static_cast<OpCode>(m_string_pool_index++), line);
}

void CodeGenerator::EmitTwoBytes(int byte1, int byte2, int line)
{
	EmitByte(static_cast<OpCode>(byte1 & 0xff), line);
	EmitByte(static_cast<OpCode>(byte2 & 0xff), line);
}

void CodeGenerator::EmitThreeBytes(int byte1, int byte2, int byte3, int line)
{
	EmitByte(static_cast<OpCode>(byte1 & 0xff), line);
	EmitByte(static_cast<OpCode>(byte2 & 0xff), line);
	EmitByte(static_cast<OpCode>(byte3 & 0xff), line);
}

void CodeGenerator::EmitNumericConstant(MidoriInteger val, int line, bool is_integer)
{
	int byte1 = val & 0xff;
	int byte2 = (val >> 8) & 0xff;
	int byte3 = (val >> 16) & 0xff;
	int byte4 = (val >> 24) & 0xff;
	int byte5 = (val >> 32) & 0xff;
	int byte6 = (val >> 40) & 0xff;
	int byte7 = (val >> 48) & 0xff;
	int byte8 = (val >> 56) & 0xff;

	if (is_integer)
	{
		EmitByte(OpCode::INTEGER_CONSTANT, line);
	}
	else
	{
		EmitByte(OpCode::FRACTION_CONSTANT, line);
	}
	EmitByte(static_cast<OpCode>(byte1), line);
	EmitByte(static_cast<OpCode>(byte2), line);
	EmitByte(static_cast<OpCode>(byte3), line);
	EmitByte(static_cast<OpCode>(byte4), line);
	EmitByte(static_cast<OpCode>(byte5), line);
	EmitByte(static_cast<OpCode>(byte6), line);
	EmitByte(static_cast<OpCode>(byte7), line);
	EmitByte(static_cast<OpCode>(byte8), line);
}

void CodeGenerator::EmitFractionConstant(MidoriFraction value, int line)
{
	MidoriInteger reinterpreted_int = *reinterpret_cast<MidoriInteger*>(&value);
	EmitNumericConstant(reinterpreted_int, line, false);
}

void CodeGenerator::EmitIntegerConstant(MidoriInteger value, int line)
{
	EmitNumericConstant(value, line, true);
}

void CodeGenerator::EmitVariable(int variable_index, OpCode op, int line)
{
	if (variable_index <= MAX_LOCAL_VARIABLES)
	{
		EmitByte(op, line);
		EmitByte(static_cast<OpCode>(variable_index), line);
		return;
	}

	AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many variables (max {}).", MAX_LOCAL_VARIABLES + 1), line));
}

int CodeGenerator::EmitJump(OpCode op, int line)
{
	EmitByte(op, line);
	EmitByte(static_cast<OpCode>(0xff), line);
	EmitByte(static_cast<OpCode>(0xff), line);
	return m_procedures[m_current_procedure_index].GetByteCodeSize() - 2;
}

void CodeGenerator::PatchJump(int offset, int line)
{
	int jump = m_procedures[m_current_procedure_index].GetByteCodeSize() - offset - 2;
	if (jump > MAX_JUMP_SIZE)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too much code to jump over (max {}).", MAX_JUMP_SIZE + 1), line));
		return;
	}

	m_procedures[m_current_procedure_index].SetByteCode(offset, static_cast<OpCode>(jump & 0xff));
	m_procedures[m_current_procedure_index].SetByteCode(offset + 1, static_cast<OpCode>((jump >> 8) & 0xff));
}

void CodeGenerator::EmitLoop(int loop_start, int line)
{
	EmitByte(OpCode::JUMP_BACK, line);

	int offset = m_procedures[m_current_procedure_index].GetByteCodeSize() - loop_start + 2;
	if (offset > MAX_JUMP_SIZE)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("For body too large (max {}).", MAX_JUMP_SIZE + 1), line));
		return;
	}

	EmitByte(static_cast<OpCode>(offset & 0xff), line);
	EmitByte(static_cast<OpCode>((offset >> 8) & 0xff), line);
}

void CodeGenerator::BeginLoop(int loop_start)
{
	m_loop_contexts.emplace(std::vector<int>(), loop_start);
}

void CodeGenerator::EndLoop(int line)
{
	LoopContext loop = m_loop_contexts.top();
	m_loop_contexts.pop();
	std::ranges::for_each
	(
		loop.m_break_positions,
		[line, this](int break_position){ PatchJump(break_position, line); }
	);
}

MidoriResult::CodeGeneratorResult CodeGenerator::GenerateCode(MidoriProgramTree&& program_tree)
{
	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& statement){ std::visit([this](auto&& arg){ (*this)(arg); }, *statement); }
	);

	if (!m_main_function_ctx.has_value())
	{
		AddError(MidoriError::GenerateCodeGeneratorError("Program entry (\"main\") not found.", 0));
	}

	if (!m_errors.empty())
	{
		return std::unexpected<std::string>(std::move(m_errors));
	}

	// invoke the program entry (main)
	const CodeGenerator::MainProcedureContext& main_module_ctx = m_main_function_ctx.value();
	int line = main_module_ctx.m_main_procedure_line;

	m_current_procedure_index = main_module_ctx.m_main_procedure_index;
	EmitVariable(main_module_ctx.m_main_procedure_global_table_index, OpCode::GET_GLOBAL, line);
	EmitByte(OpCode::CALL_DEFINED, line);
	EmitByte(static_cast<OpCode>(0), line);
	EmitByte(OpCode::HALT, line);

#ifdef DEBUG
	m_executable.AttachProcedureNames(std::move(m_procedure_names));
#endif
	m_executable.AttachProcedures(std::move(m_procedures));
	m_executable.AddStringPool(std::move(m_string_pool));

	return m_executable;
}

void CodeGenerator::operator()(Block& block)
{
	std::ranges::for_each
	(
		block.m_stmts,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, *statement);
		}
	);
	if (m_procedures[m_current_procedure_index].ReadByteCode(m_procedures[m_current_procedure_index].GetByteCodeSize() - 1) == OpCode::RETURN)
	{
		return;
	}

	while (block.m_local_count > 0)
	{
		int count_to_pop = std::min(block.m_local_count, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_SCOPE, block.m_right_brace.m_line);
		EmitByte(static_cast<OpCode>(count_to_pop), block.m_right_brace.m_line);
		block.m_local_count -= count_to_pop;
	}
}

void CodeGenerator::operator()(Simple& simple)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **simple.m_expr);
	EmitByte(OpCode::POP, simple.m_semicolon.m_line);
}

void CodeGenerator::operator()(Define& def)
{
	int line = def.m_name.m_line;
	bool is_global = !def.m_local_index.has_value();
	std::optional<int> index = std::nullopt;
	if (is_global)
	{
		MidoriText variable_name(def.m_name.m_lexeme.c_str());
		index.emplace(m_executable.AddGlobalVariable(std::move(variable_name)));
		m_global_variables[def.m_name.m_lexeme] = index.value();
	}

	std::visit([this](auto&& arg){ (*this)(arg); }, **def.m_value);

	if (is_global)
	{
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
		if (def.m_name.m_lexeme == "main"s)
		{
			if (m_main_function_ctx.has_value())
			{
				AddError(MidoriError::GenerateCodeGeneratorError("Cannot re-define program entry (\"main\").", line));
				return;
			}
			else
			{
				constexpr int main_module_arity = 0;
				m_main_function_ctx.emplace(static_cast<int>(m_current_procedure_index), index.value(), main_module_arity, line);
			}
		}
	}
}

void CodeGenerator::operator()(If& if_stmt)
{
	int line = if_stmt.m_if_keyword.m_line;
	std::visit([this](auto&& arg){ (*this)(arg); }, **if_stmt.m_condition);

	if (if_stmt.m_condition_operand_type == MidoriExpression::ConditionOperandType::INTEGER || if_stmt.m_condition_operand_type == MidoriExpression::ConditionOperandType::FRACTION)
	{
		if (if_stmt.m_else_branch.has_value())
		{
			EmitNumericConditionalJump<std::unique_ptr<MidoriStatement>&>(if_stmt.m_condition_operand_type, if_stmt.m_true_branch, if_stmt.m_else_branch.value(), line);
		}
		else
		{
			std::unique_ptr<MidoriStatement> null_else_branch = nullptr;
			EmitNumericConditionalJump<std::unique_ptr<MidoriStatement>&>(if_stmt.m_condition_operand_type, if_stmt.m_true_branch, null_else_branch, line);
		}
	}
	else
	{
		int true_jump = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg){ (*this)(arg); }, *if_stmt.m_true_branch);

		int else_jump = EmitJump(OpCode::JUMP, line);
		PatchJump(true_jump, line);
		EmitByte(OpCode::POP, line);

		if (if_stmt.m_else_branch.has_value())
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, *if_stmt.m_else_branch.value());
		}

		PatchJump(else_jump, line);
	}
}

void CodeGenerator::operator()(While& while_stmt)
{
	int loop_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
	BeginLoop(loop_start);

	std::visit([this](auto&& arg){ (*this)(arg); }, **while_stmt.m_condition);

	int line = while_stmt.m_loop_keyword.m_line;
	int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
	EmitByte(OpCode::POP, line);

	std::visit([this](auto&& arg){ (*this)(arg); }, *while_stmt.m_body);

	EmitLoop(loop_start, line);
	PatchJump(jump_if_false, line);
	EmitByte(OpCode::POP, line);

	EndLoop(line);
}

void CodeGenerator::operator()(For& for_stmt)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, *for_stmt.m_condition_intializer);

	int loop_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
	int line = for_stmt.m_loop_keyword.m_line;

	int exit_jump = -1;
	std::visit([this](auto&& arg){ (*this)(arg); }, **for_stmt.m_condition);
	exit_jump = EmitJump(OpCode::JUMP_IF_FALSE, line);
	EmitByte(OpCode::POP, line);

	int body_jump = EmitJump(OpCode::JUMP, line);
	int incrementer_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
	std::visit([this](auto&& arg){ (*this)(arg); }, *for_stmt.m_condition_incrementer);
	EmitLoop(loop_start, line);
	loop_start = incrementer_start;
	PatchJump(body_jump, line);

	BeginLoop(loop_start);
	std::visit([this](auto&& arg){ (*this)(arg); }, *for_stmt.m_body);

	EmitLoop(loop_start, line);
	if (exit_jump != -1)
	{
		PatchJump(exit_jump, line);
		EmitByte(OpCode::POP, line);
	}

	while (for_stmt.m_control_block_local_count > 0)
	{
		int count_to_pop = std::min(for_stmt.m_control_block_local_count, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_SCOPE, line);
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		for_stmt.m_control_block_local_count -= count_to_pop;
	}
	EndLoop(line);
}

void CodeGenerator::operator()(Break& break_stmt)
{
	int line = break_stmt.m_keyword.m_line;

	while (break_stmt.m_number_to_pop > 0)
	{
		int count_to_pop = std::min(break_stmt.m_number_to_pop, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_SCOPE, line);
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		break_stmt.m_number_to_pop -= count_to_pop;
	}

	m_loop_contexts.top().m_break_positions.emplace_back(EmitJump(OpCode::JUMP, line));
}

void CodeGenerator::operator()(Continue& continue_stmt)
{
	int line = continue_stmt.m_keyword.m_line;

	while (continue_stmt.m_number_to_pop > 0)
	{
		int count_to_pop = std::min(continue_stmt.m_number_to_pop, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_MULTIPLE, line);
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		continue_stmt.m_number_to_pop -= count_to_pop;
	}

	EmitLoop(m_loop_contexts.top().m_loop_start, line);
}

void CodeGenerator::operator()(Return& return_stmt)
{
	int line = return_stmt.m_keyword.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **return_stmt.m_value);

	EmitByte(OpCode::RETURN, line);
}

void CodeGenerator::operator()(Foreign& foreign)
{
	int line = foreign.m_function_name.m_line;

	const MidoriType::FunctionType& type = foreign.m_type->GetType<MidoriType::FunctionType>();
	if (!(type.m_return_type->IsType<MidoriType::IntegerType>() || type.m_return_type->IsType<MidoriType::FractionType>() || type.m_return_type->IsType<MidoriType::BoolType>() || type.m_return_type->IsType<MidoriType::UnitType>()))
	{
		AddError(MidoriError::GenerateCodeGeneratorError("Unsupported return type for foreign function.", line));
		return;
	}

	bool is_global = !foreign.m_local_index.has_value();
	std::optional<int> index = std::nullopt;
	if (is_global)
	{
		MidoriText foreign_function_name(foreign.m_function_name.m_lexeme.c_str());
		index.emplace(m_executable.AddGlobalVariable(std::move(foreign_function_name)));
		m_global_variables[foreign.m_function_name.m_lexeme] = index.value();
	}

	EmitTextConstant(foreign.m_foreign_name, line);

	if (is_global)
	{
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
	}
}

void CodeGenerator::operator()(Struct&)
{
	return;
}

void CodeGenerator::operator()(Union&)
{
	return;
}

void CodeGenerator::operator()(Switch& switch_stmt)
{
	int line = switch_stmt.m_switch_keyword.m_line;
	std::visit([this](auto&& arg){ (*this)(arg); }, **switch_stmt.m_arg_expr);
	EmitByte(OpCode::LOAD_TAG, line);

	// for each case
	// compare tag
	std::vector<int> jumps;
	for (Switch::Case& switch_case : switch_stmt.m_cases)
	{
		if (Switch::IsMemberCase(switch_case))
		{
			const Switch::MemberCase& member_case = Switch::GetMemberCase(switch_case);

			EmitByte(OpCode::DUP, line);
			MidoriInteger member_tag = static_cast<MidoriInteger>(member_case.m_tag);
			EmitIntegerConstant(member_tag, line);
			EmitByte(OpCode::EQUAL_INTEGER, line);
			int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
			EmitByte(OpCode::POP, line); // pop tag
			EmitByte(OpCode::POP, line); // pop comp result

			std::visit([this](auto&& arg){ (*this)(arg); }, *member_case.m_stmt);

			int num_to_pop = static_cast<int>(member_case.m_binding_names.size());
			while (num_to_pop > 0)
			{
				int count_to_pop = std::min(num_to_pop, static_cast<int>(UINT8_MAX));
				EmitByte(OpCode::POP_MULTIPLE, line);
				EmitByte(static_cast<OpCode>(count_to_pop), line);
				num_to_pop -= count_to_pop;
			}
			jumps.emplace_back(EmitJump(OpCode::JUMP, line));

			PatchJump(jump_if_false, line);
			EmitByte(OpCode::POP, line); // pop comp result
		}
		else
		{
			const Switch::DefaultCase& default_case = Switch::GetDefaultCase(switch_case);

			std::visit([this](auto&& arg){ (*this)(arg); }, *default_case.m_stmt);

			EmitByte(OpCode::POP, line);
			jumps.emplace_back(EmitJump(OpCode::JUMP, line));
			break;
		}
	}

	std::ranges::for_each
	(
		jumps,
		[this, line](int jump_addr)
		{
			PatchJump(jump_addr, line);
		}
	);
}

void CodeGenerator::operator()(Namespace& namespace_stmt)
{
	std::ranges::for_each
	(
		namespace_stmt.m_stmts,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, *stmt);
		}
	);
}

void CodeGenerator::operator()(MidoriExpression::As& as)
{
	int line = as.m_as_keyword.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **as.m_expr);

	std::shared_ptr<MidoriType> from_type = as.m_from_type.lock();
	const std::shared_ptr<MidoriType>& target_type = as.m_to_type;

	if (target_type->IsType<MidoriType::BoolType>())
	{
		// Do nothing
	}
	else if (target_type->IsType<MidoriType::FractionType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_FRAC, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			EmitByte(OpCode::TEXT_TO_FRAC, line);
		}
		else if (from_type->IsType<MidoriType::FractionType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorError("Unsupported 'cast to frac' instruction.", line));
		}
	}
	else if (target_type->IsType<MidoriType::IntegerType>())
	{
		if (from_type->IsType<MidoriType::FractionType>())
		{
			EmitByte(OpCode::FRAC_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			EmitByte(OpCode::TEXT_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::IntegerType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorError("Unsupported 'cast to int' instruction.", line));
		}
	}
	else if (target_type->IsType<MidoriType::UnitType>())
	{
		EmitByte(OpCode::OP_UNIT, line);
	}
	else if (target_type->IsType<MidoriType::TextType>())
	{
		if (from_type->IsType<MidoriType::FractionType>())
		{
			EmitByte(OpCode::FRAC_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorError("Unsupported 'cast to text' instruction.", line));
		}
	}
	else
	{
		// TODO: implement custom cast
		AddError(MidoriError::GenerateCodeGeneratorError("Unsupported type casting instruction.", line));
	}
}

void CodeGenerator::operator()(MidoriExpression::Binary& binary)
{
	int line = binary.m_op.m_line;
	std::visit([this](auto&& arg){ (*this)(arg); }, **binary.m_left);
	std::visit([this](auto&& arg){ (*this)(arg); }, **binary.m_right);
	const std::shared_ptr<MidoriType>& operand_type = binary.m_left->GetType();

	switch (binary.m_op.m_token_name)
	{
	case Token::Name::SINGLE_PLUS:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::ADD_FRACTION, line) : EmitByte(OpCode::ADD_INTEGER, line);
		break;
	case Token::Name::DOUBLE_PLUS:
		operand_type->IsType<MidoriType::TextType>() ? EmitByte(OpCode::CONCAT_TEXT, line) : EmitByte(OpCode::CONCAT_ARRAY, line);
		break;
	case Token::Name::SINGLE_MINUS:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::SUBTRACT_FRACTION, line) : EmitByte(OpCode::SUBTRACT_INTEGER, line);
		break;
	case Token::Name::STAR:
		operand_type->IsType<MidoriType::FractionType>()
			? EmitByte(OpCode::MULTIPLY_FRACTION, line) 
			: operand_type->IsType<MidoriType::IntegerType>()
			? EmitByte(OpCode::MULTIPLY_INTEGER, line)
			: EmitByte(OpCode::DUP_ARRAY, line);
		break;
	case Token::Name::SLASH:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::DIVIDE_FRACTION, line) : EmitByte(OpCode::DIVIDE_INTEGER, line);
		break;
	case Token::Name::PERCENT:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::MODULO_FRACTION, line) : EmitByte(OpCode::MODULO_INTEGER, line);
		break;
	case Token::Name::LEFT_SHIFT:
		EmitByte(OpCode::LEFT_SHIFT, line);
		break;
	case Token::Name::RIGHT_SHIFT:
		EmitByte(OpCode::RIGHT_SHIFT, line);
		break;
	case Token::Name::LEFT_ANGLE:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::LESS_FRACTION, line) : EmitByte(OpCode::LESS_INTEGER, line);
		break;
	case Token::Name::LESS_EQUAL:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::LESS_EQUAL_FRACTION, line) : EmitByte(OpCode::LESS_EQUAL_INTEGER, line);
		break;
	case Token::Name::RIGHT_ANGLE:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::GREATER_FRACTION, line) : EmitByte(OpCode::GREATER_INTEGER, line);
		break;
	case Token::Name::GREATER_EQUAL:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::GREATER_EQUAL_FRACTION, line) : EmitByte(OpCode::GREATER_EQUAL_INTEGER, line);
		break;
	case Token::Name::BANG_EQUAL:
		operand_type->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::NOT_EQUAL_FRACTION, line) : EmitByte(OpCode::NOT_EQUAL_INTEGER, line);
		break;
	case Token::Name::DOUBLE_EQUAL:
		operand_type->IsType<MidoriType::FractionType>()
			? EmitByte(OpCode::EQUAL_FRACTION, line)
			: operand_type->IsType<MidoriType::IntegerType>()
			? EmitByte(OpCode::EQUAL_INTEGER, line)
			: EmitByte(OpCode::EQUAL_TEXT, line);
		break;
	case Token::Name::SINGLE_AMPERSAND:
		EmitByte(OpCode::BITWISE_AND, line);
		break;
	case Token::Name::SINGLE_BAR:
		EmitByte(OpCode::BITWISE_OR, line);
		break;
	case Token::Name::CARET:
		EmitByte(OpCode::BITWISE_XOR, line);
		break;
	case Token::Name::DOUBLE_BAR:
	{
		std::visit([this](auto&& arg){ (*this)(arg); }, **binary.m_left);
		int jump_if_true = EmitJump(OpCode::JUMP_IF_TRUE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg){ (*this)(arg); }, **binary.m_right);
		PatchJump(jump_if_true, line);
		break;
	}
	case Token::Name::DOUBLE_AMPERSAND:
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_left);
		int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_right);
		PatchJump(jump_if_false, line);
		break;
	}
	default:
#ifdef _MSC_VER
		__assume(0);
#else
		__builtin_unreachable();
#endif
	}
	return;
}

void CodeGenerator::operator()(MidoriExpression::Group& group)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **group.m_expr_in);
}

void CodeGenerator::operator()(MidoriExpression::UnaryPrefix& unary)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **unary.m_expr);

	switch (unary.m_op.m_token_name)
	{
	case Token::Name::SINGLE_MINUS:
		unary.m_type_data->IsType<MidoriType::FractionType>() ? EmitByte(OpCode::NEGATE_FRACTION, unary.m_op.m_line) : EmitByte(OpCode::NEGATE_INTEGER, unary.m_op.m_line);
		break;
	case Token::Name::SINGLE_PLUS:
		break;
	case Token::Name::BANG:
		EmitByte(OpCode::NOT, unary.m_op.m_line);
		break;
	case Token::Name::TILDE:
		EmitByte(OpCode::BITWISE_NOT, unary.m_op.m_line);
		break;
	default:
		return;
	}

	return;
}

void CodeGenerator::operator()(MidoriExpression::UnarySuffix&)
{
	// TODO: no suffix operators at the moment
	return;
}

void CodeGenerator::operator()(MidoriExpression::Call& call)
{
	int line = call.m_paren.m_line;
	int arity = static_cast<int>(call.m_arguments.size());
	if (arity > MAX_FUNCTION_ARITY)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many arguments (max {}).", MAX_FUNCTION_ARITY + 1), call.m_paren.m_line));
		return;
	}

	std::ranges::for_each
	(
		call.m_arguments,
		[this](std::unique_ptr<MidoriExpression>& param)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **param);
		}
	);
	std::visit([this](auto&& arg){ (*this)(arg); }, **call.m_callee);

	if (call.m_is_foreign)
	{
		EmitByte(OpCode::CALL_FOREIGN, line);
	}
	else
	{
		EmitByte(OpCode::CALL_DEFINED, line);
	}

	EmitByte(static_cast<OpCode>(arity), line);
}

void CodeGenerator::operator()(MidoriExpression::Get& get)
{
	int line = get.m_member_name.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **get.m_struct);
	EmitByte(OpCode::GET_MEMBER, line);
	EmitByte(static_cast<OpCode>(get.m_index), line);
}

void CodeGenerator::operator()(MidoriExpression::Set& set)
{
	int line = set.m_member_name.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **set.m_struct);
	std::visit([this](auto&& arg){ (*this)(arg); }, **set.m_value);
	EmitByte(OpCode::SET_MEMBER, line);
	EmitByte(static_cast<OpCode>(set.m_index), line);
}

void CodeGenerator::operator()(MidoriExpression::BoundedName& variable)
{
	std::visit
	(
		[&variable, this](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;
			int line = variable.m_name.m_line;

			if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Local>)
			{
				EmitVariable(arg.m_index, OpCode::GET_LOCAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Global>)
			{
				EmitVariable(m_global_variables[variable.m_name.m_lexeme], OpCode::GET_GLOBAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Cell>)
			{
				EmitVariable(arg.m_index, OpCode::GET_CELL, line);
			}
			else
			{
				AddError(MidoriError::GenerateCodeGeneratorError("Bad BoundedName MidoriExpression.", line));
				return;
			}
		}, 
		variable.m_name_ctx
	);
}

void CodeGenerator::operator()(MidoriExpression::Bind& bind)
{
	int line = bind.m_name.m_line;
	std::visit([this](auto&& arg){ (*this)(arg); }, **bind.m_value);

	std::visit([&bind, line, this](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Local>)
			{
				EmitVariable(arg.m_index, OpCode::SET_LOCAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Global>)
			{
				EmitVariable(m_global_variables[bind.m_name.m_lexeme], OpCode::SET_GLOBAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Cell>)
			{
				EmitVariable(arg.m_index, OpCode::SET_CELL, line);
			}
			else
			{
				AddError(MidoriError::GenerateCodeGeneratorError("Bad Bind MidoriExpression.", line));
				return;
			}
		}, bind.m_name_ctx);
}

void CodeGenerator::operator()(MidoriExpression::TextLiteral& text)
{
	EmitTextConstant(text.m_token.m_lexeme, text.m_token.m_line);
}

void CodeGenerator::operator()(MidoriExpression::BoolLiteral& bool_expr)
{
	int line = bool_expr.m_token.m_line;
	EmitByte(bool_expr.m_token.m_lexeme == "true"s ? OpCode::OP_TRUE : OpCode::OP_FALSE, line);
}

void CodeGenerator::operator()(MidoriExpression::FractionLiteral& fraction)
{
	int line = fraction.m_token.m_line;
	EmitFractionConstant(std::stod(fraction.m_token.m_lexeme), fraction.m_token.m_line);
}

void CodeGenerator::operator()(MidoriExpression::IntegerLiteral& integer)
{
	int line = integer.m_token.m_line;
	EmitIntegerConstant(std::stoll(integer.m_token.m_lexeme), integer.m_token.m_line);
}

void CodeGenerator::operator()(MidoriExpression::UnitLiteral& unit)
{
	int line = unit.m_token.m_line;
	EmitByte(OpCode::OP_UNIT, unit.m_token.m_line);
}

void CodeGenerator::operator()(MidoriExpression::Function& function)
{
	int line = function.m_function_keyword.m_line;
	int arity = static_cast<int>(function.m_params.size());
	int captured_count = static_cast<int>(function.m_captured_count);
	if (arity > MAX_FUNCTION_ARITY)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many arguments (max {}).", MAX_FUNCTION_ARITY + 1), line));
		return;
	}
	if (captured_count > MAX_CAPTURED_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many captured variables (max {}).", MAX_CAPTURED_COUNT + 1), line));
		return;
	}

	size_t prev_index = m_current_procedure_index;
	m_current_procedure_index = m_procedures.size();
	m_procedures.emplace_back();
	std::visit([this](auto&& arg){ (*this)(arg); }, *function.m_body);

	size_t closure_proc_index = m_current_procedure_index;
#ifdef DEBUG
	std::string closure_line = "Function at line: " + std::to_string(line) + "(index: " + std::to_string(closure_proc_index) + ")";
	m_procedure_names.emplace_back(closure_line.c_str());
#endif

	m_current_procedure_index = prev_index;

	if (m_current_procedure_index > MAX_FUNCTION_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many functions (max {}).", MAX_FUNCTION_COUNT + 1), line));
		return;
	}

	EmitByte(OpCode::ALLOCATE_CLOSURE, line);
	EmitByte(static_cast<OpCode>(closure_proc_index), line);

	EmitByte(OpCode::CONSTRUCT_CLOSURE, line);
	EmitByte(static_cast<OpCode>(function.m_captured_count), line);
}

void CodeGenerator::operator()(MidoriExpression::Construct& construct)
{
	int line = construct.m_data_name.m_line;
	OpCode size = static_cast<OpCode>(construct.m_params.size());
	bool is_struct = std::holds_alternative<MidoriExpression::Construct::Struct>(construct.m_construct_ctx);

	std::ranges::for_each
	(
		construct.m_params,
		[this](std::unique_ptr<MidoriExpression>& param)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **param);
		}
	);

	if (is_struct)
	{
		EmitByte(OpCode::CONSTRUCT_STRUCT, line);
	}
	else
	{
		EmitByte(OpCode::CONSTRUCT_UNION, line);
	}
	EmitByte(size, line);

	if (!is_struct)
	{
		int tag = std::get<MidoriExpression::Construct::Union>(construct.m_construct_ctx).m_index;

		if (tag > MAX_UNION_TAG)
		{
			AddError(MidoriError::GenerateCodeGeneratorError(std::format("Union tag too large (max {}).", MAX_UNION_TAG + 1), line));
			return;
		}

		EmitByte(OpCode::SET_TAG, line);
		EmitByte(static_cast<OpCode>(tag), line);
	}
}

void CodeGenerator::operator()(MidoriExpression::Array& array)
{
	int line = array.m_op.m_line;

	int length = static_cast<int>(array.m_elems.size());
	if (length > MAX_ARRAY_SIZE)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many array elements (max {}).", MAX_ARRAY_SIZE + 1), line));
		return;
	}

	std::ranges::for_each
	(
		array.m_elems,
		[this](std::unique_ptr<MidoriExpression>& elem)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **elem);
		}
	);
	EmitByte(OpCode::CREATE_ARRAY, line);
	EmitThreeBytes(length, length >> 8, length >> 16, line);
}

void CodeGenerator::operator()(MidoriExpression::ArrayGet& array_get)
{
	int line = array_get.m_op.m_line;

	if (array_get.m_indices.size() > MAX_NESTED_ARRAY_INDEX)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many array indices (max {}).", MAX_NESTED_ARRAY_INDEX + 1), line));
		return;
	}

	std::visit([this](auto&& arg){ (*this)(arg); }, **array_get.m_arr_var);

	std::ranges::for_each
	(
		array_get.m_indices,
		[this](std::unique_ptr<MidoriExpression>& index)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **index);
		}
	);

	EmitByte(OpCode::GET_ARRAY, line);
	EmitByte(static_cast<OpCode>(array_get.m_indices.size()), line);
}

void CodeGenerator::operator()(MidoriExpression::ArraySet& array_set)
{
	int line = array_set.m_op.m_line;

	if (array_set.m_indices.size() > MAX_NESTED_ARRAY_INDEX)
	{
		AddError(MidoriError::GenerateCodeGeneratorError(std::format("Too many array indices (max {}).", MAX_NESTED_ARRAY_INDEX + 1), line));
		return;
	}

	std::visit([this](auto&& arg){ (*this)(arg); }, **array_set.m_arr_var);

	std::ranges::for_each
	(
		array_set.m_indices,
		[this](std::unique_ptr<MidoriExpression>& index)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **index);
		}
	);

	std::visit([this](auto&& arg){ (*this)(arg); }, **array_set.m_value);

	EmitByte(OpCode::SET_ARRAY, line);
	EmitByte(static_cast<OpCode>(array_set.m_indices.size()), line);
}

void CodeGenerator::operator()(MidoriExpression::Ternary& ternary)
{
	int line = ternary.m_colon.m_line;
	std::visit([this](auto&& arg){ (*this)(arg); }, **ternary.m_condition);

	if (ternary.m_condition_operand_type == MidoriExpression::ConditionOperandType::INTEGER || ternary.m_condition_operand_type == MidoriExpression::ConditionOperandType::FRACTION)
	{
		EmitNumericConditionalJump<std::unique_ptr<MidoriExpression>&>(ternary.m_condition_operand_type, ternary.m_true_branch, ternary.m_else_branch, line);
	}
	else
	{
		int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg){ (*this)(arg); }, **ternary.m_true_branch);
		int jump = EmitJump(OpCode::JUMP, line);
		PatchJump(jump_if_false, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg) { (*this)(arg); }, **ternary.m_else_branch);
		PatchJump(jump, line);
	}
}