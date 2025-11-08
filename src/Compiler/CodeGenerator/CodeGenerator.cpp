#include <format>
#include <fstream>
#include <sstream>

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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Too many text constants", line, m_file_name, m_source_lines));
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
		EmitByte(OpCode::FLOAT_CONSTANT, line);
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

void CodeGenerator::EmitFloatConstant(MidoriFloat value, int line)
{
	MidoriInteger reinterpreted_int = *reinterpret_cast<MidoriInteger*>(&value);
	EmitNumericConstant(reinterpreted_int, line, false);
}

void CodeGenerator::EmitIntegerConstant(MidoriInteger value, int line)
{
	// Optimize common small integer constants
	switch (value)
	{
	case -1:
		EmitByte(OpCode::INT_MINUS_1, line);
		return;
	case 0:
		EmitByte(OpCode::INT_0, line);
		return;
	case 1:
		EmitByte(OpCode::INT_1, line);
		return;
	case 2:
		EmitByte(OpCode::INT_2, line);
		return;
	case 3:
		EmitByte(OpCode::INT_3, line);
		return;
	case 4:
		EmitByte(OpCode::INT_4, line);
		return;
	case 5:
		EmitByte(OpCode::INT_5, line);
		return;
	case 10:
		EmitByte(OpCode::INT_10, line);
		return;
	default:
		EmitNumericConstant(value, line, true);
	}
}

void CodeGenerator::EmitVariable(int variable_index, OpCode op, int line)
{
	if (variable_index <= MAX_LOCAL_VARIABLES)
	{
		EmitByte(op, line);
		EmitByte(static_cast<OpCode>(variable_index), line);
		return;
	}

	AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many variables (max {})", MAX_LOCAL_VARIABLES + 1), line, m_file_name, m_source_lines));
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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too much code to jump over (max {})", MAX_JUMP_SIZE + 1), line, m_file_name, m_source_lines));
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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Loop body too large (max {})", MAX_JUMP_SIZE + 1), line, m_file_name, m_source_lines));
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

CodeGenerator::CodeGenerator(MidoriProgramTree&& program_tree, std::string_view file_name, const std::vector<std::string>& source_lines)
	: m_program_tree(std::move(program_tree)),
	m_file_name(file_name),
	m_source_lines(source_lines)
{
}

MidoriResult::CodeGeneratorResult CodeGenerator::GenerateCode()
{
	std::ranges::for_each
	(
		m_program_tree,
		[this](std::unique_ptr<MidoriStatement>& statement){ std::visit([this](auto&& arg){ (*this)(arg); }, **statement); }
	);

	if (!m_errors.empty())
	{
		return std::unexpected<std::string>(std::move(m_errors));
	}

	EmitByte(OpCode::HALT, 0);

	m_executable.AttachProcedureNames(std::move(m_procedure_names));
	m_executable.AttachProcedures(std::move(m_procedures));
	m_executable.AddStringPool(std::move(m_string_pool));
	m_executable.SetFileName(std::string(m_file_name));

	return m_executable;
}

void CodeGenerator::operator()(MidoriStatement::Simple& simple)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **simple.m_expr);
	EmitByte(OpCode::POP, simple.m_semicolon.m_line);
}

void CodeGenerator::operator()(MidoriStatement::Define& def)
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

	// Put a placeholder value for block expression
	bool need_placeholder =
		(
			def.m_value->IsExpression<MidoriExpression::Block>()
			&& def.m_value->GetExpression<MidoriExpression::Block>().HasDefine()
		)
		||
		(
			def.m_value->IsExpression<MidoriExpression::Match>()
			&& std::ranges::any_of(def.m_value->GetExpression<MidoriExpression::Match>().m_cases, [](const std::unique_ptr<MidoriExpression>& case_expr) { return case_expr->IsExpression<MidoriExpression::Case>() && !case_expr->GetExpression<MidoriExpression::Case>().m_binding_names.empty(); })
		);
	if (need_placeholder)
	{
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
	}

	std::visit([this](auto&& arg){ (*this)(arg); }, **def.m_value);

	if (need_placeholder)
	{
		EmitByte(OpCode::UPDATE_PLACEHOLDER, line);
	}

	if (is_global)
	{
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::DefineTuple& def_tuple)
{
	int line = def_tuple.m_names.empty() ? 0 : def_tuple.m_names[0].m_line;

	// For each binding, extract the corresponding array element
	// We regenerate the tuple expression each time since GET_ARRAY consumes it
	for (size_t i = 0u; i < def_tuple.m_names.size(); i += 1u)
	{
		bool is_global = !def_tuple.m_local_indices[i].has_value();

		// Generate the tuple expression (loads it onto stack)
		std::visit([this](auto&& arg){ (*this)(arg); }, **def_tuple.m_value);

		// Push the index
		EmitIntegerConstant(static_cast<MidoriInteger>(i), line);

		// Get array element at index i (consumes the array from stack)
		EmitByte(OpCode::GET_ARRAY, line);
		EmitByte(static_cast<OpCode>(1), line); // 1 index

		if (is_global)
		{
			MidoriText variable_name(def_tuple.m_names[i].m_lexeme.c_str());
			int index = m_executable.AddGlobalVariable(std::move(variable_name));
			m_global_variables[def_tuple.m_names[i].m_lexeme] = index;
			EmitVariable(index, OpCode::DEFINE_GLOBAL, line);
		}
		// For local variables, the element is left on stack and becomes the local
	}
}

/*
*
*
* C-Style For loop is abandoned in Midori language
*
void CodeGenerator::operator()(MidoriStatement::For& for_stmt)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **for_stmt.m_condition_intializer);

	int loop_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
	int line = for_stmt.m_loop_keyword.m_line;

	int exit_jump = -1;
	std::visit([this](auto&& arg){ (*this)(arg); }, **for_stmt.m_condition);
	exit_jump = EmitJump(OpCode::JUMP_IF_FALSE, line);
	EmitByte(OpCode::POP, line);

	int body_jump = EmitJump(OpCode::JUMP, line);
	int incrementer_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
	std::visit([this](auto&& arg){ (*this)(arg); }, **for_stmt.m_condition_incrementer);
	EmitLoop(loop_start, line);
	loop_start = incrementer_start;
	PatchJump(body_jump, line);

	BeginLoop(loop_start);
	std::visit([this](auto&& arg){ (*this)(arg); }, **for_stmt.m_body);

	EmitLoop(loop_start, line);
	if (exit_jump != -1)
	{
		PatchJump(exit_jump, line);
		EmitByte(OpCode::POP, line);
	}

	while (for_stmt.m_control_block_local_count > 0)
	{
		int count_to_pop = std::min(for_stmt.m_control_block_local_count, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_LOCAL_SCOPE, line);
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		for_stmt.m_control_block_local_count -= count_to_pop;
	}
	EndLoop(line);
}
*/

void CodeGenerator::operator()(MidoriStatement::DefineFunction& defun)
{
	int line = defun.m_name.m_line;
	bool is_global = !defun.m_local_index.has_value();
	std::optional<int> index = std::nullopt;

	// Check if this is a generic function by looking at generic parameters
	bool is_generic = !defun.m_generic_params.empty();

	if (is_generic && is_global)
	{
		// Store generic function template for later specialization
		GenericFunctionInfo info;
		info.m_name = defun.m_name.m_lexeme;
		info.m_params = defun.m_params;
		info.m_body = &defun.m_body;
		info.m_captured_count = defun.m_captured_count;
		info.m_generic_param_types = defun.m_param_types;
		info.m_generic_return_type = defun.m_return_type;

		m_generic_functions[defun.m_name.m_lexeme] = std::move(info);

		// Don't generate code yet - will be done on-demand when called
		return;
	}

	if (is_global)
	{
		MidoriText variable_name(defun.m_name.m_lexeme.c_str());
		index.emplace(m_executable.AddGlobalVariable(std::move(variable_name)));
		m_global_variables[defun.m_name.m_lexeme] = index.value();
	}

	EmitFunction(defun.m_params, defun.m_body, defun.m_name.m_lexeme, line, defun.m_captured_count);

	if (is_global)
	{
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
	}
	else
	{
		// Local function - store in local variable
		EmitVariable(defun.m_local_index.value(), OpCode::SET_LOCAL, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::Continue& continue_stmt)
{
	int line = continue_stmt.m_keyword.m_line;

	while (continue_stmt.m_number_to_pop > 0)
	{
		int count_to_pop = std::min(continue_stmt.m_number_to_pop, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_VALUES, line);
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		continue_stmt.m_number_to_pop -= count_to_pop;
	}

	EmitLoop(m_loop_contexts.top().m_loop_start, line);
}

void CodeGenerator::operator()(MidoriStatement::Foreign& foreign)
{
	int line = foreign.m_function_name.m_line;

	const MidoriType::FunctionType& type = foreign.m_type->GetType<MidoriType::FunctionType>();
	if (!(type.m_return_type->IsType<MidoriType::IntegerType>() || type.m_return_type->IsType<MidoriType::FloatType>() || type.m_return_type->IsType<MidoriType::BoolType>() || type.m_return_type->IsType<MidoriType::UnitType>()))
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported return type for foreign function", foreign.m_function_name, m_file_name, m_source_lines));
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

void CodeGenerator::operator()(MidoriStatement::Struct&)
{
	return;
}

void CodeGenerator::operator()(MidoriStatement::Union&)
{
	return;
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
	else if (target_type->IsType<MidoriType::FloatType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			EmitByte(OpCode::TEXT_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::FloatType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to float' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::IntegerType>())
	{
		if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_INT, line);
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
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to int' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::UnitType>())
	{
		EmitByte(OpCode::OP_UNIT, line);
	}
	else if (target_type->IsType<MidoriType::TextType>())
	{
		if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_TEXT, line);
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
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to text' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else
	{
		// TODO: implement custom cast
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported type casting instruction", as.m_as_keyword, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::Binary& binary)
{
	int line = binary.m_op.m_line;

	if (binary.m_op.m_token_name == Token::Name::DOUBLE_BAR)
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_left);
		int jump_if_true = EmitJump(OpCode::JUMP_IF_TRUE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_right);
		PatchJump(jump_if_true, line);
		return;
	}
	else if (binary.m_op.m_token_name == Token::Name::DOUBLE_AMPERSAND)
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_left);
		int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_right);
		PatchJump(jump_if_false, line);
	}
	else
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_left);
		std::visit([this](auto&& arg) { (*this)(arg); }, **binary.m_right);
		const std::shared_ptr<MidoriType>& operand_type = GetConcreteTypeForExpression(binary.m_left);

		switch (binary.m_op.m_token_name)
		{
		case Token::Name::SINGLE_PLUS:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::ADD_FLOAT, line) : EmitByte(OpCode::ADD_INTEGER, line);
			break;
		case Token::Name::DOUBLE_PLUS:
			operand_type->IsType<MidoriType::TextType>() ? EmitByte(OpCode::CONCAT_TEXT, line) : EmitByte(OpCode::CONCAT_ARRAY, line);
			break;
		case Token::Name::SINGLE_MINUS:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::SUBTRACT_FLOAT, line) : EmitByte(OpCode::SUBTRACT_INTEGER, line);
			break;
		case Token::Name::STAR:
			operand_type->IsType<MidoriType::FloatType>()
				? EmitByte(OpCode::MULTIPLY_FLOAT, line)
				: operand_type->IsType<MidoriType::IntegerType>()
				? EmitByte(OpCode::MULTIPLY_INTEGER, line)
				: EmitByte(OpCode::DUP_ARRAY, line);
			break;
		case Token::Name::SLASH:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::DIVIDE_FLOAT, line) : EmitByte(OpCode::DIVIDE_INTEGER, line);
			break;
		case Token::Name::PERCENT:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::MODULO_FLOAT, line) : EmitByte(OpCode::MODULO_INTEGER, line);
			break;
		case Token::Name::LEFT_SHIFT:
			EmitByte(OpCode::LEFT_SHIFT, line);
			break;
		case Token::Name::RIGHT_SHIFT:
			EmitByte(OpCode::RIGHT_SHIFT, line);
			break;
		case Token::Name::LEFT_ANGLE:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::LESS_FLOAT, line) : EmitByte(OpCode::LESS_INTEGER, line);
			break;
		case Token::Name::LESS_EQUAL:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::LESS_EQUAL_FLOAT, line) : EmitByte(OpCode::LESS_EQUAL_INTEGER, line);
			break;
		case Token::Name::RIGHT_ANGLE:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::GREATER_FLOAT, line) : EmitByte(OpCode::GREATER_INTEGER, line);
			break;
		case Token::Name::GREATER_EQUAL:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::GREATER_EQUAL_FLOAT, line) : EmitByte(OpCode::GREATER_EQUAL_INTEGER, line);
			break;
		case Token::Name::BANG_EQUAL:
			operand_type->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::NOT_EQUAL_FLOAT, line) : EmitByte(OpCode::NOT_EQUAL_INTEGER, line);
			break;
		case Token::Name::DOUBLE_EQUAL:
			operand_type->IsType<MidoriType::FloatType>()
				? EmitByte(OpCode::EQUAL_FLOAT, line)
				: operand_type->IsType<MidoriType::IntegerType>()
				? EmitByte(OpCode::EQUAL_INTEGER, line)
				: operand_type->IsType<MidoriType::TextType>()
				? EmitByte(OpCode::EQUAL_TEXT, line)
				: EmitByte(OpCode::EQUAL_INTEGER, line); // This remaining case is bool, we just treat them as integers
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
		default:
#ifdef _MSC_VER
			__assume(0);
#else
			__builtin_unreachable();
#endif
		}
	}
}

void CodeGenerator::operator()(MidoriExpression::Group& group)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **group.m_expr_in);
}

void CodeGenerator::operator()(MidoriExpression::Tuple& tuple)
{
	int line = tuple.m_op.m_line;
	int size = static_cast<int>(tuple.m_elements.size());

	// Emit code for each element (they will be pushed onto the stack)
	std::ranges::for_each
	(
		tuple.m_elements,
		[this](std::unique_ptr<MidoriExpression>& elem)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **elem);
		}
	);

	// At runtime, tuples are represented as arrays (heterogeneous)
	// Type checking ensures type safety
	EmitByte(OpCode::CREATE_ARRAY, line);
	EmitThreeBytes(size, size >> 8, size >> 16, line);
}

void CodeGenerator::operator()(MidoriExpression::UnaryPrefix& unary)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **unary.m_expr);

	switch (unary.m_op.m_token_name)
	{
	case Token::Name::SINGLE_MINUS:
		GetConcreteTypeForExpression(unary.m_expr)->IsType<MidoriType::FloatType>() ? EmitByte(OpCode::NEGATE_FLOAT, unary.m_op.m_line) : EmitByte(OpCode::NEGATE_INTEGER, unary.m_op.m_line);
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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many arguments (max {})", MAX_FUNCTION_ARITY + 1), call.m_paren, m_file_name, m_source_lines));
		return;
	}

	// Check if this is a call to a generic function
	bool is_generic_call = false;
	std::string function_name;

	if (call.m_callee->IsExpression<MidoriExpression::BoundedName>())
	{
		MidoriExpression::BoundedName& callee_name = call.m_callee->GetExpression<MidoriExpression::BoundedName>();
		function_name = callee_name.m_name.m_lexeme;

		// Check if this name refers to a generic function
		std::unordered_map<std::string, GenericFunctionInfo>::iterator generic_it = m_generic_functions.find(function_name);
		if (generic_it != m_generic_functions.end())
		{
			is_generic_call = true;
		}
	}

	if (is_generic_call)
	{
		std::vector<std::shared_ptr<MidoriType>> concrete_arg_types;
		for (std::unique_ptr<MidoriExpression>& arg : call.m_arguments)
		{
			concrete_arg_types.push_back(arg->GetType());
		}

		int specialized_proc_index = SpecializeGenericFunction(function_name, concrete_arg_types, line);
		if (specialized_proc_index == -1)
		{
			// Error already reported in SpecializeGenericFunction
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

		// Push the specialized closure
		EmitByte(OpCode::ALLOCATE_CLOSURE, line);
		EmitByte(static_cast<OpCode>(specialized_proc_index), line);

		GenericFunctionInfo& generic_info = m_generic_functions[function_name];
		EmitByte(OpCode::CONSTRUCT_CLOSURE, line);
		EmitByte(static_cast<OpCode>(generic_info.m_captured_count), line);

		if (call.m_is_tail_call)
		{
			EmitByte(OpCode::TAIL_CALL, line);
		}
		else
		{
			EmitByte(OpCode::CALL_DEFINED, line);
		}
		EmitByte(static_cast<OpCode>(arity), line);
	}
	else
	{
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
		else if (call.m_is_tail_call)
		{
			EmitByte(OpCode::TAIL_CALL, line);
		}
		else
		{
			EmitByte(OpCode::CALL_DEFINED, line);
		}

		EmitByte(static_cast<OpCode>(arity), line);
	}
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
				AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Bad BoundedName expression", variable.m_name, m_file_name, m_source_lines));
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
				AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Bad Bind expression", bind.m_name, m_file_name, m_source_lines));
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

void CodeGenerator::operator()(MidoriExpression::FloatLiteral& float_literal)
{
	int line = float_literal.m_token.m_line;
	EmitFloatConstant(std::stod(float_literal.m_token.m_lexeme), line);
}

void CodeGenerator::operator()(MidoriExpression::IntegerLiteral& integer)
{
	int line = integer.m_token.m_line;
	EmitIntegerConstant(std::stoll(integer.m_token.m_lexeme), line);
}

void CodeGenerator::operator()(MidoriExpression::UnitLiteral& unit)
{
	int line = unit.m_token.m_line;
	EmitByte(OpCode::OP_UNIT, line);
}

void CodeGenerator::operator()(MidoriExpression::Function& function)
{
	int line = function.m_function_keyword.m_line;
	EmitFunction(function.m_params, function.m_body, "Anonymous Function at line: " + std::to_string(line), line, function.m_captured_count);
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
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Union tag too large (max {})", MAX_UNION_TAG + 1), construct.m_data_name, m_file_name, m_source_lines));
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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many array elements (max {})", MAX_ARRAY_SIZE + 1), array.m_op, m_file_name, m_source_lines));
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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many array indices (max {})", MAX_NESTED_ARRAY_INDEX + 1), array_get.m_op, m_file_name, m_source_lines));
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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many array indices (max {})", MAX_NESTED_ARRAY_INDEX + 1), array_set.m_op, m_file_name, m_source_lines));
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

void CodeGenerator::operator()(MidoriExpression::IfElse& if_else)
{
	int line = if_else.m_if_token.m_line;
	std::visit([this](auto&& arg){ (*this)(arg); }, **if_else.m_condition);

	if (if_else.m_condition_operand_type == MidoriExpression::ConditionOperandType::INTEGER || if_else.m_condition_operand_type == MidoriExpression::ConditionOperandType::FLOAT)
	{
		EmitNumericConditionalJump(if_else.m_condition_operand_type, if_else.m_true_branch, if_else.m_else_branch, line);
	}
	else
	{
		int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg){ (*this)(arg); }, **if_else.m_true_branch);
		int jump = EmitJump(OpCode::JUMP, line);
		PatchJump(jump_if_false, line);
		EmitByte(OpCode::POP, line);
		std::visit([this](auto&& arg) { (*this)(arg); }, **if_else.m_else_branch);
		PatchJump(jump, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::Block& block)
{
	std::ranges::for_each
	(
		block.m_stmts,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **statement);
		}
	);

	// Discard everything else when encountered "return"
	if (!m_procedures[m_current_procedure_index].IsByteCodeEmpty() && m_procedures[m_current_procedure_index].ReadByteCode(m_procedures[m_current_procedure_index].GetByteCodeSize() - 1) == OpCode::RETURN)
	{
		return;
	}
	else
	{
		if (block.m_final_expr.has_value())
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, ***block.m_final_expr);
		}
		else
		{
			EmitByte(OpCode::OP_UNIT, block.m_right_brace.m_line);
		}

		while (block.m_local_count > 0)
		{
			int count_to_pop = std::min(block.m_local_count, static_cast<int>(UINT8_MAX));

			if (count_to_pop == block.m_local_count)
			{
				EmitByte(OpCode::POP_BLOCK_SCOPE, block.m_right_brace.m_line);
			}
			else
			{
				EmitByte(OpCode::POP_LOCAL_SCOPE, block.m_right_brace.m_line);
			}

			EmitByte(static_cast<OpCode>(count_to_pop), block.m_right_brace.m_line);
			block.m_local_count -= count_to_pop;
		}
	}
}

void CodeGenerator::operator()(MidoriExpression::Match& match)
{
	int line = match.m_match_keyword.m_line;
	std::visit([this](auto&& arg) { (*this)(arg); }, **match.m_arg_expr);
	EmitByte(OpCode::LOAD_TAG, line);

	// Check if we can use jump table optimization
	// Requirements:
	// 1. All cases are Case expressions (not Default)
	// 2. Tags are dense and sequential starting from 0: 0, 1, 2, 3, ..., n-1
	// 3. At least 3 cases (jump table overhead not worth it for 2 cases)
	bool can_use_jump_table = match.m_cases.size() >= 3u;
	std::vector<MidoriExpression::Case*> sorted_cases;
	sorted_cases.reserve(match.m_cases.size());

	if (can_use_jump_table)
	{
		for (const std::unique_ptr<MidoriExpression>& case_expr : match.m_cases)
		{
			if (!case_expr->IsExpression<MidoriExpression::Case>())
			{
				can_use_jump_table = false;
				break;
			}
			sorted_cases.emplace_back(&case_expr->GetExpression<MidoriExpression::Case>());
		}

		// Sort cases by tag and check for dense sequential tags
		if (can_use_jump_table)
		{
			std::ranges::sort(sorted_cases, [](const MidoriExpression::Case* a, const MidoriExpression::Case* b) { return a->m_tag < b->m_tag; });

			for (size_t i = 0u; i < sorted_cases.size(); i += 1u)
			{
				if (sorted_cases[i]->m_tag != static_cast<int>(i))
				{
					can_use_jump_table = false;
					break;
				}
			}
		}
	}

	if (can_use_jump_table)
	{
		EmitByte(OpCode::MATCH_JUMP_TABLE, line);
		EmitByte(static_cast<OpCode>(sorted_cases.size()), line);

		// Reserve space for jump offsets (2 bytes each)
		std::vector<int> case_offset_positions;
		case_offset_positions.reserve(sorted_cases.size());
		for (size_t i = 0u; i < sorted_cases.size(); i += 1u)
		{
			case_offset_positions.emplace_back(m_procedures[m_current_procedure_index].GetByteCodeSize());
			EmitByte(static_cast<OpCode>(0xff), line);
			EmitByte(static_cast<OpCode>(0xff), line);
		}

		// Emit all case bodies and patch offsets
		std::vector<int> end_jumps;
		end_jumps.reserve(sorted_cases.size());
		for (size_t i = 0u; i < sorted_cases.size(); i += 1u)
		{
			MidoriExpression::Case* member_case = sorted_cases[i];

			// Patch the jump table offset for this case
			int case_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
			int offset_from_table = case_start - static_cast<int>(case_offset_positions[0u] + sorted_cases.size() * 2u);
			m_procedures[m_current_procedure_index].SetByteCode(case_offset_positions[i], static_cast<OpCode>(offset_from_table & 0xff));
			m_procedures[m_current_procedure_index].SetByteCode(case_offset_positions[i] + 1, static_cast<OpCode>((offset_from_table >> 8) & 0xff));

			// Emit case body
			std::visit([this](auto&& arg) { (*this)(arg); }, **member_case->m_expr);

			// Pop match scope bindings
			int num_to_pop = static_cast<int>(member_case->m_binding_names.size());
			while (num_to_pop > 0)
			{
				int count_to_pop = std::min(num_to_pop, static_cast<int>(UINT8_MAX));

				if (count_to_pop == num_to_pop)
				{
					EmitByte(OpCode::POP_MATCH_SCOPE, line);
				}
				else
				{
					EmitByte(OpCode::POP_VALUES, line);
				}
				EmitByte(static_cast<OpCode>(count_to_pop), line);
				num_to_pop -= count_to_pop;
			}

			end_jumps.emplace_back(EmitJump(OpCode::JUMP, line));
		}

		// Patch all end jumps to point after the match
		for (int jump_addr : end_jumps)
		{
			PatchJump(jump_addr, line);
		}
	}
	else
	{
		// Fall back to linear search
		std::vector<int> jumps;
		for (std::unique_ptr<MidoriExpression>& case_expr : match.m_cases)
		{
			if (case_expr->IsExpression<MidoriExpression::Case>())
			{
				MidoriExpression::Case& member_case = case_expr->GetExpression<MidoriExpression::Case>();

				EmitByte(OpCode::DUP, line);
				MidoriInteger member_tag = static_cast<MidoriInteger>(member_case.m_tag);
				EmitIntegerConstant(member_tag, line);
				EmitByte(OpCode::EQUAL_INTEGER, line);
				int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
				EmitByte(OpCode::POP, line); // pop tag
				EmitByte(OpCode::POP, line); // pop comp result

				std::visit([this](auto&& arg) { (*this)(arg); }, **case_expr);

				int num_to_pop = static_cast<int>(member_case.m_binding_names.size());
				while (num_to_pop > 0)
				{
					int count_to_pop = std::min(num_to_pop, static_cast<int>(UINT8_MAX));

					if (count_to_pop == num_to_pop)
					{
						EmitByte(OpCode::POP_MATCH_SCOPE, line);
					}
					else
					{
						EmitByte(OpCode::POP_VALUES, line);
					}
					EmitByte(static_cast<OpCode>(count_to_pop), line);
					num_to_pop -= count_to_pop;
				}
				jumps.emplace_back(EmitJump(OpCode::JUMP, line));

				PatchJump(jump_if_false, line);
				EmitByte(OpCode::POP, line);
			}
			else
			{
				std::visit([this](auto&& arg) { (*this)(arg); }, **case_expr);

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
}

void CodeGenerator::operator()(MidoriExpression::Case& case_expr)
{
	std::visit([this](auto&& arg) { (*this)(arg); }, **case_expr.m_expr);
}

void CodeGenerator::operator()(MidoriExpression::Default& default_expr)
{
	std::visit([this](auto&& arg) { (*this)(arg); }, **default_expr.m_expr);
}

void CodeGenerator::operator()(MidoriExpression::Loop& loop)
{
	int line = loop.m_loop_keyword.m_line;

	int loop_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
	BeginLoop(loop_start);

	std::visit([this](auto&& arg) { (*this)(arg); }, **loop.m_body);
	EmitByte(OpCode::POP, line);

	EmitLoop(loop_start, line);
	EndLoop(line);
}

void CodeGenerator::operator()(MidoriExpression::Break& break_expr)
{
	int line = break_expr.m_keyword.m_line;

	std::visit([this](auto&& arg) { (*this)(arg); }, **break_expr.m_value);

	while (break_expr.m_number_to_pop > 0)
	{
		int count_to_pop = std::min(break_expr.m_number_to_pop, static_cast<int>(UINT8_MAX));
		if (count_to_pop == break_expr.m_number_to_pop)
		{
			EmitByte(OpCode::POP_BLOCK_SCOPE, line);
		}
		else
		{
			EmitByte(OpCode::POP_LOCAL_SCOPE, line);
		}
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		break_expr.m_number_to_pop -= count_to_pop;
	}

	m_loop_contexts.top().m_break_positions.emplace_back(EmitJump(OpCode::BREAK, line));
}

void CodeGenerator::operator()(MidoriExpression::Return& return_expr)
{
	int line = return_expr.m_keyword.m_line;
	std::visit([this](auto&& arg) { (*this)(arg); }, **return_expr.m_value);
	EmitByte(OpCode::RETURN, line);
}

void CodeGenerator::EmitNumericConditionalJump(MidoriExpression::ConditionOperandType operand_type, std::unique_ptr<MidoriExpression>& true_branch, std::unique_ptr<MidoriExpression>& else_branch, int line)
{
	int if_jump;
	if (operand_type == MidoriExpression::ConditionOperandType::INTEGER)
	{
		PopByte(line);
		switch (m_last_opcode)
		{
		case OpCode::LESS_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_LESS, line);
			break;
		case OpCode::LESS_EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_LESS_EQUAL, line);
			break;
		case OpCode::GREATER_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_GREATER, line);
			break;
		case OpCode::GREATER_EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_GREATER_EQUAL, line);
			break;
		case OpCode::EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_EQUAL, line);
			break;
		case OpCode::NOT_EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_NOT_EQUAL, line);
			break;
		default:
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid opcode for integer ternary condition", line, m_file_name, m_source_lines));
			return;
		}

		std::visit([this](auto&& arg) { (*this)(arg); }, **true_branch);

		int else_jump = EmitJump(OpCode::JUMP, line);
		PatchJump(if_jump, line);
		if (else_branch != nullptr)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **else_branch);
		}
		PatchJump(else_jump, line);
	}
	else
	{
		PopByte(line);
		switch (m_last_opcode)
		{
		case OpCode::LESS_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_LESS, line);
			break;
		case OpCode::LESS_EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_LESS_EQUAL, line);
			break;
		case OpCode::GREATER_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_GREATER, line);
			break;
		case OpCode::GREATER_EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_GREATER_EQUAL, line);
			break;
		case OpCode::EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_EQUAL, line);
			break;
		case OpCode::NOT_EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_NOT_EQUAL, line);
			break;
		default:
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid opcode for float ternary condition", line, m_file_name, m_source_lines));
			return;
		}

		std::visit([this](auto&& arg) { (*this)(arg); }, **true_branch);

		int else_jump = EmitJump(OpCode::JUMP, line);
		PatchJump(if_jump, line);
		if (else_branch != nullptr)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **else_branch);
		}
		PatchJump(else_jump, line);
	}
}

bool CodeGenerator::IsGenericType(const std::shared_ptr<MidoriType>& type)
{
	return std::visit
	(
		[this](auto&& type_variant) -> bool
		{
			using T = std::decay_t<decltype(type_variant)>;

			if constexpr (std::is_same_v<T, MidoriType::TypeVariable>)
			{
				return true;
			}
			else if constexpr (std::is_same_v<T, MidoriType::FunctionType>)
			{
				bool result = IsGenericType(type_variant.m_return_type);
				for (const std::shared_ptr<MidoriType>& param_type : type_variant.m_param_types)
				{
					result = result || IsGenericType(param_type);
				}
				return result;
			}
			else if constexpr (std::is_same_v<T, MidoriType::ArrayType>)
			{
				return IsGenericType(type_variant.m_element_type);
			}
			else if constexpr (std::is_same_v<T, MidoriType::StructType>)
			{
				for (const std::shared_ptr<MidoriType>& member_type : type_variant.m_member_types)
				{
					if (IsGenericType(member_type))
					{
						return true;
					}
				}
				return false;
			}
			else if constexpr (std::is_same_v<T, MidoriType::UnionType>)
			{
				for (const std::unordered_map<std::string, MidoriType::UnionType::UnionMemberContext>::value_type& member_pair : type_variant.m_member_info)
				{
					for (const std::shared_ptr<MidoriType>& member_type : member_pair.second.m_member_types)
					{
						if (IsGenericType(member_type))
						{
							return true;
						}
					}
				}
				return false;
			}
			else
			{
				return false;
			}
		},
		type->m_type
	);
}

int CodeGenerator::SpecializeGenericFunction(const std::string& base_name, const std::vector<std::shared_ptr<MidoriType>>& concrete_arg_types, int line)
{
	std::vector<std::string> concrete_type_names;
	for (const std::shared_ptr<MidoriType>& arg_type : concrete_arg_types)
	{
		concrete_type_names.push_back(arg_type->ToString());
	}
	FunctionSignature signature{ base_name, concrete_type_names };

	std::unordered_map<FunctionSignature, int, FunctionSignatureHash>::iterator it = m_specialized_functions.find(signature);
	if (it != m_specialized_functions.end())
	{
		return it->second;
	}

	std::unordered_map<std::string, GenericFunctionInfo>::iterator generic_it = m_generic_functions.find(base_name);
	if (generic_it == m_generic_functions.end())
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Generic function '{}' not found", base_name), line, m_file_name, m_source_lines));
		return -1;
	}

	GenericFunctionInfo& generic_info = generic_it->second;
	std::string specialized_name = base_name + "<"s;
	for (size_t i = 0u; i < concrete_type_names.size(); i += 1u)
	{
		if (i > 0u)
		{
			specialized_name += ","s;
		}
		specialized_name += concrete_type_names[i];
	}
	specialized_name += ">"s;

	// Build parameter name -> concrete type map
	std::unordered_map<std::string, std::shared_ptr<MidoriType>> prev_param_map = m_param_type_map;
	m_param_type_map.clear();

	for (size_t i = 0u; i < generic_info.m_params.size() && i < concrete_arg_types.size(); i += 1u)
	{
		m_param_type_map[generic_info.m_params[i].m_lexeme] = concrete_arg_types[i];
	}

	size_t prev_index = m_current_procedure_index;
	m_current_procedure_index = m_procedures.size();
	int specialized_proc_index = static_cast<int>(m_current_procedure_index);
	m_procedures.emplace_back();

	std::visit([this](auto&& arg) { (*this)(arg); }, **(*generic_info.m_body));
	EmitByte(OpCode::RETURN, line);

	m_procedure_names.emplace_back(specialized_name.c_str());

	m_current_procedure_index = prev_index;

	m_param_type_map = std::move(prev_param_map);
	m_specialized_functions[signature] = specialized_proc_index;

	return specialized_proc_index;
}

std::shared_ptr<MidoriType> CodeGenerator::GetConcreteTypeForExpression(const std::unique_ptr<MidoriExpression>& expr)
{
	if (!m_param_type_map.empty() && expr->IsExpression<MidoriExpression::BoundedName>())
	{
		const MidoriExpression::BoundedName& bounded_name = expr->GetExpression<MidoriExpression::BoundedName>();
		std::unordered_map<std::string, std::shared_ptr<MidoriType>>::iterator it = m_param_type_map.find(bounded_name.m_name.m_lexeme);
		if (it != m_param_type_map.end())
		{
			return it->second;
		}
	}

	return expr->GetType();
}

void CodeGenerator::EmitFunction(const std::vector<Token>& params, std::unique_ptr<MidoriExpression>& body, const std::string& debug_name, int line, int captured_count)
{
	int arity = static_cast<int>(params.size());
	if (arity > MAX_FUNCTION_ARITY)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many arguments (max {})", MAX_FUNCTION_ARITY + 1), line, m_file_name, m_source_lines));
		return;
	}
	if (captured_count > MAX_CAPTURED_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many captured variables (max {})", MAX_CAPTURED_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	size_t prev_index = m_current_procedure_index;
	m_current_procedure_index = m_procedures.size();
	m_procedures.emplace_back();
	std::visit([this](auto&& arg) { (*this)(arg); }, **body);

	EmitByte(OpCode::RETURN, line);

	size_t closure_proc_index = m_current_procedure_index;
	m_procedure_names.emplace_back(debug_name.c_str());

	m_current_procedure_index = prev_index;

	if (m_current_procedure_index > MAX_FUNCTION_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many functions (max {})", MAX_FUNCTION_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	EmitByte(OpCode::ALLOCATE_CLOSURE, line);
	EmitByte(static_cast<OpCode>(closure_proc_index), line);

	EmitByte(OpCode::CONSTRUCT_CLOSURE, line);
	EmitByte(static_cast<OpCode>(captured_count), line);
}

std::size_t CodeGenerator::FunctionSignatureHash::operator()(const FunctionSignature& sig) const
{
	std::size_t hash = std::hash<std::string>{}(sig.m_base_name);
	std::ranges::for_each
	(
		sig.m_concrete_types,
		[&hash](const std::string& type)
		{
			hash ^= std::hash<std::string>{}(type)+0x9e3779b9 + (hash << 6) + (hash >> 2);
		}
	);
	return hash;
}
bool CodeGenerator::FunctionSignature::operator==(const FunctionSignature& other) const
{
	return m_base_name == other.m_base_name && m_concrete_types == other.m_concrete_types;
}
