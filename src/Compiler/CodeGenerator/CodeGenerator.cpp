#include <cctype>
#include <filesystem>
#include <format>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <set>

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
	EmitByte(static_cast<OpCode>(byte1 & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(byte2 & BYTE_MASK), line);
}

void CodeGenerator::EmitThreeBytes(int byte1, int byte2, int byte3, int line)
{
	EmitByte(static_cast<OpCode>(byte1 & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(byte2 & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(byte3 & BYTE_MASK), line);
}

void CodeGenerator::EmitNumericConstant(MidoriInteger val, int line, bool is_integer)
{
	int byte1 = val & BYTE_MASK;
	int byte2 = (val >> SHIFT_8_BITS) & BYTE_MASK;
	int byte3 = (val >> SHIFT_16_BITS) & BYTE_MASK;
	int byte4 = (val >> SHIFT_24_BITS) & BYTE_MASK;
	int byte5 = (val >> SHIFT_32_BITS) & BYTE_MASK;
	int byte6 = (val >> SHIFT_40_BITS) & BYTE_MASK;
	int byte7 = (val >> SHIFT_48_BITS) & BYTE_MASK;
	int byte8 = (val >> SHIFT_56_BITS) & BYTE_MASK;

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

void CodeGenerator::EmitByteConstant(MidoriByte value, int line)
{
	EmitByte(OpCode::BYTE_CONSTANT, line);
	EmitByte(static_cast<OpCode>(value), line);
}

void CodeGenerator::EmitWordConstant(MidoriWord value, int line)
{
	EmitByte(OpCode::WORD_CONSTANT, line);
	int byte1 = value & BYTE_MASK;
	int byte2 = (value >> SHIFT_8_BITS) & BYTE_MASK;
	int byte3 = (value >> SHIFT_16_BITS) & BYTE_MASK;
	int byte4 = (value >> SHIFT_24_BITS) & BYTE_MASK;
	int byte5 = (value >> SHIFT_32_BITS) & BYTE_MASK;
	int byte6 = (value >> SHIFT_40_BITS) & BYTE_MASK;
	int byte7 = (value >> SHIFT_48_BITS) & BYTE_MASK;
	int byte8 = (value >> SHIFT_56_BITS) & BYTE_MASK;
	EmitByte(static_cast<OpCode>(byte1), line);
	EmitByte(static_cast<OpCode>(byte2), line);
	EmitByte(static_cast<OpCode>(byte3), line);
	EmitByte(static_cast<OpCode>(byte4), line);
	EmitByte(static_cast<OpCode>(byte5), line);
	EmitByte(static_cast<OpCode>(byte6), line);
	EmitByte(static_cast<OpCode>(byte7), line);
	EmitByte(static_cast<OpCode>(byte8), line);
}

void CodeGenerator::EmitVariable(int variable_index, OpCode op, int line)
{
	if (variable_index <= MAX_LOCAL_VARIABLES)
	{
		EmitByte(op, line);
		EmitByte(static_cast<OpCode>(variable_index), line);
		return;
	}

	if (variable_index > MAX_VARIABLES)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many variables (max {})", MAX_VARIABLES), line, m_file_name, m_source_lines));
		return;
	}

	OpCode wide_op = op;
	switch (op)
	{
	case OpCode::DEFINE_GLOBAL:
		wide_op = OpCode::DEFINE_GLOBAL_WIDE;
		break;
	case OpCode::GET_GLOBAL:
		wide_op = OpCode::GET_GLOBAL_WIDE;
		break;
	case OpCode::SET_GLOBAL:
		wide_op = OpCode::SET_GLOBAL_WIDE;
		break;
	case OpCode::GET_LOCAL:
		wide_op = OpCode::GET_LOCAL_WIDE;
		break;
	case OpCode::SET_LOCAL:
		wide_op = OpCode::SET_LOCAL_WIDE;
		break;
	case OpCode::GET_CELL:
		wide_op = OpCode::GET_CELL_WIDE;
		break;
	case OpCode::SET_CELL:
		wide_op = OpCode::SET_CELL_WIDE;
		break;
	default:
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid opcode for wide variable operation", line, m_file_name, m_source_lines));
		return;
	}

	EmitByte(wide_op, line);
	EmitTwoBytes(variable_index >> 8, variable_index & 0xFF, line);
}

int CodeGenerator::EmitJump(OpCode op, int line)
{
	EmitByte(op, line);
	EmitByte(static_cast<OpCode>(BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(BYTE_MASK), line);
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

	m_procedures[m_current_procedure_index].SetByteCode(offset, static_cast<OpCode>(jump & BYTE_MASK));
	m_procedures[m_current_procedure_index].SetByteCode(offset + 1, static_cast<OpCode>((jump >> SHIFT_8_BITS) & BYTE_MASK));
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

	EmitByte(static_cast<OpCode>(offset & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>((offset >> SHIFT_8_BITS) & BYTE_MASK), line);
}

void CodeGenerator::EmitEquatableEquals(const std::shared_ptr<MidoriType>& operand_type, int line)
{
	std::string mangled_name = INTERNAL_NAME_PREFIX + std::string(EQUALS_MANGLED_PREFIX) + operand_type->ToString();
	std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
	if (it != m_global_variables.end())
	{
		EmitVariable(it->second, OpCode::GET_GLOBAL, line);
		EmitByte(OpCode::CALL, line);
		EmitByte(static_cast<OpCode>(2), line);
	}
	else
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Equatable instance method '"s + mangled_name + "' not found"s, line, m_file_name, m_source_lines));
	}
}

void CodeGenerator::EmitOrderableCompare(const std::shared_ptr<MidoriType>& operand_type, int line)
{
	std::string mangled_name = INTERNAL_NAME_PREFIX + std::string(COMPARE_MANGLED_PREFIX) + operand_type->ToString();
	std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
	if (it != m_global_variables.end())
	{
		EmitVariable(it->second, OpCode::GET_GLOBAL, line);
		EmitByte(OpCode::CALL, line);
		EmitByte(static_cast<OpCode>(2), line);
	}
	else
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Orderable instance method '"s + mangled_name + "' not found"s, line, m_file_name, m_source_lines));
	}
}

void CodeGenerator::BeginLoop(int loop_start)
{
	m_loop_contexts.emplace(std::vector<int>(), loop_start, loop_start);
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

CodeGenerator::CodeGenerator(MidoriProgramTree&& program_tree, std::string_view file_name, const std::vector<std::string>& source_lines, std::string module_name, std::unordered_set<std::string> export_symbols, const TypeclassMethodMap& imported_class_methods, const TypeclassInstanceMap& imported_class_instances, const std::unordered_map<std::string, GenericFunctionInfo>& imported_generic_functions)
	: m_program_tree(std::move(program_tree)),
	m_file_name(file_name),
	m_source_lines(source_lines),
	m_module_name(std::move(module_name)),
	m_export_symbols(std::move(export_symbols)),
	m_class_methods(imported_class_methods),
	m_class_instances(imported_class_instances),
	m_generic_functions(imported_generic_functions)
{
	std::string main_proc_name = std::string(MAIN_PROCEDURE_PREFIX) + "@"s + (m_module_name.has_value() ? m_module_name.value() : std::string(file_name));
	m_procedure_names.emplace_back(main_proc_name.c_str());
}

MidoriResult::CodeGeneratorResult CodeGenerator::GenerateModuleBytecode()
{
	std::ranges::for_each
	(
		m_program_tree,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **statement);

			// Track exports: after processing DefineFunction, check if it's exported
			std::visit
			(
				[this](const auto& stmt)
				{
					using T = std::decay_t<decltype(stmt)>;
					if constexpr (std::is_same_v<T, MidoriStatement::FunctionDefinition>)
					{
						const std::string& function_name = stmt.m_name.m_lexeme;
						if (m_export_symbols.contains(function_name))
						{
							// After DefineFunction processing, m_current_procedure_index points AFTER the new procedure
							// So the procedure we just added is at index m_procedures.size() - 1
							const size_t procedure_index = m_procedures.size() - 1u;
							const size_t global_index = static_cast<size_t>(m_global_variables[function_name]);

							m_tracked_exports.emplace_back(function_name, procedure_index, global_index, BytecodeModule::SymbolType::FUNCTION);
						}
					}
					else if constexpr (std::is_same_v<T, MidoriStatement::Struct>)
					{
						const std::string& struct_name = stmt.m_name.m_lexeme;
						if (m_export_symbols.contains(struct_name))
						{
							m_tracked_exports.emplace_back
							(
								struct_name,
								0uz,  // Structs don't have procedure index
								0uz,  // Structs don't have global index
								BytecodeModule::SymbolType::STRUCT_TYPE
							);
						}
					}
					else if constexpr (std::is_same_v<T, MidoriStatement::Union>)
					{
						const std::string& union_name = stmt.m_name.m_lexeme;
						if (m_export_symbols.contains(union_name))
						{
							m_tracked_exports.emplace_back
							(
								union_name,
								0uz,  // Unions don't have procedure index
								0uz,  // Unions don't have global index
								BytecodeModule::SymbolType::UNION_TYPE
							);
						}
					}
					else if constexpr (std::is_same_v<T, MidoriStatement::ForeignDefinition>)
					{
						const std::string& foreign_name = stmt.m_function_name.m_lexeme;
						if (m_export_symbols.contains(foreign_name))
						{
							// Foreign functions are stored as global variables containing the function name string
							const size_t global_index = static_cast<size_t>(m_global_variables[foreign_name]);
							m_tracked_exports.emplace_back
							(
								foreign_name,
								0uz,  // Foreign functions don't have procedure index
								global_index,
								BytecodeModule::SymbolType::FOREIGN_FUNCTION
							);
						}
					}
					else if constexpr (std::is_same_v<T, MidoriStatement::VariableDefinition>)
					{
						const std::string& var_name = stmt.m_name.m_lexeme;
						if (m_export_symbols.contains(var_name))
						{
							const size_t global_index = static_cast<size_t>(m_global_variables[var_name]);
							m_tracked_exports.emplace_back
							(
								var_name,
								0uz,  // Global variables don't have procedure index
								global_index,
								BytecodeModule::SymbolType::GLOBAL_VARIABLE
							);
						}
					}
				},
				**statement
			);
		}
	);

	// Add RETURN to end the global procedure (procedure 0)
	// This ensures the instruction pointer doesn't run past the end of the procedure
	// Global procedures return Unit
	EmitByte(OpCode::OP_UNIT, 0);
	EmitByte(OpCode::RETURN, 0);

	if (!m_errors.empty())
	{
		return std::unexpected<std::string>(std::move(m_errors));
	}

	BytecodeModule module(m_module_name.value_or(""s), std::filesystem::path(m_file_name));
	module.m_procedures = std::move(m_procedures);
	module.m_procedure_names = std::move(m_procedure_names);
	module.m_string_pool = std::move(m_string_pool);
	module.m_exports = std::move(m_tracked_exports);
	module.m_imports = std::move(m_tracked_imports);
	module.m_generic_functions = std::move(m_generic_functions);

	std::vector<std::pair<std::string, int>> sorted_globals(m_global_variables.begin(), m_global_variables.end());
	std::ranges::sort(sorted_globals, [](const std::pair<std::string, int>& a, const std::pair<std::string, int>& b) { return a.second < b.second; });

	module.m_global_variables.reserve(sorted_globals.size());
	for (const std::pair<std::string, int>& entry : sorted_globals)
	{
		module.m_global_variables.emplace_back(entry.first.c_str());
	}

	return module;
}



void CodeGenerator::operator()(MidoriStatement::ExpressionStatement& simple)
{
	std::visit([this](auto&& arg){ (*this)(arg); }, **simple.m_expr);
	EmitByte(OpCode::POP, simple.m_semicolon.m_line);
}

void CodeGenerator::operator()(MidoriStatement::VariableDefinition& def)
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
	else
	{
		// For local variables, emit SET_LOCAL to properly store the value
		EmitVariable(def.m_local_index.value(), OpCode::SET_LOCAL, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::TupleDefinition& def_tuple)
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

void CodeGenerator::operator()(MidoriStatement::FunctionDefinition& defun)
{
	int line = defun.m_name.m_line;

	// Mangled names have at least 3 parts separated by underscores, with a capital letter after first underscore
	bool is_instance_method = false;
	{
		size_t first_underscore = defun.m_name.m_lexeme.find('_');
		if (first_underscore != std::string::npos && first_underscore + 1u < defun.m_name.m_lexeme.size())
		{
			if (std::isupper(defun.m_name.m_lexeme[first_underscore + 1u]))
			{
				size_t second_underscore = defun.m_name.m_lexeme.find('_', first_underscore + 1u);
				is_instance_method = (second_underscore != std::string::npos);
			}
		}
	}

	// Instance methods should always be global, regardless of local_index
	bool is_global = !defun.m_local_index.has_value() || is_instance_method;
	std::optional<int> index = std::nullopt;
	bool is_generic = !defun.m_generic_params.empty();

	if (is_generic && is_global)
	{
		m_generic_functions.emplace(defun.m_name.m_lexeme, GenericFunctionInfo(defun.m_name.m_lexeme, defun.m_params, defun.m_param_types, defun.m_generic_params, defun.m_constraints, defun.m_return_type, std::shared_ptr<MidoriExpression>(std::move(defun.m_body)), defun.m_captured_count));
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

	EmitLoop(m_loop_contexts.top().m_continue_target, line);
}

void CodeGenerator::operator()(MidoriStatement::ForeignDefinition& foreign)
{
	int line = foreign.m_function_name.m_line;

	const MidoriType::FunctionType& type = foreign.m_type->GetType<MidoriType::FunctionType>();
	if (!(type.m_return_type->IsType<MidoriType::IntegerType>() || type.m_return_type->IsType<MidoriType::FloatType>() || type.m_return_type->IsType<MidoriType::BoolType>() || type.m_return_type->IsType<MidoriType::UnitType>() || type.m_return_type->IsType<MidoriType::TextType>() || type.m_return_type->IsType<MidoriType::ArrayType>() || type.m_return_type->IsType<MidoriType::ByteType>() || type.m_return_type->IsType<MidoriType::WordType>()))
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported return type for foreign function", foreign.m_function_name, m_file_name, m_source_lines));
		return;
	}

	std::optional<size_t> ffi_index = MidoriFFIRegistry::FindIndex(foreign.m_foreign_name);
	if (ffi_index.has_value())
	{
		m_ffi_indices[foreign.m_function_name.m_lexeme] = ffi_index.value();
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

void CodeGenerator::operator()(MidoriStatement::Class& class_stmt)
{
	std::unordered_set<std::string> method_names;

	for (const std::unique_ptr<MidoriStatement>& method : class_stmt.m_methods)
	{
		if (method->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			const MidoriStatement::FunctionDefinition& defun = method->GetStatement<MidoriStatement::FunctionDefinition>();
			method_names.insert(defun.m_name.m_lexeme);
		}
	}

	m_class_methods[class_stmt.m_name.m_lexeme] = std::move(method_names);
	return;
}

void CodeGenerator::operator()(MidoriStatement::Instance& instance_stmt)
{
	// Compile each instance method as a separate global function
	// The methods are already named with mangled names (e.g., "show_Show_Int") by the parser during instance declaration parsing
	for (const std::unique_ptr<MidoriStatement>& method : instance_stmt.m_methods)
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, **method);

		if (method->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			const MidoriStatement::FunctionDefinition& defun = method->GetStatement<MidoriStatement::FunctionDefinition>();
			std::vector<std::string>& instance_methods = m_class_instances[instance_stmt.m_class_name.m_lexeme];
			if (std::ranges::find(instance_methods, defun.m_name.m_lexeme) == instance_methods.cend())
			{
				instance_methods.emplace_back(defun.m_name.m_lexeme);
			}
		}
	}

	// Instance resolution happens during generic function specialization via m_method_resolution_map
	return;
}

void CodeGenerator::operator()(MidoriStatement::TypeAlias&)
{
	// Type aliases are resolved at compile time, no runtime code generation needed
	return;
}

void CodeGenerator::operator()(MidoriExpression::As& as)
{
	int line = as.m_as_keyword.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **as.m_expr);

	std::shared_ptr<MidoriType> from_type = as.m_from_type.lock();
	const std::shared_ptr<MidoriType>& target_type = as.m_to_type;

	// Handle conversions that use Convertable typeclass or involve type variables
	if (as.m_uses_convertable || from_type->IsType<MidoriType::TypeVariable>() || target_type->IsType<MidoriType::TypeVariable>())
	{
		// First, check if we're inside a specialized generic function and can resolve via method resolution map
		std::string qualified_method_name = "Convertable"s + std::string(NameSeparator) + "Convert"s;
		std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(qualified_method_name);

		if (resolution_it != m_method_resolution_map.end())
		{
			// Resolve using the method resolution map (we're inside a specialized generic function)
			// Get the concrete type for the expression being converted
			std::shared_ptr<MidoriType> concrete_from_type = GetConcreteTypeForExpression(as.m_expr);
			std::string from_type_str = concrete_from_type->ToString();
			std::string to_type_str = target_type->ToString();

			std::string resolved_method;
			bool found = false;
			for (const ResolvedMethodCandidate& candidate : resolution_it->second)
			{
				if (candidate.m_first_type_name == from_type_str && candidate.m_second_type_name == to_type_str && candidate.m_has_instance)
				{
					resolved_method = candidate.m_resolved_name;
					found = true;
					break;
				}
			}

			if (found)
			{
				if (EmitResolvedNameGetGlobal(resolved_method, line))
				{
					EmitByte(OpCode::CALL, line);
					EmitByte(static_cast<OpCode>(1), line);  // 1 parameter
					return;
				}
			}
		}

		// Not in a specialized context, try direct lookup for concrete Convertable instances
		if (!from_type->IsType<MidoriType::TypeVariable>() && !target_type->IsType<MidoriType::TypeVariable>())
		{
			std::string mangled_name = INTERNAL_NAME_PREFIX + std::string(CONVERT_MANGLED_PREFIX) + from_type->ToString() + "_"s + target_type->ToString();
			std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
			if (it != m_global_variables.end())
			{
				EmitVariable(it->second, OpCode::GET_GLOBAL, line);
				EmitByte(OpCode::CALL, line);
				EmitByte(static_cast<OpCode>(1), line);  // 1 parameter
				return;
			}
			else if (as.m_uses_convertable)
			{
				AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Convertable instance method '"s + mangled_name + "' not found"s, as.m_as_keyword, m_file_name, m_source_lines));
				return;
			}
		}
		else if (as.m_uses_convertable)
		{
			// Type variables without resolution - this shouldn't happen if type checking was correct
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Cannot resolve Convertable instance for type variables outside of specialization context"s, as.m_as_keyword, m_file_name, m_source_lines));
			return;
		}
	}

	// If we reach here with type variables, fall through to check for built-in conversions
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
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_FLOAT, line);
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
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_INT, line);
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
	else if (target_type->IsType<MidoriType::ByteType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_BYTE, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_BYTE, line);
		}
		else if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_BYTE, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to byte' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::WordType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_WORD, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_WORD, line);
		}
		else if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_WORD, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to word' instruction", as.m_as_keyword, m_file_name, m_source_lines));
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
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_INT, line);
			EmitByte(OpCode::INT_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_INT, line);
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
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::ADD_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::ADD_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::ADD_WORD, line);
			}
			else
			{
				EmitByte(OpCode::ADD_INTEGER, line);
			}
			break;
		}
		case Token::Name::DOUBLE_PLUS:
		{
			if (operand_type->IsType<MidoriType::TextType>())
			{
				EmitByte(OpCode::CONCAT_TEXT, line);
			}
			else
			{
				EmitByte(OpCode::CONCAT_ARRAY, line);
			}
			break;
		}
		case Token::Name::SINGLE_MINUS:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::SUBTRACT_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::SUBTRACT_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::SUBTRACT_WORD, line);
			}
			else
			{
				EmitByte(OpCode::SUBTRACT_INTEGER, line);
			}
			break;
		}
		case Token::Name::STAR:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::MULTIPLY_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::IntegerType>())
			{
				EmitByte(OpCode::MULTIPLY_INTEGER, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::MULTIPLY_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::MULTIPLY_WORD, line);
			}
			else
			{
				EmitByte(OpCode::DUP_ARRAY, line);
			}
			break;
		}
		case Token::Name::SLASH:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::DIVIDE_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::DIVIDE_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::DIVIDE_WORD, line);
			}
			else
			{
				EmitByte(OpCode::DIVIDE_INTEGER, line);
			}
			break;
		}
		case Token::Name::PERCENT:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::MODULO_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::MODULO_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::MODULO_WORD, line);
			}
			else
			{
				EmitByte(OpCode::MODULO_INTEGER, line);
			}
			break;
		}
		case Token::Name::LEFT_SHIFT:
		{
			if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::LEFT_SHIFT_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::LEFT_SHIFT_WORD, line);
			}
			else
			{
				EmitByte(OpCode::LEFT_SHIFT, line);
			}
			break;
		}
		case Token::Name::RIGHT_SHIFT:
		{
			if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::RIGHT_SHIFT_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::RIGHT_SHIFT_WORD, line);
			}
			else
			{
				EmitByte(OpCode::RIGHT_SHIFT, line);
			}
			break;
		}
		case Token::Name::LEFT_ANGLE:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::LESS_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::LESS_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::LESS_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::LESS_WORD, line);
				}
				else
				{
					EmitByte(OpCode::LESS_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::LESS_EQUAL:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::LESS_EQUAL_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::LESS_EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::LESS_EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::LESS_EQUAL_WORD, line);
				}
				else
				{
					EmitByte(OpCode::LESS_EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::RIGHT_ANGLE:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::GREATER_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::GREATER_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::GREATER_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::GREATER_WORD, line);
				}
				else
				{
					EmitByte(OpCode::GREATER_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::GREATER_EQUAL:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::GREATER_EQUAL_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::GREATER_EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::GREATER_EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::GREATER_EQUAL_WORD, line);
				}
				else
				{
					EmitByte(OpCode::GREATER_EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::BANG_EQUAL:
		{
			if (binary.m_uses_equatable)
			{
				EmitEquatableEquals(operand_type, line);
				EmitByte(OpCode::NOT, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::NOT_EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::NOT_EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::NOT_EQUAL_WORD, line);
				}
				else
				{
					EmitByte(OpCode::NOT_EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::DOUBLE_EQUAL:
		{
			if (binary.m_uses_equatable)
			{
				EmitEquatableEquals(operand_type, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::IntegerType>())
				{
					EmitByte(OpCode::EQUAL_INTEGER, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::EQUAL_WORD, line);
				}
				else if (operand_type->IsType<MidoriType::TextType>())
				{
					EmitByte(OpCode::EQUAL_TEXT, line);
				}
				else
				{
					EmitByte(OpCode::EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::SINGLE_AMPERSAND:
		{
			EmitByte(OpCode::BITWISE_AND, line);
			break;
		}
		case Token::Name::SINGLE_BAR:
		{
			EmitByte(OpCode::BITWISE_OR, line);
			break;
		}
		case Token::Name::CARET:
		{
			EmitByte(OpCode::BITWISE_XOR, line);
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
		[this](const std::unique_ptr<MidoriExpression>& elem)
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
	{
		if (GetConcreteTypeForExpression(unary.m_expr)->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::NEGATE_FLOAT, unary.m_op.m_line);
		}
		else
		{
			EmitByte(OpCode::NEGATE_INTEGER, unary.m_op.m_line);
		}
		break;
	}
	case Token::Name::SINGLE_PLUS:
	{
		break;
	}
	case Token::Name::BANG:
	{
		EmitByte(OpCode::NOT, unary.m_op.m_line);
		break;
	}
	case Token::Name::TILDE:
	{
		EmitByte(OpCode::BITWISE_NOT, unary.m_op.m_line);
		break;
	}
	case Token::Name::HASH:
	{
		EmitByte(OpCode::GET_ARRAY_LENGTH, unary.m_op.m_line);
		break;
	}
	default:
	{
		return;
	}
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

	bool is_generic_call = false;
	std::string function_name;
	std::optional<std::string> resolved_method_name = std::nullopt;

	if (call.m_callee->IsExpression<MidoriExpression::NameAccess>())
	{
		MidoriExpression::NameAccess& callee_name = call.m_callee->GetExpression<MidoriExpression::NameAccess>();
		function_name = callee_name.m_name.m_lexeme;

		std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(function_name);
		if (resolution_it != m_method_resolution_map.end())
		{
			resolved_method_name = ResolveMethodNameForCall(function_name, call, line);
			if (!resolved_method_name.has_value())
			{
				return;
			}
			function_name = resolved_method_name.value();
		}

		std::unordered_map<std::string, GenericFunctionInfo>::iterator generic_it = m_generic_functions.find(function_name);
		if (generic_it != m_generic_functions.end())
		{
			is_generic_call = true;
		}
		else if (function_name.find("::") != std::string::npos)
		{
			// Try suffix lookup for qualified names
			std::string suffix = function_name.substr(function_name.rfind("::") + 2);
			if (m_generic_functions.contains(suffix))
			{
				is_generic_call = true;
				function_name = suffix;
			}
		}
	}

	if (is_generic_call)
	{
		std::vector<std::shared_ptr<MidoriType>> concrete_arg_types;
		for (std::unique_ptr<MidoriExpression>& arg : call.m_arguments)
		{
			// Get concrete type if we're in a specialization context
			concrete_arg_types.push_back(GetConcreteTypeForExpression(arg));
		}

		int specialized_proc_index = SpecializeGenericFunction(function_name, concrete_arg_types, line);
		if (specialized_proc_index == -1)
		{
			return;
		}

		std::ranges::for_each
		(
			call.m_arguments,
			[this](std::unique_ptr<MidoriExpression>& param)
			{
				std::visit([this](auto&& arg) { (*this)(arg); }, **param);
			}
		);


		GenericFunctionInfo& generic_info = m_generic_functions[function_name];
		if (generic_info.m_captured_count == 0)
		{
			if (call.m_is_tail_call)
			{
				// Fallback to closure call for tail calls
				EmitByte(OpCode::MAKE_CLOSURE, line);
				EmitByte(static_cast<OpCode>(specialized_proc_index), line);
				EmitByte(OpCode::BIND_CAPTURES, line);
				EmitByte(static_cast<OpCode>(0), line);
				EmitByte(OpCode::TAIL_CALL, line);
				EmitByte(static_cast<OpCode>(arity), line);
			}
			else
			{
				EmitByte(OpCode::CALL_PROC, line);
				EmitByte(static_cast<OpCode>(specialized_proc_index), line);
				EmitByte(static_cast<OpCode>(arity), line);
			}
		}
		else
		{
			// Push the specialized closure
			EmitByte(OpCode::MAKE_CLOSURE, line);
			EmitByte(static_cast<OpCode>(specialized_proc_index), line);

			EmitByte(OpCode::BIND_CAPTURES, line);
			EmitByte(static_cast<OpCode>(generic_info.m_captured_count), line);

			if (call.m_is_tail_call)
			{
				EmitByte(OpCode::TAIL_CALL, line);
			}
			else
			{
				EmitByte(OpCode::CALL, line);
			}
			EmitByte(static_cast<OpCode>(arity), line);
		}
	}
	else
	{
		std::ranges::for_each
		(
			call.m_arguments,
			[this](std::unique_ptr<MidoriExpression>& param)
			{
				std::visit([this](auto&& arg) { (*this)(arg); }, **param);
			}
		);

		bool is_optimized_call = false;

		std::optional<size_t> ffi_index_opt = std::nullopt;
		if (call.m_is_foreign && call.m_callee->IsExpression<MidoriExpression::NameAccess>())
		{
			std::unordered_map<std::string, size_t>::iterator ffi_it = m_ffi_indices.find(function_name);
			if (ffi_it != m_ffi_indices.end())
			{
				ffi_index_opt = ffi_it->second;
			}
		}

		if (!ffi_index_opt.has_value() && !is_optimized_call)
		{
			if (resolved_method_name.has_value())
			{
				if (!EmitResolvedNameGetGlobal(resolved_method_name.value(), line))
				{
					return;
				}
			}
			else
			{
				std::visit([this](auto&& arg) { (*this)(arg); }, **call.m_callee);
			}
		}

		if (is_optimized_call)
		{
			// Already emitted CALL_PROC
		}
		else if (call.m_is_foreign)
		{
			uint8_t return_type_tag = 0;
			if (call.m_type_data->IsType<MidoriType::TextType>())
			{
				return_type_tag = 1;
			}
			else if (call.m_type_data->IsType<MidoriType::ArrayType>())
			{
				return_type_tag = 2;
			}

			if (ffi_index_opt.has_value())
			{
				EmitByte(OpCode::CALL_FOREIGN_INDEXED, line);
				EmitByte(static_cast<OpCode>(ffi_index_opt.value()), line);
				EmitByte(static_cast<OpCode>(arity), line);
				EmitByte(static_cast<OpCode>(return_type_tag), line);
			}
			else
			{
				EmitByte(OpCode::CALL_FOREIGN, line);
				EmitByte(static_cast<OpCode>(arity), line);
				EmitByte(static_cast<OpCode>(return_type_tag), line);
			}
		}
		else if (call.m_is_tail_call)
		{
			EmitByte(OpCode::TAIL_CALL, line);
			EmitByte(static_cast<OpCode>(arity), line);
		}
		else
		{
			EmitByte(OpCode::CALL, line);
			EmitByte(static_cast<OpCode>(arity), line);
		}
	}
}

void CodeGenerator::operator()(MidoriExpression::MemberAccess& get)
{
	int line = get.m_member_name.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **get.m_struct);
	EmitByte(OpCode::GET_MEMBER, line);
	EmitByte(static_cast<OpCode>(get.m_index), line);
}

void CodeGenerator::operator()(MidoriExpression::MemberAssignment& set)
{
	int line = set.m_member_name.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **set.m_struct);
	std::visit([this](auto&& arg){ (*this)(arg); }, **set.m_value);
	EmitByte(OpCode::SET_MEMBER, line);
	EmitByte(static_cast<OpCode>(set.m_index), line);
}

void CodeGenerator::operator()(MidoriExpression::NameAccess& variable)
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
				const std::string& name = variable.m_name.m_lexeme;

				std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(name);
				if (resolution_it != m_method_resolution_map.end())
				{
					const std::vector<ResolvedMethodCandidate>& candidates = resolution_it->second;
					if (candidates.size() != 1u)
					{
						AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': cannot use method value when multiple class constraints are in scope.", name), line, m_file_name, m_source_lines));
						return;
					}
					if (!candidates[0u].m_has_instance)
					{
						AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Unresolved method '{}': no matching instance found.", name), line, m_file_name, m_source_lines));
						return;
					}
					EmitResolvedNameGetGlobal(candidates[0u].m_resolved_name, line);
					return;
				}

				if (name.find(NameSeparator) != std::string::npos)
				{
					// This is an imported symbol - assign a negative index as placeholder
					// Track this import for linker patching
					size_t separator_pos = name.find(NameSeparator);
					std::string module_name = name.substr(0u, separator_pos);
					std::string symbol_name = name.substr(separator_pos + 2u);

					// Check if we've already tracked this import
					int import_index = 0;
					bool found = false;
					for (size_t i = 0u; i < m_tracked_imports.size(); i += 1u)
					{
						if (m_tracked_imports[i].m_from_module == module_name && m_tracked_imports[i].m_name == symbol_name)
						{
							import_index = -(static_cast<int>(i) + 1);  // -1, -2, -3, etc.
							found = true;
							break;
						}
					}

					if (!found)
					{
						m_tracked_imports.emplace_back(symbol_name, module_name);
						import_index = -static_cast<int>(m_tracked_imports.size());  // -1 for first import, -2 for second, etc.
					}

					// Emit negative index (will be patched by linker to positive global index)
					// Cast to uint8_t will wrap negative values (e.g., -1 becomes 255, -2 becomes 254)
					EmitVariable(static_cast<uint8_t>(import_index), OpCode::GET_GLOBAL, line);
				}
				else
				{
					// Regular local global variable
					EmitVariable(m_global_variables[name], OpCode::GET_GLOBAL, line);
				}
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

void CodeGenerator::operator()(MidoriExpression::AppendAssign& append_assign)
{
	int line = append_assign.m_name.m_line;

	std::visit([&append_assign, line, this](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Local>)
			{
				EmitVariable(arg.m_index, OpCode::GET_LOCAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Global>)
			{
				const std::string& name = append_assign.m_name.m_lexeme;
				EmitVariable(m_global_variables[name], OpCode::GET_GLOBAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Cell>)
			{
				EmitVariable(arg.m_index, OpCode::GET_CELL, line);
			}
		},
		append_assign.m_name_ctx
	);

	std::visit([this](auto&& arg){ (*this)(arg); }, **append_assign.m_value);

	if (append_assign.m_type_data->IsType<MidoriType::ArrayType>())
	{
		EmitByte(OpCode::APPEND_ARRAY, line);
	}
	else if (append_assign.m_type_data->IsType<MidoriType::TextType>())
	{
		EmitByte(OpCode::APPEND_TEXT, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::PrependAssign& prepend_assign)
{
	int line = prepend_assign.m_name.m_line;

	std::visit([&prepend_assign, line, this](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Local>)
			{
				EmitVariable(arg.m_index, OpCode::GET_LOCAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Global>)
			{
				const std::string& name = prepend_assign.m_name.m_lexeme;
				EmitVariable(m_global_variables[name], OpCode::GET_GLOBAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Cell>)
			{
				EmitVariable(arg.m_index, OpCode::GET_CELL, line);
			}
		},
		prepend_assign.m_name_ctx
	);

	std::visit([this](auto&& arg){ (*this)(arg); }, **prepend_assign.m_value);

	if (prepend_assign.m_type_data->IsType<MidoriType::ArrayType>())
	{
		EmitByte(OpCode::PREPEND_ARRAY, line);
	}
	else if (prepend_assign.m_type_data->IsType<MidoriType::TextType>())
	{
		EmitByte(OpCode::PREPEND_TEXT, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::CompoundAssign& compound_assign)
{
	int line = compound_assign.m_name.m_line;

	std::visit([&compound_assign, line, this](auto&& arg)
		{
			using T = std::decay_t<decltype(arg)>;

			if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Local>)
			{
				EmitVariable(arg.m_index, OpCode::GET_LOCAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Global>)
			{
				const std::string& name = compound_assign.m_name.m_lexeme;
				EmitVariable(m_global_variables[name], OpCode::GET_GLOBAL, line);
			}
			else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Cell>)
			{
				EmitVariable(arg.m_index, OpCode::GET_CELL, line);
			}
		},
		compound_assign.m_name_ctx
	);

	std::visit([this](auto&& arg){ (*this)(arg); }, **compound_assign.m_value);

	bool is_float = compound_assign.m_type_data->IsType<MidoriType::FloatType>();
	switch (compound_assign.m_op.m_token_name)
	{
	case Token::Name::PLUS_EQUAL:
		EmitByte(is_float ? OpCode::ADD_ASSIGN_FLOAT : OpCode::ADD_ASSIGN_INT, line);
		break;
	case Token::Name::MINUS_EQUAL:
		EmitByte(is_float ? OpCode::SUB_ASSIGN_FLOAT : OpCode::SUB_ASSIGN_INT, line);
		break;
	case Token::Name::STAR_EQUAL:
		EmitByte(is_float ? OpCode::MUL_ASSIGN_FLOAT : OpCode::MUL_ASSIGN_INT, line);
		break;
	case Token::Name::SLASH_EQUAL:
		EmitByte(is_float ? OpCode::DIV_ASSIGN_FLOAT : OpCode::DIV_ASSIGN_INT, line);
		break;
	case Token::Name::PERCENT_EQUAL:
		EmitByte(is_float ? OpCode::MOD_ASSIGN_FLOAT : OpCode::MOD_ASSIGN_INT, line);
		break;
	case Token::Name::AMPERSAND_EQUAL:
		EmitByte(OpCode::AND_ASSIGN_INT, line);
		break;
	case Token::Name::BAR_EQUAL:
		EmitByte(OpCode::OR_ASSIGN_INT, line);
		break;
	case Token::Name::CARET_EQUAL:
		EmitByte(OpCode::XOR_ASSIGN_INT, line);
		break;
	case Token::Name::LEFT_SHIFT_EQUAL:
		EmitByte(OpCode::LEFT_SHIFT_ASSIGN, line);
		break;
	case Token::Name::RIGHT_SHIFT_EQUAL:
		EmitByte(OpCode::RIGHT_SHIFT_ASSIGN, line);
		break;
	}
}

void CodeGenerator::operator()(MidoriExpression::Assignment& bind)
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
				const std::string& name = bind.m_name.m_lexeme;

				if (name.find(NameSeparator) != std::string::npos)
				{
					// This is an imported symbol - assign a negative index as placeholder
					size_t separator_pos = name.find(NameSeparator);
					std::string module_name = name.substr(0u, separator_pos);
					std::string symbol_name = name.substr(separator_pos + 2u);

					// Check if we've already tracked this import
					int import_index = 0;
					bool found = false;
					for (size_t i = 0u; i < m_tracked_imports.size(); i += 1u)
					{
						if (m_tracked_imports[i].m_from_module == module_name &&
							m_tracked_imports[i].m_name == symbol_name)
						{
							import_index = -(static_cast<int>(i) + 1);  // -1, -2, -3, etc.
							found = true;
							break;
						}
					}

					if (!found)
					{
						m_tracked_imports.emplace_back(symbol_name, module_name);
						import_index = -static_cast<int>(m_tracked_imports.size());  // -1 for first import, -2 for second, etc.
					}

					// Emit negative index (will be patched by linker)
					EmitVariable(static_cast<uint8_t>(import_index), OpCode::SET_GLOBAL, line);
				}
				else
				{
					// Regular local global variable
					EmitVariable(m_global_variables[name], OpCode::SET_GLOBAL, line);
				}
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
	try
	{
		EmitIntegerConstant(std::stoll(integer.m_token.m_lexeme), line);
	}
	catch (const std::out_of_range&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Integer literal '" + integer.m_token.m_lexeme + "' is out of range. Maximum value is 9223372036854775807 (2^63 - 1), minimum value is -9223372036854775807.", integer.m_token, m_file_name, m_source_lines));
	}
	catch (const std::invalid_argument&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid integer literal '" + integer.m_token.m_lexeme + "'", integer.m_token, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::ByteLiteral& byte_literal)
{
	int line = byte_literal.m_token.m_line;
	const std::string& lexeme = byte_literal.m_token.m_lexeme;
	try
	{
		uint64_t value = 0u;
		if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X'))
		{
			value = std::stoull(lexeme, nullptr, 16);
		}
		else if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'b' || lexeme[1u] == 'B'))
		{
			value = std::stoull(lexeme, nullptr, 2);
		}
		else
		{
			value = std::stoull(lexeme);
		}

		if (value > 0xFF)
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Byte literal '" + lexeme + "' is out of range. Maximum value is 255 (0xFF).", byte_literal.m_token, m_file_name, m_source_lines));
			return;
		}
		EmitByteConstant(static_cast<MidoriByte>(value), line);
	}
	catch (const std::out_of_range&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Byte literal '" + lexeme + "' is out of range. Maximum value is 255 (0xFF).", byte_literal.m_token, m_file_name, m_source_lines));
	}
	catch (const std::invalid_argument&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid byte literal '" + lexeme + "'", byte_literal.m_token, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::WordLiteral& word_literal)
{
	int line = word_literal.m_token.m_line;
	const std::string& lexeme = word_literal.m_token.m_lexeme;
	try
	{
		uint64_t value = 0u;
		if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X'))
		{
			value = std::stoull(lexeme, nullptr, 16);
		}
		else if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'b' || lexeme[1u] == 'B'))
		{
			value = std::stoull(lexeme, nullptr, 2);
		}
		else
		{
			value = std::stoull(lexeme);
		}
		EmitWordConstant(value, line);
	}
	catch (const std::out_of_range&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Word literal '" + lexeme + "' is out of range. Maximum value is 18446744073709551615 (0xFFFFFFFFFFFFFFFF).", word_literal.m_token, m_file_name, m_source_lines));
	}
	catch (const std::invalid_argument&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid word literal '" + lexeme + "'", word_literal.m_token, m_file_name, m_source_lines));
	}
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

void CodeGenerator::operator()(MidoriExpression::IndexAccess& array_get)
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

void CodeGenerator::operator()(MidoriExpression::IndexAssignment& array_set)
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

void CodeGenerator::operator()(MidoriExpression::RangeBinary& range_binary)
{
	int line = range_binary.m_range_op.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **range_binary.m_start);

	// Generate code for default step (1 for Int, 1.0 for Float)
	if (range_binary.m_type_data->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::IntegerType>())
	{
		EmitByte(OpCode::INT_1, line);
	}
	else
	{
		EmitFloatConstant(1.0, line);
	}

	std::visit([this](auto&& arg){ (*this)(arg); }, **range_binary.m_end);

	if (range_binary.m_type_data->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::IntegerType>())
	{
		EmitByte(OpCode::CREATE_INT_RANGE, line);
	}
	else
	{
		EmitByte(OpCode::CREATE_FLOAT_RANGE, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::RangeTernary& range_ternary)
{
	int line = range_ternary.m_first_range_op.m_line;

	std::visit([this](auto&& arg){ (*this)(arg); }, **range_ternary.m_start);
	std::visit([this](auto&& arg){ (*this)(arg); }, **range_ternary.m_step);
	std::visit([this](auto&& arg){ (*this)(arg); }, **range_ternary.m_end);

	if (range_ternary.m_type_data->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::IntegerType>())
	{
		EmitByte(OpCode::CREATE_INT_RANGE, line);
	}
	else
	{
		EmitByte(OpCode::CREATE_FLOAT_RANGE, line);
	}
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
			EmitByte(static_cast<OpCode>(BYTE_MASK), line);
			EmitByte(static_cast<OpCode>(BYTE_MASK), line);
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
			m_procedures[m_current_procedure_index].SetByteCode(case_offset_positions[i], static_cast<OpCode>(offset_from_table & BYTE_MASK));
			m_procedures[m_current_procedure_index].SetByteCode(case_offset_positions[i] + 1, static_cast<OpCode>((offset_from_table >> SHIFT_8_BITS) & BYTE_MASK));

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

void CodeGenerator::operator()(MidoriExpression::For& for_expr)
{
	int line = for_expr.m_for_keyword.m_line;

	if (for_expr.m_is_array_iteration)
	{
		// Array iteration
		// The parser has reserved 4 local variable slots:
		// for_expr.m_loop_variable_index: the element value
		// for_expr.m_hidden_step_index: current index (reused)
		// for_expr.m_hidden_end_index: array length (reused)
		// for_expr.m_hidden_array_index: array reference

		// Update m_local_count to account for the 4 reserved locals
		if (m_local_count < for_expr.m_hidden_array_index + 1)
		{
			m_local_count = for_expr.m_hidden_array_index + 1;
		}

		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // loop variable (element)
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // index
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // length
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // array ref

		// Evaluate array expression
		std::visit([this](auto&& arg) { (*this)(arg); }, **for_expr.m_range);
		// Stack: [0, 0, 0, 0, array]

		// Store array reference
		EmitByte(OpCode::DUP, line);
		EmitVariable(for_expr.m_hidden_array_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
		// Stack: [0, 0, 0, array, array]

		// Get and store array length
		EmitByte(OpCode::GET_ARRAY_LENGTH, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
		// Stack: [0, 0, length, array]

		// Initialize index to 0
		EmitByte(OpCode::INT_0, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
		// Stack: [0, 0, length, array]

		// Jump to condition check
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment index
		int continue_target = m_procedures[m_current_procedure_index].GetByteCodeSize();
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::INT_1, line);
		EmitByte(OpCode::ADD_INTEGER, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);
		int loop_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
		BeginLoop(loop_start);

		m_loop_contexts.top().m_continue_target = continue_target;

		// Check if index < length
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::GET_LOCAL, line);
		int exit_jump = EmitJump(OpCode::IF_INTEGER_LESS, line);

		// Get element at current index and store in loop variable
		EmitVariable(for_expr.m_hidden_array_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::GET_ARRAY, line);
		EmitByte(static_cast<OpCode>(1), line);  // 1 index dimension
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Execute body
		std::visit([this](auto&& arg) { (*this)(arg); }, **for_expr.m_body);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(exit_jump, line);

		EmitByte(OpCode::POP, line);  // Pop array ref
		EmitByte(OpCode::POP, line);  // Pop length
		EmitByte(OpCode::POP, line);  // Pop index
		EmitByte(OpCode::POP, line);  // Pop loop variable

		// Push unit value as result for normal loop exit
		EmitByte(OpCode::OP_UNIT, line);

		EndLoop(line);
	}
	else
	{
		// Range iteration (original code)
		bool is_float = for_expr.m_range->GetType()->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::FloatType>();

		// The parser has reserved 4 local variable slots (but we only use 3 for range)
		// for_expr.m_loop_variable_index: the loop variable
		// for_expr.m_hidden_step_index: hidden step value
		// for_expr.m_hidden_end_index: hidden end value

		// Update m_local_count to account for the 4 reserved locals
		if (m_local_count < for_expr.m_hidden_array_index + 1)
		{
			m_local_count = for_expr.m_hidden_array_index + 1;
		}

		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // Extra placeholder for consistency

		std::visit([this](auto&& arg) { (*this)(arg); }, **for_expr.m_range);
		// Stack: [0, 0, 0, 0, range]

		// Duplicate range for each extraction
		EmitByte(OpCode::DUP, line);
		EmitByte(OpCode::DUP, line);
		// Stack: [0, 0, 0, 0, range, range, range]

		EmitByte(OpCode::GET_RANGE_START, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_STEP, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_END, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Jump to condition check (skip increment on first iteration)
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment loop variable before checking condition
		int continue_target = m_procedures[m_current_procedure_index].GetByteCodeSize();
		EmitVariable(for_expr.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(is_float ? OpCode::ADD_FLOAT : OpCode::ADD_INTEGER, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);
		int loop_start = m_procedures[m_current_procedure_index].GetByteCodeSize();
		BeginLoop(loop_start);

		m_loop_contexts.top().m_continue_target = continue_target;

		// Runtime check for step direction to support both forward and backward iteration
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		if (is_float)
		{
			EmitFloatConstant(0.0, line);
		}
		else
		{
			EmitByte(OpCode::INT_0, line);
		}

		OpCode step_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_jump = EmitJump(step_comparison, line);

		// Forward iteration (step > 0): exit loop if i >= end
		EmitVariable(for_expr.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode forward_comparison = is_float ? OpCode::IF_FLOAT_LESS : OpCode::IF_INTEGER_LESS;
		int forward_exit_jump = EmitJump(forward_comparison, line);

		int body_jump = EmitJump(OpCode::JUMP, line);

		// Backward iteration (step <= 0): exit loop if i <= end
		PatchJump(backward_jump, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode backward_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_exit_jump = EmitJump(backward_comparison, line);

		PatchJump(body_jump, line);
		std::visit([this](auto&& arg) { (*this)(arg); }, **for_expr.m_body);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(forward_exit_jump, line);
		PatchJump(backward_exit_jump, line);

		EmitByte(OpCode::POP, line);  // Pop unused array slot
		EmitByte(OpCode::POP, line);  // Pop end
		EmitByte(OpCode::POP, line);  // Pop step
		EmitByte(OpCode::POP, line);  // Pop loop variable

		// Push unit value as result for normal loop exit
		EmitByte(OpCode::OP_UNIT, line);

		EndLoop(line);
	}
}

void CodeGenerator::operator()(MidoriExpression::ArrayComprehension& comp)
{
	int line = comp.m_bracket.m_line;

	// Update m_local_count to account for the 5 reserved locals
	if (m_local_count < comp.m_result_array_index + 1)
	{
		m_local_count = comp.m_result_array_index + 1;
	}

	// Push 5 placeholders for: loop_var, step/index, end/length, array_ref, result_array
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // loop variable
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // step/index
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // end/length
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // array ref (for array iteration)
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // result array

	// Create empty result array and store it
	EmitByte(OpCode::CREATE_ARRAY, line);
	EmitThreeBytes(0, 0, 0, line);  // Empty array with 0 elements
	EmitVariable(comp.m_result_array_index, OpCode::SET_LOCAL, line);
	EmitByte(OpCode::POP, line);

	if (comp.m_is_array_iteration)
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, **comp.m_range);

		// Store array reference
		EmitByte(OpCode::DUP, line);
		EmitVariable(comp.m_hidden_array_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Get and store array length
		EmitByte(OpCode::GET_ARRAY_LENGTH, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Initialize index to 0
		EmitByte(OpCode::INT_0, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Jump to condition check
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment index
		int continue_target = m_procedures[m_current_procedure_index].GetByteCodeSize();
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::INT_1, line);
		EmitByte(OpCode::ADD_INTEGER, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);

		// Check if index < length
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::GET_LOCAL, line);
		int exit_jump = EmitJump(OpCode::IF_INTEGER_LESS, line);

		// Get element at current index and store in loop variable
		EmitVariable(comp.m_hidden_array_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::GET_ARRAY, line);
		EmitByte(static_cast<OpCode>(1), line);  // 1 index dimension
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Evaluate transform expression first, then get result array, swap and append
		std::visit([this](auto&& arg) { (*this)(arg); }, **comp.m_transform_expr);
		EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::SWAP, line);
		EmitByte(OpCode::ADD_BACK_ARRAY, line);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(exit_jump, line);
	}
	else
	{
		bool is_float = comp.m_range->GetType()->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::FloatType>();

		std::visit([this](auto&& arg) { (*this)(arg); }, **comp.m_range);

		// Duplicate range for each extraction
		EmitByte(OpCode::DUP, line);
		EmitByte(OpCode::DUP, line);

		EmitByte(OpCode::GET_RANGE_START, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_STEP, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_END, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Jump to condition check
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment loop variable
		int continue_target = m_procedures[m_current_procedure_index].GetByteCodeSize();
		EmitVariable(comp.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(is_float ? OpCode::ADD_FLOAT : OpCode::ADD_INTEGER, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);

		// Runtime check for step direction
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		if (is_float)
		{
			EmitFloatConstant(0.0, line);
		}
		else
		{
			EmitByte(OpCode::INT_0, line);
		}

		OpCode step_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_jump = EmitJump(step_comparison, line);

		// Forward iteration (step > 0): exit loop if i >= end
		EmitVariable(comp.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode forward_comparison = is_float ? OpCode::IF_FLOAT_LESS : OpCode::IF_INTEGER_LESS;
		int forward_exit_jump = EmitJump(forward_comparison, line);

		int body_jump = EmitJump(OpCode::JUMP, line);

		// Backward iteration (step <= 0): exit loop if i <= end
		PatchJump(backward_jump, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode backward_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_exit_jump = EmitJump(backward_comparison, line);

		PatchJump(body_jump, line);

		// Evaluate transform expression first, then get result array, swap and append
		std::visit([this](auto&& arg) { (*this)(arg); }, **comp.m_transform_expr);
		EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::SWAP, line);
		EmitByte(OpCode::ADD_BACK_ARRAY, line);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(forward_exit_jump, line);
		PatchJump(backward_exit_jump, line);
	}

	// Clean up: pop 4 placeholders and the result placeholder, then push result array
	// Stack: [..., loop_var, step, end, array_ref, result_placeholder]
	EmitByte(OpCode::POP, line);  // Pop array ref / unused slot
	EmitByte(OpCode::POP, line);  // Pop end/length
	EmitByte(OpCode::POP, line);  // Pop step/index
	EmitByte(OpCode::POP, line);  // Pop loop variable
	// Stack: [..., result_placeholder]

	// Pop the result placeholder and push the actual result array
	EmitByte(OpCode::POP, line);  // Pop result_placeholder
	// Stack: [...]

	// Push the result array as the expression value
	EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
	// Stack: [..., result_array]
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

void CodeGenerator::operator()(MidoriExpression::Async& async_expr)
{
	int line = async_expr.m_keyword.m_line;
	int captured_count = async_expr.m_captured_count;

	if (captured_count > MAX_CAPTURED_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many captured variables (max {})", MAX_CAPTURED_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	size_t prev_index = m_current_procedure_index;
	m_current_procedure_index = m_procedures.size();
	m_procedures.emplace_back();

	std::visit([this](auto&& arg) { (*this)(arg); }, **async_expr.m_expr);

	EmitByte(OpCode::ASYNC_RETURN, line);

	size_t async_proc_index = m_current_procedure_index;

	std::string full_name = "async_task@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_procedure_names.emplace_back(full_name.c_str());

	m_current_procedure_index = prev_index;

	if (m_current_procedure_index > MAX_FUNCTION_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many functions (max {})", MAX_FUNCTION_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	EmitByte(OpCode::MAKE_CLOSURE, line);
	EmitByte(static_cast<OpCode>(async_proc_index), line);

	EmitByte(OpCode::BIND_CAPTURES, line);
	EmitByte(static_cast<OpCode>(captured_count), line);

	EmitByte(OpCode::SPAWN_ASYNC, line);
}

void CodeGenerator::operator()(MidoriExpression::Await& await_expr)
{
	int line = await_expr.m_keyword.m_line;
	std::visit([this](auto&& arg) { (*this)(arg); }, **await_expr.m_expr);
	EmitByte(OpCode::AWAIT_FUTURE, line);
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

// Helper for type deduction
void CodeGenerator::DeduceGenericTypesRecursive(const std::shared_ptr<MidoriType>& param_type, const std::shared_ptr<MidoriType>& concrete_type, std::unordered_map<std::string, std::shared_ptr<MidoriType>>& map, std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash>& visited)
{
	if (!param_type || !concrete_type)
	{
		return;
	}
	if (param_type.get() == concrete_type.get())
	{
		return;
	}
	if (visited.contains({param_type.get(), concrete_type.get()}))
	{
		return;
	}
	visited.insert({ param_type.get(), concrete_type.get() });

	std::visit
	(
		[&param_type, &concrete_type, &map, &visited, this](auto&& p_var)
		{
			using T = std::decay_t<decltype(p_var)>;

			if constexpr (std::is_same_v<T, MidoriType::GenericParam>)
			{
				map[p_var.m_name] = concrete_type;
			}
			else if constexpr (std::is_same_v<T, MidoriType::TypeVariable>)
			{
				map[param_type->ToString()] = concrete_type;
			}
			else if constexpr (std::is_same_v<T, MidoriType::ArrayType>)
			{
				if (concrete_type->IsType<MidoriType::ArrayType>())
				{
					DeduceGenericTypesRecursive(p_var.m_element_type, concrete_type->GetType<MidoriType::ArrayType>().m_element_type, map, visited);
				}
			}
			else if constexpr (std::is_same_v<T, MidoriType::StructType>)
			{
				if (concrete_type->IsType<MidoriType::StructType>())
				{
					const MidoriType::StructType& c_struct = concrete_type->GetType<MidoriType::StructType>();
					if (p_var.m_member_types.size() == c_struct.m_member_types.size())
					{
						for (size_t i = 0uz; i < p_var.m_member_types.size(); i += 1uz)
						{
							DeduceGenericTypesRecursive(p_var.m_member_types[i], c_struct.m_member_types[i], map, visited);
						}
					}
				}
			}
			else if constexpr (std::is_same_v<T, MidoriType::FunctionType>)
			{
				if (concrete_type->IsType<MidoriType::FunctionType>())
				{
					const MidoriType::FunctionType& c_func = concrete_type->GetType<MidoriType::FunctionType>();
					DeduceGenericTypesRecursive(p_var.m_return_type, c_func.m_return_type, map, visited);
					if (p_var.m_param_types.size() == c_func.m_param_types.size())
					{
						for (size_t i = 0uz; i < p_var.m_param_types.size(); i += 1uz)
						{
							DeduceGenericTypesRecursive(p_var.m_param_types[i], c_func.m_param_types[i], map, visited);
						}
					}
				}
			}
			else if constexpr (std::is_same_v<T, MidoriType::TupleType>)
			{
				if (concrete_type->IsType<MidoriType::TupleType>())
				{
					const MidoriType::TupleType& c_tuple = concrete_type->GetType<MidoriType::TupleType>();
					if (p_var.m_element_types.size() == c_tuple.m_element_types.size())
					{
						for (size_t i = 0uz; i < p_var.m_element_types.size(); i += 1uz)
						{
							DeduceGenericTypesRecursive(p_var.m_element_types[i], c_tuple.m_element_types[i], map, visited);
						}
					}
				}
			}
			else if constexpr (std::is_same_v<T, MidoriType::UnionType>)
			{
				if (concrete_type->IsType<MidoriType::UnionType>())
				{
					const MidoriType::UnionType& c_union = concrete_type->GetType<MidoriType::UnionType>();
					for (const auto& [name, ctx] : p_var.m_member_info)
					{
						if (c_union.m_member_info.contains(name))
						{
							const MidoriType::UnionType::UnionMemberContext& c_ctx = c_union.m_member_info.at(name);
							if (ctx.m_member_types.size() == c_ctx.m_member_types.size())
							{
								for (size_t i = 0uz; i < ctx.m_member_types.size(); i += 1uz)
								{
									DeduceGenericTypesRecursive(ctx.m_member_types[i], c_ctx.m_member_types[i], map, visited);
								}
							}
						}
					}
				}
			}
		},
		param_type->m_type
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
	TypeEnvironment prev_param_map = m_param_type_map;
	m_param_type_map.clear();

	for (size_t i = 0u; i < generic_info.m_params.size() && i < concrete_arg_types.size(); i += 1u)
	{
		m_param_type_map[generic_info.m_params[i].m_lexeme] = concrete_arg_types[i];
	}

	// Build generic type parameter -> concrete type map
	// Use deep deduction to match generic parameters and TypeVariables nested in arguments
	TypeEnvironment prev_generic_type_map = m_generic_type_substitution;
	m_generic_type_substitution.clear();
	TypeEnvironment& generic_type_map = m_generic_type_substitution;

	std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash> visited;

	for (size_t i = 0u; i < generic_info.m_param_types.size() && i < concrete_arg_types.size(); i += 1u)
	{
		DeduceGenericTypesRecursive(generic_info.m_param_types[i], concrete_arg_types[i], generic_type_map, visited);
	}

	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>> prev_resolution_map = m_method_resolution_map;
	m_method_resolution_map.clear();

	for (const MidoriType::ClassConstraint& constraint : generic_info.m_constraints)
	{
		TypeclassMethodMap::iterator tc_it = m_class_methods.find(constraint.m_class_name);
		if (tc_it != m_class_methods.end())
		{
			std::vector<std::shared_ptr<MidoriType>> concrete_type_args;
			concrete_type_args.reserve(constraint.m_type_args.size());
			for (const std::shared_ptr<MidoriType>& type_arg : constraint.m_type_args)
			{
				concrete_type_args.emplace_back(SubstituteGenericTypes(type_arg, generic_type_map));
			}

			std::string first_type_name;
			std::string second_type_name;
			if (!concrete_type_args.empty())
			{
				first_type_name = concrete_type_args[0u]->ToString();
			}
			if (concrete_type_args.size() > 1u)
			{
				second_type_name = concrete_type_args[1u]->ToString();
			}

			for (const std::string& method_name : tc_it->second)
			{
				std::string qualified_method_name = constraint.m_class_name + std::string(NameSeparator) + method_name;

				std::string mangled_name_prefix = MidoriType::MangleInstanceMethodName(method_name, constraint.m_class_name, concrete_type_args);
				std::string resolved_method_name = mangled_name_prefix;
				bool instance_found = false;

				TypeclassInstanceMap::iterator instances_it = m_class_instances.find(constraint.m_class_name);
				if (instances_it != m_class_instances.end())
				{
					std::string pattern_with_at = mangled_name_prefix + ModuleSeparator;
					for (const std::string& instance_method : instances_it->second)
					{
						if (instance_method == mangled_name_prefix || instance_method.starts_with(pattern_with_at))
						{
							resolved_method_name = instance_method;
							instance_found = true;
							break;
						}
					}
				}

				ResolvedMethodCandidate candidate;
				candidate.m_first_type_name = first_type_name;
				candidate.m_second_type_name = second_type_name;
				candidate.m_resolved_name = resolved_method_name;
				candidate.m_has_instance = instance_found;

				m_method_resolution_map[qualified_method_name].emplace_back(std::move(candidate));
			}
		}
	}

	size_t prev_index = m_current_procedure_index;
	m_current_procedure_index = m_procedures.size();
	int specialized_proc_index = static_cast<int>(m_current_procedure_index);
	m_procedures.emplace_back();
	m_specialized_functions[signature] = specialized_proc_index;

	std::visit([this](auto&& arg) { (*this)(arg); }, **generic_info.m_body);
	EmitByte(OpCode::RETURN, line);

	std::string full_specialized_name = specialized_name + "@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_procedure_names.emplace_back(full_specialized_name.c_str());

	m_current_procedure_index = prev_index;

	m_param_type_map = std::move(prev_param_map);
	m_method_resolution_map = std::move(prev_resolution_map);
	m_generic_type_substitution = std::move(prev_generic_type_map);
	return specialized_proc_index;
}

std::optional<std::string> CodeGenerator::ResolveMethodNameForCall(const std::string& callee_name, const MidoriExpression::Call& call, int line)
{
	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator it = m_method_resolution_map.find(callee_name);
	if (it == m_method_resolution_map.end())
	{
		return std::nullopt;
	}

	const std::vector<ResolvedMethodCandidate>& candidates = it->second;
	if (candidates.empty())
	{
		return std::nullopt;
	}

	if (call.m_arguments.empty())
	{
		if (candidates.size() == 1u && candidates[0u].m_has_instance)
		{
			return candidates[0u].m_resolved_name;
		}

		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': cannot resolve a method call with no arguments.", callee_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	if (call.m_arguments.empty())
	{
		if (candidates.size() == 1u && candidates[0u].m_has_instance)
		{
			return candidates[0u].m_resolved_name;
		}

		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': cannot resolve a method call with no arguments.", callee_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	std::shared_ptr<MidoriType> first_arg_type = GetConcreteTypeForExpression(call.m_arguments[0u]);
	std::string first_arg_type_name = first_arg_type->ToString();
	std::string return_type_name = call.m_type_data->ToString();

	std::vector<const ResolvedMethodCandidate*> matching;
	for (const ResolvedMethodCandidate& candidate : candidates)
	{
		if (candidate.m_first_type_name == first_arg_type_name)
		{
			matching.emplace_back(&candidate);
		}
	}

	if (matching.empty())
	{
		std::string candidates_info;
		for (const ResolvedMethodCandidate& candidate : candidates)
		{
			candidates_info += std::format("\nCandidate: {} (First: '{}', Instance: {})", candidate.m_resolved_name, candidate.m_first_type_name, candidate.m_has_instance);
		}
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': no constraint matches argument type '{}'. Candidates:{}", callee_name, first_arg_type_name, candidates_info), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	const bool return_type_is_concrete =
		!return_type_name.empty() &&
		return_type_name != "Undecided"s &&
		!(return_type_name.size() > 1u && return_type_name[0u] == 'T' && std::isdigit(static_cast<char>(return_type_name[1u])) != 0);

	if (matching.size() > 1u && return_type_is_concrete)
	{
		std::vector<const ResolvedMethodCandidate*> matching_by_return;
		for (const ResolvedMethodCandidate* candidate : matching)
		{
			if (!candidate->m_second_type_name.empty() && candidate->m_second_type_name == return_type_name)
			{
				matching_by_return.emplace_back(candidate);
			}
		}
		if (!matching_by_return.empty())
		{
			matching = std::move(matching_by_return);
		}
	}

	if (matching.size() != 1u)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': multiple constraints match argument type '{}'. Make constraints more specific.", callee_name, first_arg_type_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	const ResolvedMethodCandidate& selected = *matching[0u];
	if (!selected.m_has_instance)
	{
		std::string suffix;
		if (!selected.m_first_type_name.empty())
		{
			suffix = std::format(" (constraint types: {}", selected.m_first_type_name);
			if (!selected.m_second_type_name.empty())
			{
				suffix.append(", "s).append(selected.m_second_type_name);
			}
			suffix.append(")"s);
		}
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Unresolved method '{}': no matching instance found{}.", callee_name, suffix), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	return selected.m_resolved_name;
}

bool CodeGenerator::EmitResolvedNameGetGlobal(const std::string& resolved_name, int line)
{
	size_t at_pos = resolved_name.find(ModuleSeparator);
	if (at_pos != std::string::npos)
	{
		std::string symbol_name = resolved_name.substr(0u, at_pos);
		std::string module_name = resolved_name.substr(at_pos + 1u);

		int import_index = 0;
		bool found = false;
		for (size_t i = 0u; i < m_tracked_imports.size(); i += 1u)
		{
			if (m_tracked_imports[i].m_from_module == module_name && m_tracked_imports[i].m_name == symbol_name)
			{
				import_index = -(static_cast<int>(i) + 1);
				found = true;
				break;
			}
		}

		if (!found)
		{
			m_tracked_imports.emplace_back(symbol_name, module_name);
			import_index = -static_cast<int>(m_tracked_imports.size());
		}

		EmitVariable(static_cast<uint8_t>(import_index), OpCode::GET_GLOBAL, line);
		return true;
	}

	std::unordered_map<std::string, int>::iterator global_it = m_global_variables.find(resolved_name);
	if (global_it == m_global_variables.end())
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Resolved symbol '{}' not found in globals.", resolved_name), line, m_file_name, m_source_lines));
		return false;
	}

	EmitVariable(global_it->second, OpCode::GET_GLOBAL, line);
	return true;
}

std::shared_ptr<MidoriType> CodeGenerator::GetConcreteTypeForExpression(const std::unique_ptr<MidoriExpression>& expr)
{
	if (!m_param_type_map.empty() && expr->IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& bounded_name = expr->GetExpression<MidoriExpression::NameAccess>();
		TypeEnvironment::iterator it = m_param_type_map.find(bounded_name.m_name.m_lexeme);
		if (it != m_param_type_map.end())
		{
			return it->second;
		}
	}

	std::shared_ptr<MidoriType> type = expr->GetType();
	if (!m_generic_type_substitution.empty())
	{
		return SubstituteGenericTypes(type, m_generic_type_substitution);
	}
	return type;
}

std::shared_ptr<MidoriType> CodeGenerator::SubstituteGenericTypes(const std::shared_ptr<MidoriType>& type, const TypeEnvironment& generic_type_map)
{
	using namespace std::string_literals;

	using SubstituteFn = std::function<std::shared_ptr<MidoriType>(const std::shared_ptr<MidoriType>&)>;

	std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>> cache;
	std::unordered_set<const MidoriType*> visiting;

	SubstituteFn substitute = [&generic_type_map, &cache, &visiting, &substitute](const std::shared_ptr<MidoriType>& current) -> std::shared_ptr<MidoriType>
		{
			if (!current)
			{
				return current;
			}

			std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>::iterator cache_it = cache.find(current.get());
			if (cache_it != cache.end())
			{
				return cache_it->second;
			}

			if (visiting.contains(current.get()))
			{
				return current;
			}

			visiting.insert(current.get());

			std::shared_ptr<MidoriType> result = std::visit
			(
				[&generic_type_map, &cache, &visiting, &substitute, &current](auto&& type_variant) -> std::shared_ptr<MidoriType>
				{
					using T = std::decay_t<decltype(type_variant)>;

					if constexpr (std::is_same_v<T, MidoriType::GenericParam>)
					{
						TypeEnvironment::const_iterator it = generic_type_map.find(type_variant.m_name);
						if (it != generic_type_map.end())
						{
							return it->second;
						}
						return current;
					}
					else if constexpr (std::is_same_v<T, MidoriType::TypeVariable>)
					{
						TypeEnvironment::const_iterator it = generic_type_map.find(current->ToString());
						if (it != generic_type_map.end())
						{
							return it->second;
						}
						return current;
					}
					else if constexpr (std::is_same_v<T, MidoriType::ArrayType>)
					{
						std::shared_ptr<MidoriType> substituted_element = substitute(type_variant.m_element_type);
						if (substituted_element != type_variant.m_element_type)
						{
							return std::make_shared<MidoriType>(MidoriType::ArrayType{ substituted_element });
						}
						return current;
					}
					else if constexpr (std::is_same_v<T, MidoriType::TupleType>)
					{
						std::vector<std::shared_ptr<MidoriType>> substituted_elements;
						bool changed = false;
						for (const std::shared_ptr<MidoriType>& elem_type : type_variant.m_element_types)
						{
							std::shared_ptr<MidoriType> substituted = substitute(elem_type);
							substituted_elements.push_back(substituted);
							if (substituted != elem_type)
							{
								changed = true;
							}
						}
						if (changed)
						{
							return std::make_shared<MidoriType>(MidoriType::TupleType{ std::move(substituted_elements) });
						}
						return current;
					}
					else if constexpr (std::is_same_v<T, MidoriType::FunctionType>)
					{
						std::vector<std::shared_ptr<MidoriType>> substituted_params;
						bool changed = false;
						for (const std::shared_ptr<MidoriType>& param_type : type_variant.m_param_types)
						{
							std::shared_ptr<MidoriType> substituted = substitute(param_type);
							substituted_params.push_back(substituted);
							if (substituted != param_type)
							{
								changed = true;
							}
						}
						std::shared_ptr<MidoriType> substituted_return = substitute(type_variant.m_return_type);
						if (substituted_return != type_variant.m_return_type)
						{
							changed = true;
						}
						if (changed)
						{
							return std::make_shared<MidoriType>(MidoriType::FunctionType{
								std::move(substituted_params),
								substituted_return,
								type_variant.m_is_foreign,
								type_variant.m_constraints
							});
						}
						return current;
					}
					else if constexpr (std::is_same_v<T, MidoriType::StructType>)
					{
						std::vector<std::shared_ptr<MidoriType>> empty_member_types;
						std::vector<std::string> member_names_copy = type_variant.m_member_names;
						std::shared_ptr<MidoriType> new_struct = MidoriType::MakeStructType(type_variant.m_name, std::move(empty_member_types), std::move(member_names_copy), {});
						cache[current.get()] = new_struct;

						std::vector<std::shared_ptr<MidoriType>> substituted_members;
						std::ranges::transform(type_variant.m_member_types, std::back_inserter(substituted_members), substitute);
						new_struct->GetType<MidoriType::StructType>().m_member_types = std::move(substituted_members);
						if (!type_variant.m_generic_params.empty() || type_variant.m_is_generic_instantiation)
						{
							new_struct->GetType<MidoriType::StructType>().m_is_generic_instantiation = true;
						}
						return new_struct;
					}
					else if constexpr (std::is_same_v<T, MidoriType::UnionType>)
					{
						std::shared_ptr<MidoriType> new_union = MidoriType::MakeUnionType(type_variant.m_name, {});
						cache[current.get()] = new_union;
						MidoriType::UnionType& new_union_ref = new_union->GetType<MidoriType::UnionType>();
						if (!type_variant.m_generic_params.empty() || type_variant.m_is_generic_instantiation)
						{
							new_union_ref.m_is_generic_instantiation = true;
						}

						for (const auto& [member_name, member_ctx] : type_variant.m_member_info)
						{
							std::vector<std::shared_ptr<MidoriType>> substituted_members;
							std::ranges::transform(member_ctx.m_member_types, std::back_inserter(substituted_members), substitute);
							new_union_ref.m_member_info.emplace(member_name, MidoriType::UnionType::UnionMemberContext{ std::move(substituted_members), member_ctx.m_tag });
						}
						return new_union;
					}
					else
					{
						return current;
					}
				},
				current->m_type
			);

			visiting.erase(current.get());
			return result;
		};

	return substitute(type);
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

	std::string full_name = debug_name + "@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_procedure_names.emplace_back(full_name.c_str());

	m_current_procedure_index = prev_index;

	if (m_current_procedure_index > MAX_FUNCTION_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many functions (max {})", MAX_FUNCTION_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	if (captured_count == 0)
	{
		EmitByte(OpCode::MAKE_FUNCTION, line);
		EmitByte(static_cast<OpCode>(closure_proc_index), line);
	}
	else
	{
		EmitByte(OpCode::MAKE_CLOSURE, line);
		EmitByte(static_cast<OpCode>(closure_proc_index), line);

		EmitByte(OpCode::BIND_CAPTURES, line);
		EmitByte(static_cast<OpCode>(captured_count), line);
	}
}

std::size_t CodeGenerator::FunctionSignatureHash::operator()(const FunctionSignature& sig) const
{
	std::size_t hash = std::hash<std::string>{}(sig.m_base_name);
	std::ranges::for_each
	(
		sig.m_concrete_types,
		[&hash](const std::string& type)
		{
			hash ^= std::hash<std::string>{}(type)+HASH_OFFSET_BASIS + (hash << HASH_LEFT_SHIFT) + (hash >> HASH_RIGHT_SHIFT);
		}
	);
	return hash;
}
bool CodeGenerator::FunctionSignature::operator==(const FunctionSignature& other) const
{
	return m_base_name == other.m_base_name && m_concrete_types == other.m_concrete_types;
}
std::size_t CodeGenerator::TypePairHash::operator()(const std::pair<MidoriType*, MidoriType*>& pair) const
{
	std::size_t h1 = std::hash<MidoriType*>{}(pair.first);
	std::size_t h2 = std::hash<MidoriType*>{}(pair.second);
	return h1 ^ (h2 << 1);
}
