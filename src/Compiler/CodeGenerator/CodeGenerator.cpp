#include <cctype>
#include <filesystem>
#include <format>
#include <iostream>
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

CodeGenerator::CodeGenerator(MidoriProgramTree&& program_tree, std::string_view file_name, const std::vector<std::string>& source_lines, std::string module_name, std::unordered_set<std::string> export_symbols, const std::unordered_map<std::string, std::unordered_set<std::string>>& imported_class_methods, const std::unordered_map<std::string, std::vector<std::string>>& imported_class_instances)
	: m_program_tree(std::move(program_tree)),
	m_file_name(file_name),
	m_source_lines(source_lines),
	m_module_name(std::move(module_name)),
	m_export_symbols(std::move(export_symbols)),
	m_class_methods(imported_class_methods),
	m_class_instances(imported_class_instances)
{
	std::string main_proc_name = "__main__@"s + (m_module_name.has_value() ? m_module_name.value() : std::string(file_name));
	m_procedure_names.emplace_back(main_proc_name.c_str());
}

MidoriResult::CodeGeneratorResult CodeGenerator::GenerateModuleBytecode()
{
	std::ranges::for_each
	(
		m_program_tree,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **statement);

			// Track exports: after processing DefineFunction, check if it's exported
			std::visit
			(
				[this](const auto& stmt)
				{
					using T = std::decay_t<decltype(stmt)>;
					if constexpr (std::is_same_v<T, MidoriStatement::DefineFunction>)
					{
						const std::string& function_name = stmt.m_name.m_lexeme;
						if (m_export_symbols.contains(function_name))
						{
							// After DefineFunction processing, m_current_procedure_index points AFTER the new procedure
							// So the procedure we just added is at index m_procedures.size() - 1
							const size_t procedure_index = m_procedures.size() - 1u;
							const size_t global_index = m_global_variables[function_name];

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
								0,  // Structs don't have procedure index
								0,  // Structs don't have global index
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
								0,  // Unions don't have procedure index
								0,  // Unions don't have global index
								BytecodeModule::SymbolType::UNION_TYPE
							);
						}
					}
					else if constexpr (std::is_same_v<T, MidoriStatement::Foreign>)
					{
						const std::string& foreign_name = stmt.m_function_name.m_lexeme;
						if (m_export_symbols.contains(foreign_name))
						{
							// Foreign functions are stored as global variables containing the function name string
							const size_t global_index = m_global_variables[foreign_name];
							m_tracked_exports.emplace_back
							(
								foreign_name,
								0,  // Foreign functions don't have procedure index
								global_index,
								BytecodeModule::SymbolType::FOREIGN_FUNCTION
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

	std::vector<std::pair<std::string, int>> sorted_globals(m_global_variables.begin(), m_global_variables.end());
	std::ranges::sort(sorted_globals, [](const std::pair<std::string, int>& a, const std::pair<std::string, int>& b) { return a.second < b.second; });

	module.m_global_variables.reserve(sorted_globals.size());
	for (const std::pair<std::string, int>& entry : sorted_globals)
	{
		module.m_global_variables.emplace_back(entry.first.c_str());
	}

	return module;
}

CodeGenerator::GenericFunctionInfo::GenericFunctionInfo(std::string name, std::vector<Token> params, std::vector<std::shared_ptr<MidoriType>> param_types, const std::vector<Token>& generic_params, std::vector<MidoriType::ClassConstraint> constraints, std::shared_ptr<MidoriType> return_type, std::unique_ptr<MidoriExpression>* body, int captured_count)
	: m_name(std::move(name))
	, m_params(std::move(params))
	, m_param_types(std::move(param_types))
	, m_constraints(std::move(constraints))
	, m_generic_return_type(std::move(return_type))
	, m_body(body)
	, m_captured_count(captured_count)
{
	m_generic_param_types.reserve(generic_params.size());
	for (const Token& generic_param : generic_params)
	{
		m_generic_param_types.emplace_back(std::make_shared<MidoriType>(MidoriType::GenericParam(generic_param.m_lexeme)));
	}
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
	else
	{
		// For local variables, emit SET_LOCAL to properly store the value
		EmitVariable(def.m_local_index.value(), OpCode::SET_LOCAL, line);
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

void CodeGenerator::operator()(MidoriStatement::DefineFunction& defun)
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
		m_generic_functions.emplace(defun.m_name.m_lexeme, GenericFunctionInfo(defun.m_name.m_lexeme, defun.m_params, defun.m_param_types, defun.m_generic_params, defun.m_constraints, defun.m_return_type, &defun.m_body, defun.m_captured_count));
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

void CodeGenerator::operator()(MidoriStatement::Class& class_stmt)
{
	std::unordered_set<std::string> method_names;

	for (const std::unique_ptr<MidoriStatement>& method : class_stmt.m_methods)
	{
		if (method->IsStatement<MidoriStatement::DefineFunction>())
		{
			const MidoriStatement::DefineFunction& defun = method->GetStatement<MidoriStatement::DefineFunction>();
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

		if (method->IsStatement<MidoriStatement::DefineFunction>())
		{
			const MidoriStatement::DefineFunction& defun = method->GetStatement<MidoriStatement::DefineFunction>();
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
					EmitByte(OpCode::CALL_DEFINED, line);
					EmitByte(static_cast<OpCode>(1), line);  // 1 parameter
					return;
				}
			}
		}

		// Not in a specialized context, try direct lookup for concrete Convertable instances
		if (!from_type->IsType<MidoriType::TypeVariable>() && !target_type->IsType<MidoriType::TypeVariable>())
		{
			std::string mangled_name = "Convert_Convertable_"s + from_type->ToString() + "_"s + target_type->ToString();
			std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
			if (it != m_global_variables.end())
			{
				EmitVariable(it->second, OpCode::GET_GLOBAL, line);
				EmitByte(OpCode::CALL_DEFINED, line);
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

	bool is_generic_call = false;
	std::string function_name;
	std::optional<std::string> resolved_method_name = std::nullopt;

	if (call.m_callee->IsExpression<MidoriExpression::BoundedName>())
	{
		MidoriExpression::BoundedName& callee_name = call.m_callee->GetExpression<MidoriExpression::BoundedName>();
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

		if (resolved_method_name.has_value())
		{
			if (!EmitResolvedNameGetGlobal(resolved_method_name.value(), line))
			{
				return;
			}
		}
		else
		{
			std::visit([this](auto&& arg){ (*this)(arg); }, **call.m_callee);
		}

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

void CodeGenerator::operator()(MidoriExpression::For& for_expr)
{
	int line = for_expr.m_for_keyword.m_line;
	bool is_float = for_expr.m_range->GetType()->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::FloatType>();

	// The parser has reserved 3 local variable slots
	// for_expr.m_loop_variable_index: the loop variable
	// for_expr.m_hidden_step_index: hidden step value
	// for_expr.m_hidden_end_index: hidden end value

	// Update m_local_count to account for the 3 reserved locals
	// This ensures any variables declared in the loop body get indices starting from 3
	if (m_local_count < for_expr.m_hidden_end_index + 1)
	{
		m_local_count = for_expr.m_hidden_end_index + 1;
	}

	EmitByte(OpCode::PUSH_PLACEHOLDER, line);
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);

	std::visit([this](auto&& arg) { (*this)(arg); }, **for_expr.m_range);
	// Stack: [0, 0, 0, range]

	// Duplicate range for each extraction
	EmitByte(OpCode::DUP, line);
	EmitByte(OpCode::DUP, line);
	// Stack: [0, 0, 0, range, range, range]

	EmitByte(OpCode::GET_RANGE_START, line);
	// Stack: [0, 0, 0, range, range, start]
	EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
	// Stack: [start, 0, 0, range, range, start]
	EmitByte(OpCode::POP, line);
	// Stack: [start, 0, 0, range, range]

	EmitByte(OpCode::GET_RANGE_STEP, line);
	// Stack: [start, 0, 0, range, step]
	EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
	// Stack: [start, step, 0, range, step]
	EmitByte(OpCode::POP, line);
	// Stack: [start, step, 0, range]

	EmitByte(OpCode::GET_RANGE_END, line);
	// Stack: [start, step, 0, end]
	EmitVariable(for_expr.m_hidden_end_index, OpCode::SET_LOCAL, line);
	// Stack: [start, step, end, end]
	EmitByte(OpCode::POP, line);
	// Stack: [start, step, end]

	// Stack: [start, step, end] at local positions 0, 1, 2

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
	// Check if step > 0 (forward) or step <= 0 (backward)
	EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
	if (is_float)
	{
		EmitFloatConstant(0.0, line);
	}
	else
	{
		EmitByte(OpCode::INT_0, line);
	}

	// IF_INTEGER_GREATER jumps when !(step > 0), i.e., when step <= 0 (backward iteration)
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

	EmitByte(OpCode::POP, line);  // Pop end
	EmitByte(OpCode::POP, line);  // Pop step
	EmitByte(OpCode::POP, line);  // Pop loop variable
	// Stack: []

	// Push unit value as result for normal loop exit
	// This must be BEFORE EndLoop so break statements skip it and use their own value
	EmitByte(OpCode::OP_UNIT, line);

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

	// Build generic type parameter -> concrete type map
	// Match generic parameters to concrete types by position
	// For each parameter that has a generic/type variable type, assign it the next generic parameter in order
	std::unordered_map<std::string, std::shared_ptr<MidoriType>> generic_type_map;

	size_t generic_param_index = 0;
	for (size_t i = 0u; i < generic_info.m_param_types.size() && i < concrete_arg_types.size(); i += 1u)
	{
		const std::shared_ptr<MidoriType>& param_type = generic_info.m_param_types[i];
		const std::shared_ptr<MidoriType>& concrete_type = concrete_arg_types[i];

		if (param_type->IsType<MidoriType::GenericParam>() || param_type->IsType<MidoriType::TypeVariable>())
		{
			if (generic_param_index < generic_info.m_generic_param_types.size())
			{
				const std::shared_ptr<MidoriType>& orig_gen_param = generic_info.m_generic_param_types[generic_param_index];
				if (orig_gen_param->IsType<MidoriType::GenericParam>())
				{
					const MidoriType::GenericParam& gen_param = orig_gen_param->GetType<MidoriType::GenericParam>();
					generic_type_map[gen_param.m_name] = concrete_type;
					generic_param_index += 1u;
				}
			}
		}
	}


	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>> prev_resolution_map = m_method_resolution_map;
	m_method_resolution_map.clear();

	for (const MidoriType::ClassConstraint& constraint : generic_info.m_constraints)
	{
		std::unordered_map<std::string, std::unordered_set<std::string>>::iterator tc_it = m_class_methods.find(constraint.m_class_name);
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

				std::unordered_map<std::string, std::vector<std::string>>::iterator instances_it = m_class_instances.find(constraint.m_class_name);
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

	std::visit([this](auto&& arg) { (*this)(arg); }, **(*generic_info.m_body));
	EmitByte(OpCode::RETURN, line);

	std::string full_specialized_name = specialized_name + "@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_procedure_names.emplace_back(full_specialized_name.c_str());

	m_current_procedure_index = prev_index;

	m_param_type_map = std::move(prev_param_map);
	m_method_resolution_map = std::move(prev_resolution_map);
	m_specialized_functions[signature] = specialized_proc_index;

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
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': no constraint matches argument type '{}'.", callee_name, first_arg_type_name), line, m_file_name, m_source_lines));
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

std::shared_ptr<MidoriType> CodeGenerator::SubstituteGenericTypes(const std::shared_ptr<MidoriType>& type, const std::unordered_map<std::string, std::shared_ptr<MidoriType>>& generic_type_map)
{
	using namespace std::string_literals;

	return std::visit(
		[&](auto&& type_variant) -> std::shared_ptr<MidoriType>
		{
			using T = std::decay_t<decltype(type_variant)>;

			if constexpr (std::is_same_v<T, MidoriType::GenericParam>)
			{
				// This is a generic parameter - substitute it with the concrete type
				auto it = generic_type_map.find(type_variant.m_name);
				if (it != generic_type_map.end())
				{
					return it->second;
				}
				// If not found in map, return as-is
				return type;
			}
			else if constexpr (std::is_same_v<T, MidoriType::ArrayType>)
			{
				// Recursively substitute in array element type
				auto substituted_element = SubstituteGenericTypes(type_variant.m_element_type, generic_type_map);
				if (substituted_element != type_variant.m_element_type)
				{
					return std::make_shared<MidoriType>(MidoriType::ArrayType{ substituted_element });
				}
				return type;
			}
			else if constexpr (std::is_same_v<T, MidoriType::TupleType>)
			{
				// Recursively substitute in tuple element types
				std::vector<std::shared_ptr<MidoriType>> substituted_elements;
				bool changed = false;
				for (const auto& elem_type : type_variant.m_element_types)
				{
					auto substituted = SubstituteGenericTypes(elem_type, generic_type_map);
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
				return type;
			}
			else if constexpr (std::is_same_v<T, MidoriType::FunctionType>)
			{
				// Recursively substitute in parameter and return types
				std::vector<std::shared_ptr<MidoriType>> substituted_params;
				bool changed = false;
				for (const auto& param_type : type_variant.m_param_types)
				{
					auto substituted = SubstituteGenericTypes(param_type, generic_type_map);
					substituted_params.push_back(substituted);
					if (substituted != param_type)
					{
						changed = true;
					}
				}
				auto substituted_return = SubstituteGenericTypes(type_variant.m_return_type, generic_type_map);
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
				return type;
			}
			else
			{
				// For all other types, return as-is
				return type;
			}
		},
		type->m_type
	);
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
