#include <algorithm>
#include <fstream>
#include <queue>
#include <sstream>

#include "Common/Constant/Constant.h"
#include "Compiler/Lexer/Lexer.h"
#include "Parser.h"

using namespace std::string_literals;

Parser::Parser(TokenStream&& tokens,std::string_view file_name, const std::vector<std::string>& source_lines, const std::unordered_map<std::string, CompiledModule::SymbolTable>& imports, const std::unordered_map<std::string, TypeEnvironment>& imported_type_signatures, const std::vector<UseImport>& use_imports, const ModuleDeclaration* module_decl, const CompiledModule::TypeclassMetadataMap& imported_typeclass_metadata)
	: m_tokens(std::move(tokens)),
	m_file_name(file_name),
	m_source_lines(source_lines),
	m_module_declarations(nullptr),
	m_use_imports(nullptr),
	m_current_module(module_decl),
	m_current_use_imports(use_imports),
	m_imported_symbols(imports),
	m_imported_type_signatures(imported_type_signatures)
{
	for (const auto& [tc_name, metadata] : imported_typeclass_metadata)
	{
		m_class_methods[tc_name] = metadata.m_method_names;
		m_typeclass_type_params[tc_name] = metadata.m_type_param_names;
		m_class_instances[tc_name] = metadata.m_instance_methods;
		m_typeclass_method_types[tc_name] = metadata.m_method_types;
	}
}

bool Parser::SharesNamespace(const std::string& namespace1, const std::string& namespace2) const
{
	// Extract top-level namespace (everything before first '.')
	std::function<std::string(const std::string&)> get_top_level_namespace = [](const std::string& full_name) -> std::string
		{
			size_t pos = full_name.find('.');
			if (pos != std::string::npos)
			{
				return full_name.substr(0u, pos);
			}
			return full_name;
		};

	std::string top_ns1 = get_top_level_namespace(namespace1);
	std::string top_ns2 = get_top_level_namespace(namespace2);

	// Both in global namespace (empty module names)
	if (top_ns1.empty() && top_ns2.empty())
	{
		return true;
	}

	// One is in global, other is not
	if (top_ns1.empty() || top_ns2.empty())
	{
		return false;
	}

	// Check if they share the same top-level namespace
	// Math.Vector and Math.Matrix both share "Math"
	// Math.Vector.Internal and Math.Utils both share "Math"
	return top_ns1 == top_ns2;
}

std::string Parser::ExtractSymbolName(const std::string& qualified_name) const
{
	size_t last_separator = qualified_name.rfind(NameSeparator);
	if (last_separator != std::string::npos)
	{
		return qualified_name.substr(last_separator + NameSeparator.length());
	}
	else
	{
		return qualified_name;
	}
}

std::string Parser::ExtractQualifier(const std::string& qualified_name) const
{
	size_t last_separator = qualified_name.rfind(NameSeparator);
	if (last_separator != std::string::npos)
	{
		return qualified_name.substr(0u, last_separator);
	}
	else
	{
		return {};
	}
}

MidoriResult::ExpressionResult Parser::ResolveQualifiedName(const Token& name_token, const std::string& mangled_name)
{
	std::string lookup_name = mangled_name;
	std::vector<Scope>::const_reverse_iterator found_scope_it = FindVariableScope(lookup_name);

	if (found_scope_it != m_scopes.rend())
	{
		Scope::VariableTable::const_iterator find_result = found_scope_it->m_variables.find(lookup_name);

		std::string module_name;
		bool is_imported = IsInUseImports(lookup_name, module_name);

		Token qualified_token = name_token;
		if (is_imported)
		{
			qualified_token.m_lexeme = module_name + NameSeparator.data() + lookup_name;
		}

		// Global
		if (IsGlobalName(found_scope_it))
		{
			return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(qualified_token, MidoriExpression::NameContext::Global()));
		}
		// Local
		else if (IsLocalName(find_result))
		{
			return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(qualified_token, MidoriExpression::NameContext::Local(find_result->second.m_relative_index.value())));
		}
		// Cell
		else
		{
			return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(qualified_token, MidoriExpression::NameContext::Cell(find_result->second.m_absolute_index.value())));
		}
	}

	// Not found in local scopes - check imported modules (for bare imports)
	// Iterate through all imported modules to find if any exports this symbol
	for (const auto& [imported_module_name, symbol_table] : m_imported_symbols)
	{
		if (symbol_table.HasExport(lookup_name))
		{
			VisibilityLevel visibility = symbol_table.GetExportVisibility(lookup_name);

			bool can_access = false;
			if (visibility == VisibilityLevel::Public)
			{
				can_access = true;
			}
			else if (visibility == VisibilityLevel::Private)
			{
				// Private exports only accessible to modules in same namespace
				if (m_current_module != nullptr && m_current_module->m_has_module_declaration)
				{
					if (SharesNamespace(m_current_module->m_module_name, imported_module_name))
					{
						can_access = true;
					}
				}
			}

			if (can_access)
			{
				Token qualified_token = name_token;
				qualified_token.m_lexeme = imported_module_name + NameSeparator.data() + lookup_name;
				return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(qualified_token, MidoriExpression::NameContext::Global()));
			}
			else
			{
				return std::unexpected<std::string>(GenerateParserError(std::format("Cannot access private symbol '{}' from module '{}'.", lookup_name, imported_module_name), name_token));
			}
		}
	}

	std::vector<std::string> matching_typeclasses;
	for (const MidoriType::ClassConstraint& constraint : m_active_constraints)
	{
		std::unordered_map<std::string, std::unordered_set<std::string>>::const_iterator tc_it = m_class_methods.find(constraint.m_class_name);
		if (tc_it != m_class_methods.cend() && tc_it->second.contains(lookup_name))
		{
			matching_typeclasses.emplace_back(constraint.m_class_name);
		}
	}

	if (!matching_typeclasses.empty())
	{
		if (matching_typeclasses.size() == 1u)
		{
			return std::unexpected<std::string>
			(
				GenerateParserError
				(
					std::format("Unqualified class method '{}'. Use qualified syntax like '{}{}{}'.", lookup_name, matching_typeclasses[0], NameSeparator, lookup_name),
					name_token
				)
			);
		}

		std::string candidates;
		for (size_t i = 0u; i < matching_typeclasses.size(); i += 1u)
		{
			if (i > 0u)
			{
				candidates.append(", "s);
			}
			candidates.append(matching_typeclasses[i]).append(NameSeparator).append(lookup_name);
		}

		return std::unexpected<std::string>
		(
			GenerateParserError
			(
				std::format("Unqualified class method '{}'. Use qualified syntax like one of: {}.", lookup_name, candidates),
				name_token
			)
		);
	}

	return std::unexpected<std::string>(GenerateParserError("Undefined name.", name_token));
}

bool Parser::CanAccessSymbol(const std::string& symbol_name) const
{
	// No module system enabled, allow all access
	if (m_module_declarations == nullptr || m_current_module == nullptr)
	{
		return true;
	}

	// First check if symbol is defined in current scope (local or global)
	// Local symbols always take precedence over imported symbols
	std::string mangled_name = symbol_name;  // For namespace-qualified names
	std::vector<Scope>::const_reverse_iterator found_scope_it = std::ranges::find_if
	(
		m_scopes.rbegin(),
		m_scopes.rend(),
		[&mangled_name](const Scope& scope)
		{
			return scope.m_variables.contains(mangled_name) ||
				scope.m_struct_constructors.contains(mangled_name) ||
				scope.m_union_constructors.contains(mangled_name) ||
				scope.m_defined_names.contains(mangled_name);
		}
	);

	// If symbol is defined in current scope, always allow access
	if (found_scope_it != m_scopes.rend())
	{
		return true;
	}

	// For unqualified symbol access to external symbols, the symbol must either be:
	// 1. Explicitly imported via 'use' statement, OR
	// 2. Not exported by any module (i.e., foreign function)

	// Check if symbol was explicitly imported via 'use'
	std::string module_name;
	if (IsInUseImports(symbol_name, module_name))
	{
		// Symbol is in use imports, verify it's actually exported by that module
		return ResolveQualifiedSymbol(module_name, symbol_name);
	}

	// Check if symbol is exported by ANY module
	bool found_in_any_export = false;
	for (const auto& [file_path, module_decl] : *m_module_declarations)
	{
		if (module_decl.HasExport(symbol_name))
		{
			found_in_any_export = true;
			break;
		}
	}

	// If symbol is exported by a module but NOT in use imports, deny access
	// (must use qualified name like Module.Symbol)
	if (found_in_any_export)
	{
		return false;
	}

	// Symbol not exported by any module - allow access (might be foreign function)
	return true;
}

bool Parser::IsInUseImports(const std::string& symbol_name, std::string& out_module_name) const
{
	// Check if the symbol was explicitly imported via 'use' statement
	for (const UseImport& use_import : m_current_use_imports)
	{
		if (use_import.m_symbol_name == symbol_name)
		{
			out_module_name = use_import.m_module_name;
			return true;
		}
	}
	return false;
}

bool Parser::ResolveQualifiedSymbol(const std::string& module_name, const std::string& symbol_name) const
{
	const bool using_new_path = (m_module_declarations == nullptr);

	if (using_new_path)
	{
		// New path: Check imported symbol tables (from per-module compilation)
		std::unordered_map<std::string, CompiledModule::SymbolTable>::const_iterator it = m_imported_symbols.find(module_name);
		if (it != m_imported_symbols.cend())
		{
			// Check if the symbol is exported by this module
			return it->second.HasExport(symbol_name);
		}
		return false;  // Module not found in imports
	}
	return false;  // Module not found
}

bool Parser::IsGlobalName(const std::vector<Scope>::const_reverse_iterator& found_scope_it) const
{
	return found_scope_it == std::prev(m_scopes.crend());
}

bool Parser::IsLocalName(const Scope::VariableTable::const_iterator& found_tbl_it) const
{
	return m_function_depth == 0 || found_tbl_it->second.m_function_depth == m_function_depth;
}

bool Parser::IsAtGlobalScope() const
{
	return m_scopes.size() == 1u;
}

std::string Parser::GenerateParserError(std::string&& message, const Token& token)
{
	Synchronize();
	return MidoriError::GenerateParserErrorWithContext(std::move(message), token, m_file_name, m_source_lines);
}

bool Parser::IsAtEnd()
{
	return Peek(0).m_token_name == Token::Name::END_OF_FILE;
}

bool Parser::Check(Token::Name type, int offset)
{
	return !IsAtEnd() && Peek(offset).m_token_name == type;
}

Token& Parser::Peek(int offset)
{
	return m_current_token_index + offset < m_tokens.Size() ? m_tokens[m_current_token_index + offset] : m_tokens[m_tokens.Size() - 1];
}

Token& Parser::Previous()
{
	return m_tokens[static_cast<size_t>(m_current_token_index - 1)];
}

std::vector<Parser::Scope>::const_reverse_iterator Parser::FindTypeScope(std::string& name)
{
	for (std::vector<Parser::Scope>::const_reverse_iterator it = m_scopes.crbegin(); it != m_scopes.crend(); ++it)
	{
		if (it->m_defined_types.find(name) != it->m_defined_types.end())
		{
			return it;
		}
	}

	std::string mangled_name;
	for (size_t end_idx : std::views::iota(0u, m_namespaces.size()))
	{
		std::string stacked_namespace;
		for (size_t idx : std::views::iota(0u, end_idx + 1u))
		{
			stacked_namespace.append(m_namespaces[idx]).append(NameSeparator);
		}
		mangled_name.append(stacked_namespace).append(name);

		for (std::vector<Parser::Scope>::const_reverse_iterator it = m_scopes.crbegin(); it != m_scopes.crend(); ++it)
		{
			if (it->m_defined_types.find(mangled_name) != it->m_defined_types.end())
			{
				name = std::move(mangled_name);
				return it;
			}
		}

		mangled_name.clear();
	}

	return m_scopes.crend();
}

std::vector<Parser::Scope>::const_reverse_iterator Parser::FindVariableScope(std::string& name)
{
	for (std::vector<Parser::Scope>::const_reverse_iterator it = m_scopes.crbegin(); it != m_scopes.crend(); ++it)
	{
		if (it->m_variables.find(name) != it->m_variables.end())
		{
			return it;
		}
	}

	std::string mangled_name;
	for (size_t end_idx : std::views::iota(0u, m_namespaces.size()))
	{
		std::string stacked_namespace;
		for (size_t idx : std::views::iota(0u, end_idx + 1u))
		{
			stacked_namespace.append(m_namespaces[idx]).append(NameSeparator);
		}
		mangled_name.append(stacked_namespace).append(name);

		for (std::vector<Parser::Scope>::const_reverse_iterator it = m_scopes.crbegin(); it != m_scopes.crend(); ++it)
		{
			if (it->m_variables.find(mangled_name) != it->m_variables.end())
			{
				name = std::move(mangled_name);
				return it;
			}
		}

		mangled_name.clear();
	}

	return m_scopes.crend();
}

Token& Parser::Advance()
{
	if (!IsAtEnd())
	{
		m_current_token_index += 1;
	}
	return Previous();
}

MidoriResult::TokenResult Parser::Consume(Token::Name type, std::string_view message)
{
	if (Check(type, 0))
	{
		return Advance();
	}
	else
	{
		return std::unexpected<std::string>(MidoriError::GenerateParserErrorWithContext(message, Peek(0), m_file_name, m_source_lines));
	}
}

void Parser::BeginScope()
{
	m_scopes.emplace_back();
}

int Parser::EndScope()
{
	const Scope& scope = m_scopes.back();
	int block_local_count = static_cast<int>(scope.m_variables.size());
	m_total_locals_in_curr_scope -= block_local_count;
	m_total_variables -= block_local_count;
	m_scopes.pop_back();
	return block_local_count;
}

std::string Parser::Mangle(std::string_view name)
{
	size_t sep_idx = name.find(NameSeparator);

	if (sep_idx != std::string::npos)
	{
		// Name already has a qualifier (e.g., "UnionName::Member")
		std::string_view top_qualifier = name.substr(0u, sep_idx);
		std::vector<std::string>::const_iterator find_result = std::find(m_namespaces.cbegin(), m_namespaces.cend(), top_qualifier);

		if (find_result != m_namespaces.cend())
		{
			// Found the qualifier in our stack, resolve to absolute path
			std::string mangled_name;

			// Prepend qualifiers up to (but not including) the found one
			for (std::vector<std::string>::const_iterator it = m_namespaces.cbegin(); it != find_result; ++it)
			{
				mangled_name.append(*it).append(NameSeparator);
			}

			// Append the original name (which already includes the found qualifier)
			mangled_name.append(name);
			return mangled_name;
		}
		else
		{
			// Qualifier not in our stack - return as-is (already absolute)
			return std::string(name);
		}
	}
	else
	{
		// No separator - prepend all current qualification contexts (e.g., union names)
		std::string mangled_name;
		for (const std::string& qualifier : m_namespaces)
		{
			mangled_name.append(qualifier).append(NameSeparator);
		}
		mangled_name.append(name);
		return mangled_name;
	}
}

MidoriResult::TokenResult Parser::DefineName(Token& name, bool is_variable)
{
	name.m_lexeme = Mangle(name.m_lexeme);

	if (m_scopes.back().m_defined_names.contains(name.m_lexeme))
	{
		return std::unexpected<std::string>(GenerateParserError("Name already exists in the current scope", name));
	}

	// m_scopes.size() - 2 because the last scope is the current scope
	for (int i = static_cast<int>(m_scopes.size()) - 2; i >= 0; --i)
	{
		size_t index = static_cast<size_t>(i);
		if (m_scopes[index].m_struct_constructors.contains(name.m_lexeme))
		{
			// TODO: Warning
			// Overshadowing a struct
		}
		if (m_scopes[index].m_variables.contains(name.m_lexeme))
		{
			// TODO: Warning
			// Overshadowing a variable
		}
		if (m_scopes[index].m_union_constructors.contains(name.m_lexeme))
		{
			// TODO: Warning
			// Overshadowing a union
		}
		if (m_scopes[index].m_defined_types.contains(name.m_lexeme))
		{
			// TODO: Warning
			// Overshadowing a type
		}
	}

	m_scopes.back().m_defined_names.emplace(name.m_lexeme);
	if (is_variable)
	{
		m_scopes.back().m_variables.emplace(name.m_lexeme, VariableContext());
	}

	return name;
}

std::optional<int> Parser::RegisterOrUpdateLocalVariable(const std::string& name)
{
	std::optional<int> local_index = std::nullopt;

	if (!IsAtGlobalScope())
	{
		m_scopes.back().m_variables[name] = VariableContext(m_total_locals_in_curr_scope++, m_total_variables++, m_function_depth);
		local_index.emplace(m_scopes.back().m_variables[name].m_relative_index.value());
	}

	return local_index;
}

MidoriResult::ExpressionResult Parser::ParseFactor()
{
	return ParseBinary(&Parser::ParseUnaryLogicalBitwise, Token::Name::STAR, Token::Name::SLASH, Token::Name::PERCENT);
}

MidoriResult::ExpressionResult Parser::ParseShift()
{
	return ParseBinary(&Parser::ParseTerm, Token::Name::LEFT_SHIFT, Token::Name::RIGHT_SHIFT);
}

MidoriResult::ExpressionResult Parser::ParseRange()
{
	return ParseShift()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& start) -> MidoriResult::ExpressionResult
			{
				if (!Match(Token::Name::DOUBLE_DOT))
				{
					return start;
				}

				Token first_range_op = Previous();

				return ParseShift()
					.and_then
					(
						[this, &first_range_op, &start](std::unique_ptr<MidoriExpression>&& middle) -> MidoriResult::ExpressionResult
						{
							if (!Match(Token::Name::DOUBLE_DOT))
							{
								return std::unexpected<std::string>(GenerateParserError("Expected '..' for step in range expression. Use 'start..step..end' syntax.", Peek(0)));
							}

							Token second_range_op = Previous();

							return ParseShift()
								.and_then
								(
									[&first_range_op, &second_range_op, &start, &middle](std::unique_ptr<MidoriExpression>&& end) -> MidoriResult::ExpressionResult
									{
										return std::make_unique<MidoriExpression>(MidoriExpression::RangeTernary(first_range_op, second_range_op, std::move(start), std::move(middle), std::move(end)));
									}
								);
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseTerm()
{
	return ParseBinary(&Parser::ParseFactor, Token::Name::SINGLE_PLUS, Token::Name::DOUBLE_PLUS, Token::Name::SINGLE_MINUS);
}

MidoriResult::ExpressionResult Parser::ParseComparison()
{
	return ParseBinary(&Parser::ParseRange, Token::Name::LEFT_ANGLE, Token::Name::LESS_EQUAL, Token::Name::RIGHT_ANGLE, Token::Name::GREATER_EQUAL);
}

MidoriResult::ExpressionResult Parser::ParseEquality()
{
	return ParseBinary(&Parser::ParseComparison, Token::Name::BANG_EQUAL, Token::Name::DOUBLE_EQUAL);
}

MidoriResult::ExpressionResult Parser::ParseBitwiseAnd()
{
	return ParseBinary(&Parser::ParseEquality, Token::Name::SINGLE_AMPERSAND);
}

MidoriResult::ExpressionResult Parser::ParseBitwiseXor()
{
	return ParseBinary(&Parser::ParseBitwiseAnd, Token::Name::CARET);
}

MidoriResult::ExpressionResult Parser::ParseBitwiseOr()
{
	return ParseBinary(&Parser::ParseBitwiseXor, Token::Name::SINGLE_BAR);
}

MidoriResult::ExpressionResult Parser::ParseBind()
{
	return ParsePipe()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& left_expr) -> MidoriResult::ExpressionResult
			{
				if (Match(Token::Name::SINGLE_EQUAL))
				{
					Token& equal = Previous();
					return ParseBind()
						.and_then
						(
							[this, &left_expr, &equal](std::unique_ptr<MidoriExpression>&& right_expr) -> MidoriResult::ExpressionResult
							{
								if (left_expr->IsExpression<MidoriExpression::BoundedName>())
								{
									MidoriExpression::BoundedName& variable_expr = left_expr->GetExpression<MidoriExpression::BoundedName>();
									std::vector<Scope>::const_reverse_iterator found_scope_it = FindVariableScope(variable_expr.m_name.m_lexeme);

									if (found_scope_it != m_scopes.crend())
									{
										Scope::VariableTable::const_iterator find_result = found_scope_it->m_variables.find(variable_expr.m_name.m_lexeme);
										if (IsGlobalName(found_scope_it))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::Bind(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Global()));
										}
										else if (IsLocalName(find_result))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::Bind(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Local(find_result->second.m_relative_index.value())));
										}
										else
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::Bind(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Cell(find_result->second.m_absolute_index.value())));
										}
									}
									return std::unexpected<std::string>(GenerateParserError("Unbound name.", variable_expr.m_name));
								}
								else if (left_expr->IsExpression<MidoriExpression::Get>())
								{
									MidoriExpression::Get& get_expr = left_expr->GetExpression<MidoriExpression::Get>();
									return std::make_unique<MidoriExpression>(MidoriExpression::Set(get_expr.m_member_name, std::move(get_expr.m_struct), std::move(right_expr)));
								}
								else if (left_expr->IsExpression<MidoriExpression::ArrayGet>())
								{
									MidoriExpression::ArrayGet& access_expr = left_expr->GetExpression<MidoriExpression::ArrayGet>();
									return std::make_unique<MidoriExpression>(MidoriExpression::ArraySet(access_expr.m_op, std::move(access_expr.m_indices), std::move(access_expr.m_arr_var), std::move(right_expr)));
								}
								return std::unexpected<std::string>(GenerateParserError("Invalid binding target.", equal));
							}
						);
				}
				else if (Match(Token::Name::PLUS_PLUS_EQUAL))
				{
					Token& op = Previous();
					return ParseBind()
						.and_then
						(
							[this, &left_expr, &op](std::unique_ptr<MidoriExpression>&& right_expr) -> MidoriResult::ExpressionResult
							{
								if (left_expr->IsExpression<MidoriExpression::BoundedName>())
								{
									MidoriExpression::BoundedName& variable_expr = left_expr->GetExpression<MidoriExpression::BoundedName>();
									std::vector<Scope>::const_reverse_iterator found_scope_it = FindVariableScope(variable_expr.m_name.m_lexeme);

									if (found_scope_it != m_scopes.crend())
									{
										Scope::VariableTable::const_iterator find_result = found_scope_it->m_variables.find(variable_expr.m_name.m_lexeme);
										if (IsGlobalName(found_scope_it))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::AppendAssign(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Global()));
										}
										else if (IsLocalName(find_result))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::AppendAssign(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Local(find_result->second.m_relative_index.value())));
										}
										else
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::AppendAssign(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Cell(find_result->second.m_absolute_index.value())));
										}
									}
									return std::unexpected<std::string>(GenerateParserError("Unbound name.", variable_expr.m_name));
								}
								return std::unexpected<std::string>(GenerateParserError("Invalid append assignment target (must be a variable).", op));
							}
						);
				}
				else if (Match(Token::Name::EQUAL_PLUS_PLUS))
				{
					Token& op = Previous();
					return ParseBind()
						.and_then
						(
							[this, &left_expr, &op](std::unique_ptr<MidoriExpression>&& right_expr) -> MidoriResult::ExpressionResult
							{
								if (left_expr->IsExpression<MidoriExpression::BoundedName>())
								{
									MidoriExpression::BoundedName& variable_expr = left_expr->GetExpression<MidoriExpression::BoundedName>();
									std::vector<Scope>::const_reverse_iterator found_scope_it = FindVariableScope(variable_expr.m_name.m_lexeme);

									if (found_scope_it != m_scopes.crend())
									{
										Scope::VariableTable::const_iterator find_result = found_scope_it->m_variables.find(variable_expr.m_name.m_lexeme);
										if (IsGlobalName(found_scope_it))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::PrependAssign(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Global()));
										}
										else if (IsLocalName(find_result))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::PrependAssign(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Local(find_result->second.m_relative_index.value())));
										}
										else
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::PrependAssign(variable_expr.m_name, std::move(right_expr), MidoriExpression::NameContext::Cell(find_result->second.m_absolute_index.value())));
										}
									}
									return std::unexpected<std::string>(GenerateParserError("Unbound name.", variable_expr.m_name));
								}
								return std::unexpected<std::string>(GenerateParserError("Invalid prepend assignment target (must be a variable).", op));
							}
						);
				}
				else if (Match(Token::Name::PLUS_EQUAL, Token::Name::MINUS_EQUAL, Token::Name::STAR_EQUAL, Token::Name::SLASH_EQUAL, Token::Name::PERCENT_EQUAL, Token::Name::AMPERSAND_EQUAL, Token::Name::BAR_EQUAL, Token::Name::CARET_EQUAL, Token::Name::LEFT_SHIFT_EQUAL, Token::Name::RIGHT_SHIFT_EQUAL))
				{
					Token& op = Previous();
					return ParseBind()
						.and_then
						(
							[this, &left_expr, &op](std::unique_ptr<MidoriExpression>&& right_expr) -> MidoriResult::ExpressionResult
							{
								if (left_expr->IsExpression<MidoriExpression::BoundedName>())
								{
									MidoriExpression::BoundedName& variable_expr = left_expr->GetExpression<MidoriExpression::BoundedName>();
									std::vector<Scope>::const_reverse_iterator found_scope_it = FindVariableScope(variable_expr.m_name.m_lexeme);

									if (found_scope_it != m_scopes.crend())
									{
										Scope::VariableTable::const_iterator find_result = found_scope_it->m_variables.find(variable_expr.m_name.m_lexeme);
										if (IsGlobalName(found_scope_it))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::CompoundAssign(variable_expr.m_name, op, std::move(right_expr), MidoriExpression::NameContext::Global()));
										}
										else if (IsLocalName(find_result))
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::CompoundAssign(variable_expr.m_name, op, std::move(right_expr), MidoriExpression::NameContext::Local(find_result->second.m_relative_index.value())));
										}
										else
										{
											return std::make_unique<MidoriExpression>(MidoriExpression::CompoundAssign(variable_expr.m_name, op, std::move(right_expr), MidoriExpression::NameContext::Cell(find_result->second.m_absolute_index.value())));
										}
									}
									return std::unexpected<std::string>(GenerateParserError("Unbound name.", variable_expr.m_name));
								}
								return std::unexpected<std::string>(GenerateParserError("Invalid compound assignment target (must be a variable).", op));
							}
						);
				}

				return left_expr;
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseUnaryLogicalBitwise()
{
	if (Match(Token::Name::BANG, Token::Name::TILDE, Token::Name::HASH))
	{
		Token& op = Previous();
		return ParseUnaryLogicalBitwise()
			.and_then
			(
				[&op](std::unique_ptr<MidoriExpression>&& right) -> MidoriResult::ExpressionResult
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::UnaryPrefix(op, std::move(right)));
				}
			);
	}

	return ParseUnaryArithmetic();
}

MidoriResult::ExpressionResult Parser::ParseUnaryArithmetic()
{
	if (Match(Token::Name::SINGLE_MINUS, Token::Name::SINGLE_PLUS))
	{
		Token& op = Previous();
		return ParseUnaryArithmetic()
			.and_then
			(
				[&op](std::unique_ptr<MidoriExpression>&& right) -> MidoriResult::ExpressionResult
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::UnaryPrefix(op, std::move(right)));
				}
			);
	}
	else if (Match(Token::Name::ASYNC))
	{
		return ParseAsyncExpression();
	}
	else if (Match(Token::Name::AWAIT))
	{
		return ParseAwaitExpression();
	}
	else
	{
		return ParseConstruct();
	}
}

MidoriResult::ExpressionResult Parser::ParseExpression()
{
	return ParseAs();
}

MidoriResult::ExpressionResult Parser::ParseAs()
{
	return ParseBind()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& expr) ->MidoriResult::ExpressionResult
			{
				while (Match(Token::Name::AS))
				{
					Token& as = Previous();
					MidoriResult::TypeResult type = ParseType();
					if (!type.has_value())
					{
						return std::unexpected<std::string>(type.error());
					}

					expr = std::make_unique<MidoriExpression>(MidoriExpression::As(as, std::move(type.value()), std::move(expr)));
				}

				return expr;
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseArrayAccessHelper(std::unique_ptr<MidoriExpression>&& arr_var)
{
	Token& op = Previous();

	// If Previous() is RIGHT_LEFT_BRACKET (from array literal ending like [1,2,3][0]),
	// we already have the '[' part consumed, so skip the Consume
	if (op.m_token_name == Token::Name::RIGHT_LEFT_BRACKET)
	{
		return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriExpression>>
			(
				[this]() { return ParseBind(); },
				[this]() { return Consume(Token::Name::RIGHT_LEFT_BRACKET, "Expected '][' after index."); },
				[this]() { return Consume(Token::Name::RIGHT_BRACKET, "Expected ']' after index."); }
			)
			.and_then
			(
				[&op, &arr_var](std::vector<std::unique_ptr<MidoriExpression>>&& indices) ->MidoriResult::ExpressionResult
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::ArrayGet(op, std::move(indices), std::move(arr_var)));
				}
			);
	}
	else
	{
		// Normal case: consume LEFT_BRACKET
		return Consume(Token::Name::LEFT_BRACKET, "Expected '[' before index.")
			.and_then
			(
				[&op, &arr_var, this](Token&&) ->MidoriResult::ExpressionResult
				{
					return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriExpression>>
						(
							[this]() { return ParseBind(); },
							[this]() { return Consume(Token::Name::RIGHT_LEFT_BRACKET, "Expected '][' after index."); },
							[this]() { return Consume(Token::Name::RIGHT_BRACKET, "Expected ']' after index."); }
						)
						.and_then
						(
							[&op, &arr_var](std::vector<std::unique_ptr<MidoriExpression>>&& indices) ->MidoriResult::ExpressionResult
							{
								return std::make_unique<MidoriExpression>(MidoriExpression::ArrayGet(op, std::move(indices), std::move(arr_var)));
							}
						);
				}
			);
	}
}

MidoriResult::ExpressionResult Parser::ParseArrayAccess()
{
	return ParsePrimary()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& arr_var) -> MidoriResult::ExpressionResult
			{
				// Check if we can do array access:
				// Either next token is '[' OR previous token was '][' (from array literal ending)
				bool has_bracket_for_access = Check(Token::Name::LEFT_BRACKET, 0) || Previous().m_token_name == Token::Name::RIGHT_LEFT_BRACKET;

				return has_bracket_for_access
					? ParseArrayAccessHelper(std::move(arr_var))
					: std::move(arr_var);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseCall()
{
	std::function<MidoriResult::ExpressionResult(std::unique_ptr<MidoriExpression>&&)> parse_call_aux_fun =
		[&parse_call_aux_fun, this](std::unique_ptr<MidoriExpression>&& expr) -> MidoriResult::ExpressionResult
		{
			if (Match(Token::Name::LEFT_PAREN))
			{
				return FinishCall(std::move(expr))
					.and_then
					(
						[&parse_call_aux_fun](std::unique_ptr<MidoriExpression>&& expr) -> MidoriResult::ExpressionResult
						{
							return parse_call_aux_fun(std::move(expr));
						}
					);
			}
			else if (Match(Token::Name::SINGLE_DOT))
			{
				return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected identifier after '.'.")
					.and_then
					(
						[&parse_call_aux_fun, &expr](Token&& name) -> MidoriResult::ExpressionResult
						{
							return parse_call_aux_fun(std::make_unique<MidoriExpression>(MidoriExpression::Get(name, std::move(expr))));
						}
					);
			}
			else
			{
				return expr;
			}
		};

	return ParseArrayAccess()
		.and_then
		(
			[&parse_call_aux_fun](std::unique_ptr<MidoriExpression>&& expr)
			{
				return parse_call_aux_fun(std::move(expr));
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseConstruct()
{
	if (Match(Token::Name::NEW))
	{
		if (Match(Token::Name::IDENTIFIER_LITERAL))
		{
			Token base_name_token = Previous();
			MidoriResult::TokenResult data_name_token = MatchNameResolution();
			if (!data_name_token.has_value())
			{
				return std::unexpected<std::string>(data_name_token.error());
			}
			std::vector<std::shared_ptr<MidoriType>> type_args;
			if (Match(Token::Name::LEFT_ANGLE))
			{
				MidoriResult::TypeListResult type_args_result = ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
					(
						[this]() { return ParseType(); },
						[this]() { return Consume(Token::Name::COMMA, "Expected ',' after type argument."); },
						[this]() { return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after type arguments."); }
					);

				if (!type_args_result.has_value())
				{
					return std::unexpected<std::string>(type_args_result.error());
				}

				type_args = std::move(type_args_result.value());
			}

			std::string constructor_name = Mangle(data_name_token.value().m_lexeme);

			Token data_name_token_value = base_name_token;
			data_name_token_value.m_lexeme = constructor_name;

			std::optional<std::shared_ptr<MidoriType>> defined_type = std::nullopt;
			bool is_struct = false;
			for (Scopes::const_reverse_iterator scopes_iter = m_scopes.crbegin(); scopes_iter != m_scopes.crend(); ++scopes_iter)
			{
				const Scope& scope = *scopes_iter;
				if (scope.m_union_constructors.contains(data_name_token_value.m_lexeme))
				{
					defined_type.emplace(scope.m_union_constructors.at(data_name_token_value.m_lexeme));
					break;
				}
				else if (scope.m_struct_constructors.contains(data_name_token_value.m_lexeme))
				{
					is_struct = true;
					defined_type.emplace(scope.m_struct_constructors.at(data_name_token_value.m_lexeme));
					break;
				}
			}

			if (defined_type == std::nullopt)
			{
				return std::unexpected<std::string>(GenerateParserError("Undefined struct.", data_name_token_value));
			}

			// If type arguments were provided, instantiate the generic type
			if (!type_args.empty())
			{
				std::shared_ptr<MidoriType> base_type = defined_type.value();

				// Get generic parameters from the base type
				std::vector<std::string> generic_params;
				if (base_type->IsType<MidoriType::StructType>())
				{
					generic_params = base_type->GetType<MidoriType::StructType>().m_generic_params;
				}
				else if (base_type->IsType<MidoriType::UnionType>())
				{
					generic_params = base_type->GetType<MidoriType::UnionType>().m_generic_params;
				}

				// Check argument count matches parameter count
				if (type_args.size() != generic_params.size())
				{
					return std::unexpected<std::string>
						(
							GenerateParserError
							(
								"Type argument count mismatch: expected " + std::to_string(generic_params.size()) + ", got " + std::to_string(type_args.size()), data_name_token_value
							)
						);
				}

				std::unordered_map<std::string, std::shared_ptr<MidoriType>> substitutions;
				for (size_t i = 0u; i < generic_params.size(); i += 1u)
				{
					substitutions[generic_params[i]] = type_args[i];
				}

				defined_type = MidoriType::SubstituteTypeParams(base_type, substitutions);
			}

			return Consume(Token::Name::LEFT_PAREN, "Expected '(' after type.")
				.and_then
				(
					[&defined_type, &data_name_token_value, is_struct, this](Token&&) ->MidoriResult::ExpressionResult
					{
						return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriExpression>>
							(
								[this]() { return ParseExpression(); },
								[this]() { return Consume(Token::Name::COMMA, "Expected ',' after expression."); },
								[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after arguments."); }
							)
							.and_then
							(
								[&defined_type, &data_name_token_value, is_struct](std::vector<std::unique_ptr<MidoriExpression>>&& arguments)->MidoriResult::ExpressionResult
								{
									std::shared_ptr<MidoriType> defined_type_copy = defined_type.value();
									if (is_struct)
									{
										std::unique_ptr<MidoriExpression> cons_struct_expr = std::make_unique<MidoriExpression>(MidoriExpression::Construct(data_name_token_value, std::move(arguments), std::move(defined_type_copy), MidoriExpression::Construct::Struct{}));
										return cons_struct_expr;
									}
									else
									{
										const MidoriType::UnionType& union_type = defined_type.value()->GetType<MidoriType::UnionType>();
										std::unique_ptr<MidoriExpression> cons_union_expr = std::make_unique<MidoriExpression>(MidoriExpression::Construct(data_name_token_value, std::move(arguments), std::move(defined_type_copy), MidoriExpression::Construct::Union(union_type.m_member_info.at(data_name_token_value.m_lexeme).m_tag)));
										return cons_union_expr;
									}
								}
							)
							.or_else
							(
								[&data_name_token_value, this](std::string&& original_error) ->MidoriResult::ExpressionResult
								{
									return std::unexpected<std::string>(std::move(original_error));
								}
							);
					}
				);
		}
		else
		{
			return std::unexpected<std::string>(GenerateParserError("Expected struct name after 'new'.", Previous()));
		}
	}
	else
	{
		return ParseCall();
	}
}

MidoriResult::ExpressionResult Parser::FinishCall(std::unique_ptr<MidoriExpression>&& callee)
{
	return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriExpression>>
		(
			[this]() { return ParseExpression(); },
			[this]() { return Consume(Token::Name::COMMA, "Expected ',' after expression."); },
			[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after arguments."); }
		)
		.and_then
		(
			[&callee, this](std::vector<std::unique_ptr<MidoriExpression>>&& arguments) ->MidoriResult::ExpressionResult
			{
				return std::make_unique<MidoriExpression>(MidoriExpression::Call(Previous(), std::move(callee), std::move(arguments)));
			}
		);
}

MidoriResult::ExpressionResult Parser::ParsePrimary()
{
	if (Match(Token::Name::LEFT_BRACE))
	{
		return ParseBlockExpression();
	}
	else if (Match(Token::Name::LEFT_PAREN))
	{
		Token left_paren = Previous();

		if (Match(Token::Name::RIGHT_PAREN))
		{
			// Empty tuple is unit
			return std::make_unique<MidoriExpression>(MidoriExpression::UnitLiteral(Previous()));
		}

		return ParseExpression()
			.and_then
			(
				[this, left_paren](std::unique_ptr<MidoriExpression>&& first_expr) -> MidoriResult::ExpressionResult
				{
					if (Match(Token::Name::COMMA))
					{
						// It's a tuple - parse remaining elements
						std::vector<std::unique_ptr<MidoriExpression>> elements;
						elements.push_back(std::move(first_expr));

						// Parse remaining tuple elements
						do
						{
							MidoriResult::ExpressionResult elem_result = ParseExpression();
							if (!elem_result)
							{
								return elem_result;
							}
							elements.push_back(std::move(elem_result.value()));
						} while (Match(Token::Name::COMMA));

						return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after tuple elements.")
							.and_then
							(
								[&elements, left_paren](Token&&) -> MidoriResult::ExpressionResult
								{
									return std::make_unique<MidoriExpression>(MidoriExpression::Tuple(left_paren, std::move(elements)));
								}
							);
					}
					else
					{
						return Consume(Token::Name::RIGHT_PAREN, "Expected right parentheses.")
							.and_then
							(
								[&first_expr](Token&&) -> MidoriResult::ExpressionResult
								{
									return std::make_unique<MidoriExpression>(MidoriExpression::Group(std::move(first_expr)));
								}
							);
					}
				}
			);
	}
	else if (Match(Token::Name::IDENTIFIER_LITERAL))
	{
		return MatchNameResolution()
			.and_then
			(
				[this](Token&& variable) -> MidoriResult::ExpressionResult
				{
					std::string mangled_name = Mangle(variable.m_lexeme);
					std::string symbol_name = ExtractSymbolName(variable.m_lexeme);
					std::string qualifier = ExtractQualifier(variable.m_lexeme);

					// Only check CanAccessSymbol for unqualified names
					// Qualified names (Module::Symbol) bypass this check and are validated below
					if (qualifier.empty() && !CanAccessSymbol(symbol_name))
					{
						std::string error_msg = "Symbol '"s + symbol_name + "' is not accessible"s;
						if (m_module_declarations != nullptr)
						{
							for (const auto& [file_path, module_decl] : *m_module_declarations)
							{
								if (module_decl.HasExport(symbol_name))
								{
									VisibilityLevel visibility = module_decl.GetExportVisibility(symbol_name);
									if (visibility == VisibilityLevel::Private)
									{
										error_msg += "\n  Note: '"s + symbol_name + "' is marked as 'private export' in module "s + module_decl.m_module_name;
										error_msg += "\n  Note: Only modules in the "s + module_decl.m_module_name.substr(0, module_decl.m_module_name.find_last_of('.')) + " namespace can access it"s;
									}
									else if (visibility == VisibilityLevel::Internal)
									{
										error_msg += "\n  Note: '"s + symbol_name + "' is not exported from module "s + module_decl.m_module_name;
										error_msg += "\n  Suggestion: Add it to a 'public export' or 'private export' block"s;
									}
									break;
								}
							}
						}

						error_msg += "\n  Hint: Use 'use "s + std::string(m_module_declarations != nullptr && std::ranges::any_of(*m_module_declarations, [&symbol_name](const auto& pair) { return pair.second.HasExport(symbol_name); }) ? "ModuleName"s : ""s) + ".{"s + symbol_name + "}' to import it, or use qualified access like 'ModuleName"s + NameSeparator.data() + symbol_name + "'"s;
						return std::unexpected<std::string>(GenerateParserError(std::move(error_msg), variable));
					}

					// Check if this is a module-qualified name
					if (!qualifier.empty())
					{
						// Qualified class method call (e.g., Show::show)
						for (const MidoriType::ClassConstraint& constraint : m_active_constraints)
						{
							if (constraint.m_class_name != qualifier)
							{
								continue;
							}

							std::unordered_map<std::string, std::unordered_set<std::string>>::const_iterator tc_it = m_class_methods.find(constraint.m_class_name);
							if (tc_it != m_class_methods.cend() && tc_it->second.contains(symbol_name))
							{
								return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(variable, MidoriExpression::NameContext::Global()));
							}
						}

						const bool using_new_path = (m_module_declarations == nullptr);

						if (using_new_path)
						{
							// New path: Use imported symbols
							if (ResolveQualifiedSymbol(qualifier, symbol_name))
							{
								// Symbol is accessible - create a global reference
								// (The symbol was defined in another module and is accessible)
								// Keep the fully qualified name so the code generator can identify imports
								return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(variable, MidoriExpression::NameContext::Global()));
							}
							else
							{
								// Check if module exists in imports
								std::unordered_map<std::string, CompiledModule::SymbolTable>::const_iterator it = m_imported_symbols.find(qualifier);
								if (it == m_imported_symbols.cend())
								{
									return std::unexpected<std::string>(GenerateParserError("Module '"s + qualifier + "' not found"s, variable));
								}
								else
								{
									return std::unexpected<std::string>(GenerateParserError("Symbol '"s + symbol_name + "' is not exported by module '"s + qualifier + "'"s, variable));
								}
							}
						}
					}

					return ResolveQualifiedName(variable, mangled_name);
				}
			);
	}
	else if (Match(Token::Name::FUNCTION))
	{
		return ParseFunctionExpression();
	}
	else if (Match(Token::Name::TRUE, Token::Name::FALSE))
	{
		return std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(Previous()));
	}
	else if (Match(Token::Name::FLOAT_LITERAL))
	{
		return std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(Previous()));
	}
	else if (Match(Token::Name::INTEGER_LITERAL))
	{
		Token& token = Previous();
		const std::string& lexeme = token.m_lexeme;

		// Check if it's a hex or binary literal
		if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X' || lexeme[1u] == 'b' || lexeme[1u] == 'B'))
		{
			uint64_t value = 0u;
			if (lexeme[1u] == 'x' || lexeme[1u] == 'X')
			{
				value = std::stoull(lexeme, nullptr, 16);
			}
			else
			{
				value = std::stoull(lexeme, nullptr, 2);
			}

			// Determine type based on value
			if (value <= 0xFF)
			{
				// Fits in Byte (0-255)
				return std::make_unique<MidoriExpression>(MidoriExpression::ByteLiteral(token));
			}
			else if (value <= static_cast<uint64_t>(std::numeric_limits<int64_t>::max()))
			{
				// Fits in signed Int
				return std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(token));
			}
			else
			{
				// Needs Word (unsigned 64-bit)
				return std::make_unique<MidoriExpression>(MidoriExpression::WordLiteral(token));
			}
		}
		else
		{
			// Decimal literal - keep as IntegerLiteral for backwards compatibility
			return std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(token));
		}
	}
	else if (Match(Token::Name::TEXT_LITERAL))
	{
		return std::make_unique<MidoriExpression>(MidoriExpression::TextLiteral(Previous()));
	}
	else if (Match(Token::Name::LEFT_BRACKET))
	{
		Token& op = Previous();

		// Check for empty array first
		if (Match(Token::Name::RIGHT_BRACKET) || Match(Token::Name::RIGHT_LEFT_BRACKET))
		{
			return std::make_unique<MidoriExpression>(MidoriExpression::Array(op, {}));
		}

		// Look ahead to detect array comprehension [expr for x in range]
		// If detected, we need to register the loop variable BEFORE parsing the transform expression
		std::optional<int> comp_var_offset = DetectArrayComprehension();
		if (comp_var_offset.has_value())
		{
			return ParseArrayComprehension(op);
		}

		// Otherwise, parse as normal array literal
		return ParseExpression()
			.and_then
			(
				[&op, this](std::unique_ptr<MidoriExpression>&& first_expr) -> MidoriResult::ExpressionResult
				{
					if (Match(Token::Name::COMMA))
					{
						// Parse remaining elements
						return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriExpression>>
							(
								[this]() { return ParseExpression(); },
								[this]() { return Consume(Token::Name::COMMA, "Expected ',' after expression."); },
								[this]() -> MidoriResult::TokenResult
								{
									if (Match(Token::Name::RIGHT_BRACKET))
									{
										return Previous();
									}
									else if (Match(Token::Name::RIGHT_LEFT_BRACKET))
									{
										return Previous();
									}
									else
									{
										return std::unexpected<std::string>(GenerateParserError("Expected ']' for array expression.", Peek(0)));
									}
								}
							)
							.and_then
							(
								[&op, first_expr = std::move(first_expr)](std::vector<std::unique_ptr<MidoriExpression>>&& expressions) mutable -> MidoriResult::ExpressionResult
								{
									expressions.insert(expressions.begin(), std::move(first_expr));
									return std::make_unique<MidoriExpression>(MidoriExpression::Array(op, std::move(expressions)));
								}
							);
					}
					else if (Match(Token::Name::RIGHT_BRACKET) || Match(Token::Name::RIGHT_LEFT_BRACKET))
					{
						// Single element array
						std::vector<std::unique_ptr<MidoriExpression>> expressions;
						expressions.emplace_back(std::move(first_expr));
						return std::make_unique<MidoriExpression>(MidoriExpression::Array(op, std::move(expressions)));
					}
					else
					{
						return std::unexpected<std::string>(GenerateParserError("Expected ',' or ']' after array element.", Peek(0)));
					}
				}
			);
	}
	else if (Match(Token::Name::IF))
	{
		return ParseIfElseExpression();
	}
	else if (Match(Token::Name::MATCH))
	{
		return ParseMatchExpression();
	}
	else if (Match(Token::Name::LOOP))
	{
		return ParseLoopExpression();
	}
	else if (Match(Token::Name::FOR))
	{
		return ParseForExpression();
	}
	else if (Match(Token::Name::BREAK))
	{
		return ParseBreakExpression();
	}
	else if (Match(Token::Name::RETURN))
	{
		return ParseReturnExpression();
	}
	else
	{
		return std::unexpected<std::string>(GenerateParserError("Expected expression.", Peek(0)));
	}
}

MidoriResult::ExpressionResult Parser::ParseLogicalAnd()
{
	return ParseBinary(&Parser::ParseBitwiseOr, Token::Name::DOUBLE_AMPERSAND);
}

MidoriResult::ExpressionResult Parser::ParseLogicalOr()
{
	return ParseBinary(&Parser::ParseLogicalAnd, Token::Name::DOUBLE_BAR);
}

MidoriResult::ExpressionResult Parser::ParsePipe()
{
	return ParseLogicalOr()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& left_expr) -> MidoriResult::ExpressionResult
			{
				while (Match(Token::Name::BAR_BRACKET))
				{
					Token& pipe_op = Previous();
					MidoriResult::ExpressionResult right = ParseLogicalOr();
					if (!right.has_value())
					{
						return std::unexpected<std::string>(std::move(right.error()));
					}

					// Transform pipe into call expression
					// x |> f(y) becomes f(x, y)
					// x |> f becomes f(x)
					if (right.value()->IsExpression<MidoriExpression::Call>())
					{
						MidoriExpression::Call& call_expr = right.value()->GetExpression<MidoriExpression::Call>();
						call_expr.m_arguments.insert(call_expr.m_arguments.begin(), std::move(left_expr));
						left_expr = std::move(right.value());
					}
					else
					{
						std::vector<std::unique_ptr<MidoriExpression>> arguments;
						arguments.emplace_back(std::move(left_expr));
						left_expr = std::make_unique<MidoriExpression>(MidoriExpression::Call(pipe_op, std::move(right.value()), std::move(arguments)));
					}
				}

				return left_expr;
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseBlockExpression()
{
	std::function<MidoriResult::ExpressionResult(std::vector<std::unique_ptr<MidoriStatement>>&&, std::unique_ptr<MidoriExpression>&&)> build_block = [this](std::vector<std::unique_ptr<MidoriStatement>>&& stmts, std::unique_ptr<MidoriExpression>&& final_expr) -> MidoriResult::ExpressionResult
		{
			return Consume(Token::Name::RIGHT_BRACE, "Expected '}' after block expression.")
				.and_then
				(
					[&stmts, &final_expr, this](Token&& right_brace)
					{
						int block_local_count = EndScope();
						return final_expr != nullptr
							? MidoriResult::ExpressionResult(std::make_unique<MidoriExpression>(MidoriExpression::Block(right_brace, std::move(stmts), block_local_count, std::move(final_expr))))
							: MidoriResult::ExpressionResult(std::make_unique<MidoriExpression>(MidoriExpression::Block(right_brace, std::move(stmts), block_local_count)));
					}
				);
		};

	BeginScope();
	return ParseZeroOrMoreUnlimited<std::unique_ptr<MidoriStatement>>([this]() { return ParseDeclaration(); })
		.and_then
		(
			[&build_block, this](std::vector<std::unique_ptr<MidoriStatement>>&& stmts) ->MidoriResult::ExpressionResult
			{
				return TryParser<std::unique_ptr<MidoriExpression>>
					(
						[this]() -> MidoriResult::ExpressionResult
						{
							// Peek ahead: if next token is '}', do NOT try to parse an expression
							if (Check(Token::Name::RIGHT_BRACE, 0))
							{
								return std::unexpected<std::string>("");  // Signal: no expression expected
							}
							else
							{
								return ParseExpression();
							}
						}
					)
					.and_then
					(
						[&stmts, &build_block](std::unique_ptr<MidoriExpression>&& final_expr) -> MidoriResult::ExpressionResult
						{
							return build_block(std::move(stmts), std::move(final_expr));
						}
					)
					.or_else
					(
						[&stmts, &build_block](std::string&& err) -> MidoriResult::ExpressionResult
						{
							// Only fall back if this was an intentional absence
							if (err.empty())
							{
								return build_block(std::move(stmts), nullptr);
							}
							else
							{
								return std::unexpected<std::string>(std::move(err));
							}
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseBreakExpression()
{
	Token& keyword = Previous();
	if (m_local_count_before_loop.empty())
	{
		return std::unexpected<std::string>(GenerateParserError("'break' must be used inside a loop.", keyword));
	}
	else
	{
		return ParseExpression()
			.and_then
			(
				[&keyword, this](std::unique_ptr<MidoriExpression>&& expr)->MidoriResult::ExpressionResult
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::Break(keyword, m_total_variables - m_local_count_before_loop.top(), std::move(expr)));
				}
			);
	}
}

MidoriResult::ExpressionResult Parser::ParseReturnExpression()
{
	Token& keyword = Previous();
	if (m_function_depth == 0)
	{
		return std::unexpected<std::string>(GenerateParserError("'return' must be used inside a function.", keyword));
	}
	else
	{
		return ParseExpression()
			.and_then
			(
				[&keyword, this](std::unique_ptr<MidoriExpression>&& expr) ->MidoriResult::ExpressionResult
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::Return(keyword, std::move(expr)));
				}
			);
	}
}

MidoriResult::ExpressionResult Parser::ParseLoopExpression()
{
	Token& keyword = Previous();
	m_local_count_before_loop.emplace(m_total_variables);

	return ParseExpression()
		.and_then
		(
			[&keyword, this](std::unique_ptr<MidoriExpression>&& body)->MidoriResult::ExpressionResult
			{
				m_local_count_before_loop.pop();
				return std::make_unique<MidoriExpression>(MidoriExpression::Loop(keyword, std::move(body)));
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseForExpression()
{
	Token& for_keyword = Previous();

	if (!Match(Token::Name::IDENTIFIER_LITERAL))
	{
		return std::unexpected<std::string>(GenerateParserError("Expected identifier after 'for'.", Peek(0)));
	}
	Token loop_variable = Previous();

	if (!Match(Token::Name::IN))
	{
		return std::unexpected<std::string>(GenerateParserError("Expected 'in' after loop variable.", Peek(0)));
	}
	Token in_keyword = Previous();

	return ParseExpression()
		.and_then
		(
			[&for_keyword, &loop_variable, &in_keyword, this](std::unique_ptr<MidoriExpression>&& range)->MidoriResult::ExpressionResult
			{
				static int s_for_counter = 0;
				BeginScope();

				// Add loop variable to scope
				std::string var_name(loop_variable.m_lexeme);
				std::optional<int> local_index = RegisterOrUpdateLocalVariable(var_name);
				int var_index = m_total_variables - 1; // The index that was just assigned

				// Reserve additional local variable slots for hidden loop state values
				// These are not actual variables that can be referenced by name, but they need
				// to occupy local variable slots to prevent conflicts with body variables
				// For range iteration: step and end
				// For array iteration: current index, length, and array reference
				// Names use '$' prefix which is not valid in user identifiers
				RegisterOrUpdateLocalVariable(std::string(FOR_STEP_PREFIX) + std::to_string(s_for_counter));
				int hidden_step_index = m_total_variables - 1;

				RegisterOrUpdateLocalVariable(std::string(FOR_END_PREFIX) + std::to_string(s_for_counter));
				int hidden_end_index = m_total_variables - 1;

				RegisterOrUpdateLocalVariable(std::string(FOR_ARRAY_PREFIX) + std::to_string(s_for_counter));
				int hidden_array_index = m_total_variables - 1;
				s_for_counter += 1;

				// NOW set the loop local count, after the 4 for loop variables are registered
				// This ensures continue/break don't try to pop these loop control variables
				m_local_count_before_loop.emplace(m_total_variables);

				return ParseExpression()
					.and_then
					(
						[&for_keyword, &loop_variable, &in_keyword, range = std::move(range), var_index, hidden_step_index, hidden_end_index, hidden_array_index, this](std::unique_ptr<MidoriExpression>&& body) mutable ->MidoriResult::ExpressionResult
						{
							EndScope();

							m_local_count_before_loop.pop();
							std::unique_ptr<MidoriExpression> for_expr = std::make_unique<MidoriExpression>(MidoriExpression::For(for_keyword, loop_variable, in_keyword, std::move(range), std::move(body)));
							MidoriExpression::For& for_expr_ref = for_expr->GetExpression<MidoriExpression::For>();
							for_expr_ref.m_loop_variable_index = var_index;
							for_expr_ref.m_hidden_step_index = hidden_step_index;
							for_expr_ref.m_hidden_end_index = hidden_end_index;
							for_expr_ref.m_hidden_array_index = hidden_array_index;
							return for_expr;
						}
					);
			}
		);
}

std::optional<int> Parser::DetectArrayComprehension()
{
	// Look ahead to find `for identifier in` pattern at bracket depth 0
	// Returns offset to the identifier token if found, std::nullopt otherwise
	// We're positioned right after '[', looking for: expr for identifier in range ]

	int offset = 0;
	int bracket_depth = 0;
	int paren_depth = 0;
	int brace_depth = 0;

	while (!IsAtEnd())
	{
		Token::Name current = Peek(offset).m_token_name;

		// Track nesting
		if (current == Token::Name::LEFT_BRACKET || current == Token::Name::RIGHT_LEFT_BRACKET)
		{
			bracket_depth += 1;
		}
		else if (current == Token::Name::RIGHT_BRACKET)
		{
			if (bracket_depth == 0)
			{
				// Reached end of array without finding comprehension
				return std::nullopt;
			}
			bracket_depth -= 1;
		}
		else if (current == Token::Name::LEFT_PAREN)
		{
			paren_depth += 1;
		}
		else if (current == Token::Name::RIGHT_PAREN)
		{
			paren_depth -= 1;
		}
		else if (current == Token::Name::LEFT_BRACE)
		{
			brace_depth += 1;
		}
		else if (current == Token::Name::RIGHT_BRACE)
		{
			brace_depth -= 1;
		}
		else if (current == Token::Name::FOR && bracket_depth == 0 && paren_depth == 0 && brace_depth == 0)
		{
			// Found 'for' at depth 0, check for 'identifier in' pattern
			if (Peek(offset + 1).m_token_name == Token::Name::IDENTIFIER_LITERAL && Peek(offset + 2).m_token_name == Token::Name::IN)
			{
				// Return offset to the identifier
				return offset + 1;
			}
		}
		else if (current == Token::Name::COMMA && bracket_depth == 0 && paren_depth == 0 && brace_depth == 0)
		{
			// Comma at depth 0 means this is a regular array literal
			return std::nullopt;
		}

		offset += 1;

		// Safety limit to prevent infinite loop
		if (offset > MAX_ARRAY_SIZE)
		{
			return std::nullopt;
		}
	}

	return std::nullopt;
}

MidoriResult::ExpressionResult Parser::ParseArrayComprehension(Token& bracket)
{
	std::optional<int> var_offset = DetectArrayComprehension();
	if (!var_offset.has_value())
	{
		return std::unexpected<std::string>(GenerateParserError("Internal error: comprehension detection failed.", Peek(0)));
	}

	Token loop_variable = Peek(var_offset.value());

	static int s_comp_counter = 0;
	BeginScope();

	// Register loop variable FIRST so it's in scope for the transform expression
	std::string var_name(loop_variable.m_lexeme);
	RegisterOrUpdateLocalVariable(var_name);
	int var_index = m_total_variables - 1;

	// Reserve hidden variable slots (similar to For loop)
	RegisterOrUpdateLocalVariable(std::string(FOR_STEP_PREFIX) + std::to_string(s_comp_counter));
	int hidden_step_index = m_total_variables - 1;

	RegisterOrUpdateLocalVariable(std::string(FOR_END_PREFIX) + std::to_string(s_comp_counter));
	int hidden_end_index = m_total_variables - 1;

	RegisterOrUpdateLocalVariable(std::string(FOR_ARRAY_PREFIX) + std::to_string(s_comp_counter));
	int hidden_array_index = m_total_variables - 1;

	RegisterOrUpdateLocalVariable(std::string(COMPREHENSION_RESULT_PREFIX) + std::to_string(s_comp_counter));
	int result_array_index = m_total_variables - 1;

	s_comp_counter += 1;

	return ParseExpression()
		.and_then
		(
			[&bracket, &loop_variable, var_index, hidden_step_index, hidden_end_index, hidden_array_index, result_array_index, this](std::unique_ptr<MidoriExpression>&& transform_expr) -> MidoriResult::ExpressionResult
			{
				if (!Match(Token::Name::FOR))
				{
					EndScope();
					return std::unexpected<std::string>(GenerateParserError("Expected 'for' in array comprehension.", Peek(0)));
				}

				if (!Match(Token::Name::IDENTIFIER_LITERAL))
				{
					EndScope();
					return std::unexpected<std::string>(GenerateParserError("Expected identifier after 'for' in array comprehension.", Peek(0)));
				}
				Token actual_loop_var = Previous();

				if (!Match(Token::Name::IN))
				{
					EndScope();
					return std::unexpected<std::string>(GenerateParserError("Expected 'in' after loop variable in array comprehension.", Peek(0)));
				}
				Token in_keyword = Previous();

				return ParseExpression()
					.and_then
					(
						[&bracket, &actual_loop_var, &in_keyword, var_index, hidden_step_index, hidden_end_index, hidden_array_index, result_array_index, transform_expr = std::move(transform_expr), this](std::unique_ptr<MidoriExpression>&& range) mutable -> MidoriResult::ExpressionResult
						{
							if (!Match(Token::Name::RIGHT_BRACKET) && !Match(Token::Name::RIGHT_LEFT_BRACKET))
							{
								EndScope();
								return std::unexpected<std::string>(GenerateParserError("Expected ']' after array comprehension.", Peek(0)));
							}

							EndScope();

							std::unique_ptr<MidoriExpression> comp_expr = std::make_unique<MidoriExpression>(MidoriExpression::ArrayComprehension(bracket, actual_loop_var, in_keyword, std::move(transform_expr), std::move(range)));

							MidoriExpression::ArrayComprehension& comp_ref = comp_expr->GetExpression<MidoriExpression::ArrayComprehension>();
							comp_ref.m_loop_variable_index = var_index;
							comp_ref.m_hidden_step_index = hidden_step_index;
							comp_ref.m_hidden_end_index = hidden_end_index;
							comp_ref.m_hidden_array_index = hidden_array_index;
							comp_ref.m_result_array_index = result_array_index;

							return comp_expr;
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseDefineStatement()
{
	if (Match(Token::Name::LEFT_PAREN))
	{
		std::vector<Token> names;
		std::vector<std::optional<int>> local_indices;

		do
		{
			MidoriResult::TokenResult name_result = Consume(Token::Name::IDENTIFIER_LITERAL, "Expected identifier in tuple pattern.");
			if (!name_result)
			{
				return std::unexpected(name_result.error());
			}

			Token var_name = std::move(name_result.value());
			constexpr bool is_variable = true;

			MidoriResult::TokenResult defined_name_result = DefineName(var_name, is_variable);
			if (!defined_name_result)
			{
				return std::unexpected(defined_name_result.error());
			}

			Token defined_name = std::move(defined_name_result.value());
			std::optional<int> local_index = RegisterOrUpdateLocalVariable(defined_name.m_lexeme);

			names.emplace_back(std::move(defined_name));
			local_indices.emplace_back(std::move(local_index));

		} while (Match(Token::Name::COMMA));

		return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after tuple pattern.")
			.and_then
			(
				[&names, &local_indices, this](Token&&) -> MidoriResult::StatementResult
				{
					return Consume(Token::Name::SINGLE_EQUAL, "Expected '=' after tuple pattern.")
						.and_then
						(
							[&names, &local_indices, this](Token&&) -> MidoriResult::StatementResult
							{
								return ParseExpression()
									.and_then
									(
										[&names, &local_indices, this](std::unique_ptr<MidoriExpression>&& expr) -> MidoriResult::StatementResult
										{
											return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after tuple binding.")
												.and_then
												(
													[&names, &local_indices, &expr](Token&&) -> MidoriResult::StatementResult
													{
														return std::make_unique<MidoriStatement>(MidoriStatement::DefineTuple(std::move(names), std::move(expr), std::move(local_indices)));
													}
												);
										}
									);
							}
						);
				}
			);
	}

	// Single variable: def x = ...
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected name.")
		.and_then
		(
			[this](Token&& var_name) -> MidoriResult::StatementResult
			{
				constexpr bool is_variable = true;
				return DefineName(var_name, is_variable)
					.and_then
					(
						[this](Token&& define_name) -> MidoriResult::StatementResult
						{
							std::function<MidoriResult::StatementResult(std::optional<std::shared_ptr<MidoriType>>)> def_aux_func = [&define_name, this](std::optional<std::shared_ptr<MidoriType>> type_annotation)
								{
									std::optional<int> local_index = RegisterOrUpdateLocalVariable(define_name.m_lexeme);

									return Consume(Token::Name::SINGLE_EQUAL, "Expected '=' after defining a name.")
										.and_then
										(
											[&define_name, &type_annotation, &local_index, this](Token&&) -> MidoriResult::StatementResult
											{
												return ParseExpression()
													.and_then
													(
														[&define_name, &type_annotation, &local_index, this](std::unique_ptr<MidoriExpression>&& expr) -> MidoriResult::StatementResult
														{
															return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after name binding.")
																.and_then
																(
																	[&define_name, &expr, &type_annotation, &local_index](Token&&) -> MidoriResult::StatementResult
																	{
																		return std::make_unique<MidoriStatement>(MidoriStatement::Define(define_name, std::move(expr), std::move(type_annotation), std::move(local_index)));
																	}
																);
														}
													);
											}
										);
								};

							if (Match(Token::Name::SINGLE_COLON))
							{
								return ParseType()
									.and_then
									(
										[&def_aux_func](std::shared_ptr<MidoriType>&& type)->MidoriResult::StatementResult
										{
											return def_aux_func(std::move(type));
										}
									);
							}
							else
							{
								return def_aux_func(std::nullopt);
							}
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseDefineFunctionStatement()
{
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected function name.")
		.and_then
		(
			[this](Token&& func_name) -> MidoriResult::StatementResult
			{
				constexpr bool is_variable = true;
				return DefineName(func_name, is_variable)
					.and_then
					(
						[this](Token&& func_name) -> MidoriResult::StatementResult
						{
							std::optional<int> local_index = RegisterOrUpdateLocalVariable(func_name.m_lexeme);

							// Parse optional generic parameters <T, U, ...>
							// Create scope BEFORE parsing so DefineName() in ParseGenericParameters adds them to this scope
							std::vector<Token> generic_params;
							std::vector<std::shared_ptr<MidoriType>> generic_param_types;
							bool has_generic_params = false;

							if (Match(Token::Name::LEFT_ANGLE))
							{
								has_generic_params = true;
								BeginScope();  // Create scope for generic parameters

								MidoriResult::TokenListResult generic_parse_result = ParseGenericParameters(&generic_param_types);
								if (!generic_parse_result.has_value())
								{
									EndScope();  // Clean up scope on error
									return std::unexpected<std::string>(generic_parse_result.error());
								}

								generic_params = std::move(generic_parse_result.value());
							}

							return Consume(Token::Name::LEFT_PAREN, "Expected '(' before function parameters.")
								.and_then
								(
									[&func_name, &generic_params, &generic_param_types, &local_index, has_generic_params, this](Token&&) -> MidoriResult::StatementResult
									{
										m_function_depth += 1;
										int prev_total_locals = m_total_locals_in_curr_scope;
										m_total_locals_in_curr_scope = 0;
										BeginScope();

										MidoriResult::FunctionParamsResult params_parse_result = ParseFunctionParameters();

										if (!params_parse_result.has_value())
										{
											EndScope();
											m_total_locals_in_curr_scope = prev_total_locals;
											m_function_depth -= 1;

											// Close the generic parameter scope if it was created
											if (has_generic_params)
											{
												EndScope();
											}

											return std::unexpected<std::string>(params_parse_result.error());
										}

										std::vector<std::pair<Token, std::shared_ptr<MidoriType>>> param_tuples = std::move(params_parse_result.value());
										std::vector<Token> params;
										std::vector<std::shared_ptr<MidoriType>> param_types;

										std::ranges::transform(param_tuples, std::back_inserter(params), [](auto&& tuple) { return std::move(std::get<0>(tuple)); });
										std::ranges::transform(param_tuples, std::back_inserter(param_types), [](auto&& tuple) { return std::move(std::get<1>(tuple)); });

										return Consume(Token::Name::SINGLE_COLON, "Expected ':' before return type.")
											.and_then
											(
												[&func_name, &generic_params, &generic_param_types, &params, &param_types, &local_index, has_generic_params, prev_total_locals, this](Token&&) -> MidoriResult::StatementResult
												{
													return ParseType()
														.and_then
														(
															[&func_name, &generic_params, &generic_param_types, &params, &param_types, &local_index, has_generic_params, prev_total_locals, this](std::shared_ptr<MidoriType>&& return_type) -> MidoriResult::StatementResult
															{
																std::vector<MidoriType::ClassConstraint> constraints;
																size_t prev_constraints_size = m_active_constraints.size();
																if (Match(Token::Name::WHERE))
																{
																	std::function<std::expected<MidoriType::ClassConstraint, std::string>()> parse_constraint = [this]() -> std::expected<MidoriType::ClassConstraint, std::string>
																	{
																		return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected class name in constraint.")
																			.and_then
																			(
																				[this](Token&& first_token) -> std::expected<MidoriType::ClassConstraint, std::string>
																				{
																					Token typeclass_name = std::move(first_token);
																					return Consume(Token::Name::LEFT_ANGLE, "Expected '<' after class name in constraint (e.g., 'Show<T>').")
																						.and_then
																						(
																							[&typeclass_name, this](Token&&) -> std::expected<MidoriType::ClassConstraint, std::string>
																							{
																								return ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
																									(
																										[this]() { return ParseType(); },
																										[this]() { return Consume(Token::Name::COMMA, "Expected ',' between type arguments."); },
																										[this]() { return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after type arguments."); }
																									)
																									.and_then
																									(
																										[&typeclass_name](std::vector<std::shared_ptr<MidoriType>>&& type_args) -> std::expected<MidoriType::ClassConstraint, std::string>
																										{
																											return MidoriType::ClassConstraint{ typeclass_name.m_lexeme, std::move(type_args) };
																										}
																									);
																							}
																						);
																				}
																			);
																	};

																	std::expected<std::vector<MidoriType::ClassConstraint>, std::string> constraints_result = ParseDelimitedZeroOrMoreUnlimited<MidoriType::ClassConstraint>
																		(
																			parse_constraint,
																			[this]() { return Consume(Token::Name::COMMA, "Expected ',' between constraints."); }
																		);

																	if (!constraints_result.has_value())
																	{
																		return std::unexpected<std::string>(constraints_result.error());
																	}

																	constraints = std::move(constraints_result.value());

																	if (constraints.empty())
																	{
																		return std::unexpected<std::string>(GenerateParserError("Expected at least one constraint after 'where' keyword.", func_name));
																	}

																	m_active_constraints.insert(m_active_constraints.end(), constraints.begin(), constraints.end());
																}

																struct ActiveConstraintGuard
																{
																	Parser* m_parser = nullptr;
																	size_t m_prev_size = 0u;
																	~ActiveConstraintGuard()
																	{
																		if (m_parser != nullptr)
																		{
																			m_parser->m_active_constraints.resize(m_prev_size);
																		}
																	}
																};
																ActiveConstraintGuard constraint_guard{ this, prev_constraints_size };

																return Consume(Token::Name::FAT_ARROW, "Expected '=>' before function body.")
																	.and_then
																	(
																		[&func_name, &generic_params, &generic_param_types, &params, &param_types, &return_type, &constraints, &local_index, has_generic_params, prev_total_locals, this](Token&&) -> MidoriResult::StatementResult
																		{
																			return ParseExpression()
																				.and_then
																				(
																					[&func_name, &generic_params, &generic_param_types, &params, &param_types, &return_type, &constraints, &local_index, has_generic_params, prev_total_locals, this](std::unique_ptr<MidoriExpression>&& body) -> MidoriResult::StatementResult
																					{
																						return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after function body.")
																							.and_then
																							(
																								[&func_name, &generic_params, &generic_param_types, &params, &param_types, &return_type, &constraints, &body, &local_index, has_generic_params, prev_total_locals, this](Token&&) -> MidoriResult::StatementResult
																								{
																									EndScope();
																									m_total_locals_in_curr_scope = prev_total_locals;
																									m_function_depth -= 1;

																									if (has_generic_params)
																									{
																										EndScope();
																									}

																									std::vector<MidoriType::ClassConstraint> constraints_copy = constraints;
																									return std::make_unique<MidoriStatement>(MidoriStatement::DefineFunction(func_name, std::move(generic_params), std::move(params), std::move(param_types), std::move(return_type), std::move(body), std::move(local_index), m_total_variables, std::move(constraints_copy)));
																								}
																							);
																					}
																				);
																		}
																	);
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseStructDeclaration()
{
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected struct name.")
		.and_then
		(
			[this](Token&& struct_name)->MidoriResult::StatementResult
			{
				struct_name.m_lexeme = Mangle(struct_name.m_lexeme);
				if (struct_name.m_lexeme[0u] != std::toupper(struct_name.m_lexeme[0u]))
				{
					return std::unexpected<std::string>(GenerateParserError("Struct name must start with a capital letter.", struct_name));
				}

				constexpr bool is_variable = false;
				return DefineName(struct_name, is_variable)
					.and_then
					(
						[this](Token&& struct_name)->MidoriResult::StatementResult
						{
							// Parse optional generic parameters <T, U, ...>
							// Create scope BEFORE parsing so DefineName() in ParseGenericParameters adds them to this scope
							std::vector<Token> generic_params;
							std::vector<std::shared_ptr<MidoriType>> generic_param_types;
							bool has_generic_params = false;

							if (Match(Token::Name::LEFT_ANGLE))
							{
								has_generic_params = true;
								BeginScope();  // Create scope for generic parameters

								MidoriResult::TokenListResult generic_parse_result = ParseGenericParameters(&generic_param_types);
								if (!generic_parse_result.has_value())
								{
									EndScope();  // Clean up scope on error
									return std::unexpected<std::string>(generic_parse_result.error());
								}

								generic_params = std::move(generic_parse_result.value());
							}

							return Consume(Token::Name::LEFT_BRACE, "Expected '{' before struct body.")
								.and_then
								(
									[&struct_name, &generic_params, &generic_param_types, has_generic_params, this](Token&&) ->MidoriResult::StatementResult
									{
										return ParseDelimitedZeroOrMoreLimited<std::tuple<std::shared_ptr<MidoriType>, std::string>>
											(
												[&struct_name, this]()
												{
													return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected struct member name.")
														.and_then
														(
															[&struct_name, this](Token&& identifier)
															{
																return Consume(Token::Name::SINGLE_COLON, "Expected ':' before struct member type token.")
																	.and_then
																	(
																		[&struct_name, &identifier, this](Token&&)
																		{
																			return ParseType()
																				.and_then
																				(
																					[&struct_name, &identifier, this](std::shared_ptr<MidoriType>&& type) -> std::expected<std::tuple<std::shared_ptr<MidoriType>, std::string>, std::string>
																					{
																						return std::make_tuple(std::move(type), identifier.m_lexeme);
																					}
																				);
																		}
																	);
															}
														);
												},
												[this]() { return Consume(Token::Name::COMMA, "Expected ',' struct member."); },
												[this]() { return Consume(Token::Name::RIGHT_BRACE, "Expected '}' struct members."); }
											)
											.and_then
											(
												[&struct_name, &generic_params, has_generic_params, this](std::vector<std::tuple<std::shared_ptr<MidoriType>, std::string>>&& tuples) ->MidoriResult::StatementResult
												{
													return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after struct body.")
														.and_then
														(
															[&tuples, &struct_name, &generic_params, has_generic_params, this](Token&&) ->MidoriResult::StatementResult
															{
																std::vector<std::shared_ptr<MidoriType>> member_types;
																std::vector<std::string> member_names;

																std::ranges::transform(tuples, std::back_inserter(member_types), [](auto&& tuple) { return std::move(std::get<0>(tuple)); });
																std::ranges::transform(tuples, std::back_inserter(member_names), [](auto&& tuple) { return std::move(std::get<1>(tuple)); });

																// Extract generic param names
																std::vector<std::string> generic_param_names;
																std::ranges::transform(generic_params, std::back_inserter(generic_param_names), [](const Token& tok) { return tok.m_lexeme; });

																std::shared_ptr<MidoriType> struct_type = MidoriType::MakeStructType(struct_name.m_lexeme, std::move(member_types), std::move(member_names), std::move(generic_param_names));

																// End the generic param scope if it was created
																if (has_generic_params)
																{
																	EndScope();
																}

																m_scopes.back().m_struct_constructors[struct_name.m_lexeme] = struct_type;
																m_scopes.back().m_defined_types[struct_name.m_lexeme] = struct_type;

																return std::make_unique<MidoriStatement>(MidoriStatement::Struct(std::move(struct_name), std::move(generic_params), std::move(struct_type)));
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseUnionDeclaration()
{
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected union name.")
		.and_then
		(
			[this](Token&& union_name) -> MidoriResult::StatementResult
			{
				std::string union_name_before_mangle = union_name.m_lexeme;
				union_name.m_lexeme = Mangle(union_name.m_lexeme);

				if (union_name.m_lexeme[0u] != std::toupper(union_name.m_lexeme[0u]))
				{
					return std::unexpected<std::string>(GenerateParserError("Union name must start with a capital letter.", union_name));
				}

				constexpr bool is_variable = false;
				return DefineName(union_name, is_variable)
					.and_then
					(
						[&union_name_before_mangle, this](Token&& union_name) -> MidoriResult::StatementResult
						{
							// Parse optional generic parameters <T, U, ...>
							std::vector<Token> generic_params;
							std::vector<std::shared_ptr<MidoriType>> generic_param_types;
							bool has_generic_params = false;

							if (Match(Token::Name::LEFT_ANGLE))
							{
								has_generic_params = true;
								BeginScope();

								MidoriResult::TokenListResult generic_parse_result = ParseGenericParameters(&generic_param_types);
								if (!generic_parse_result.has_value())
								{
									EndScope();
									return std::unexpected<std::string>(generic_parse_result.error());
								}

								generic_params = std::move(generic_parse_result.value());
							}

							// Extract generic param names
							std::vector<std::string> generic_param_names;
							std::ranges::transform(generic_params, std::back_inserter(generic_param_names), [](const Token& tok) { return tok.m_lexeme; });

							int tag = 0;
							std::shared_ptr<MidoriType> union_type = MidoriType::MakeUnionType(union_name.m_lexeme, std::move(generic_param_names));
							MidoriType::UnionType& union_type_ref = union_type->GetType<MidoriType::UnionType>();

							size_t type_scope_idx = has_generic_params ? m_scopes.size() - 2uz : m_scopes.size() - 1uz;
							m_scopes[type_scope_idx].m_defined_types[union_name.m_lexeme] = union_type;
							m_namespaces.emplace_back(union_name_before_mangle);

							return Consume(Token::Name::SINGLE_EQUAL, "Expected '=' before union body.")
								.and_then
								(
									[&union_type_ref, &union_type, &union_name, &tag, &generic_params, &generic_param_types, has_generic_params, this](Token&&) mutable -> MidoriResult::StatementResult
									{
										return ParseDelimitedZeroOrMoreUnlimited<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>>
											(
												[&tag, this]() -> std::expected<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>, std::string>
												{
													return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected union member name.")
														.and_then
														(
															[&tag, this](Token&& member_name) mutable -> std::expected<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>, std::string>
															{
																member_name.m_lexeme = Mangle(member_name.m_lexeme);
																return DefineName(member_name, is_variable)
																	.and_then
																	(
																		[&tag, this](Token&& member_name) mutable -> std::expected<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>, std::string>
																		{
																			if (Match(Token::Name::LEFT_PAREN))
																			{
																				return ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
																					(
																						[this]() { return ParseType(); },
																						[this]() { return Consume(Token::Name::COMMA, "Expected ',' after type."); },
																						[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after union constructor."); }
																					)
																					.and_then
																					(
																						[&tag, &member_name](std::vector<std::shared_ptr<MidoriType>>&& types) -> std::expected<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>, std::string>
																						{
																							std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int> return_val = std::make_tuple(member_name.m_lexeme, std::move(types), tag);
																							tag += 1;
																							return return_val;
																						}
																					);
																			}
																			else
																			{
																				std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int> return_val = std::make_tuple(member_name.m_lexeme, std::vector<std::shared_ptr<MidoriType>>(), tag);
																				tag += 1;
																				return return_val;
																			}
																		}
																	);
															}
														);
												},
												[this]() { return Consume(Token::Name::SINGLE_BAR, "Expected '|' after a union member."); }
											)
											.and_then
											(
												[&union_type_ref, &union_type, &union_name, &generic_params, has_generic_params, this](std::vector<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>>&& result)
												{
													std::ranges::for_each
													(
														result,
														[&union_type_ref](auto&& elem)
														{
															auto&& [name, types, tag] = elem;
															union_type_ref.m_member_info.emplace(std::move(name), MidoriType::UnionType::UnionMemberContext(std::move(types), tag));
														}
													);

													std::ranges::for_each
													(
														union_type_ref.m_member_info,
														[union_type, has_generic_params, this](const auto& member_info_entry)
														{
															// Store constructors in the parent scope (where the union is declared)
															// If we have generic params, we're one scope level deeper, so go back one
															if (has_generic_params)
															{
																m_scopes[m_scopes.size() - 2].m_union_constructors[member_info_entry.first] = union_type;
															}
															else
															{
																m_scopes.back().m_union_constructors[member_info_entry.first] = union_type;
															}
														}
													);

													return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after union body.")
														.and_then
														(
															[&union_name, &union_type, &generic_params, has_generic_params, this](Token&&) -> MidoriResult::StatementResult
															{
																m_namespaces.pop_back();

																if (has_generic_params)
																{
																	EndScope();
																}

																return std::make_unique<MidoriStatement>(MidoriStatement::Union(std::move(union_name), std::move(generic_params), std::move(union_type)));
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseClassDeclaration()
{
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected class name.")
		.and_then
		(
			[this](Token&& typeclass_name) -> MidoriResult::StatementResult
			{
				typeclass_name.m_lexeme = Mangle(typeclass_name.m_lexeme);
				if (typeclass_name.m_lexeme[0u] != std::toupper(typeclass_name.m_lexeme[0u]))
				{
					return std::unexpected<std::string>(GenerateParserError("Class name must start with a capital letter.", typeclass_name));
				}

				constexpr bool is_variable = false;
				return DefineName(typeclass_name, is_variable)
					.and_then
					(
						[this](Token&& typeclass_name) -> MidoriResult::StatementResult
						{
							std::vector<Token> type_params;
							std::vector<std::shared_ptr<MidoriType>> type_param_types;
							bool has_type_params = false;

							if (Match(Token::Name::LEFT_ANGLE))
							{
								has_type_params = true;
								BeginScope();

								MidoriResult::TokenListResult generic_parse_result = ParseGenericParameters(&type_param_types);
								if (!generic_parse_result.has_value())
								{
									EndScope();
									return std::unexpected<std::string>(generic_parse_result.error());
								}

								type_params = std::move(generic_parse_result.value());
							}

							if (!has_type_params)
							{
								return std::unexpected<std::string>(GenerateParserError("Class must have at least one type parameter.", typeclass_name));
							}

							std::vector<std::string> type_param_names;
							std::ranges::transform(type_params, std::back_inserter(type_param_names), [](const Token& tok) { return tok.m_lexeme; });
							m_typeclass_type_params[typeclass_name.m_lexeme] = type_param_names;

							return Consume(Token::Name::LEFT_BRACE, "Expected '{' before class body.")
								.and_then
								(
									[&typeclass_name, &type_params, this](Token&&) -> MidoriResult::StatementResult
									{
										return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriStatement>>
											(
												[&typeclass_name, this]() -> MidoriResult::StatementResult
												{
													return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected method name.")
														.and_then
														(
															[&typeclass_name, this](Token&& method_name) -> MidoriResult::StatementResult
															{
																std::string method_name_str = method_name.m_lexeme;

																return Consume(Token::Name::SINGLE_COLON, "Expected ':' after method name.")
																	.and_then
																	(
																		[&typeclass_name, &method_name, &method_name_str, this](Token&&) -> MidoriResult::StatementResult
																		{
																			return Consume(Token::Name::FUNCTION, "Expected 'fn' in method signature.")
																				.and_then
																				(
																					[&typeclass_name, &method_name, &method_name_str, this](Token&&) -> MidoriResult::StatementResult
																					{
																						return Consume(Token::Name::LEFT_PAREN, "Expected '(' before method parameters.")
																							.and_then
																							(
																								[&typeclass_name, &method_name, &method_name_str, this](Token&&) -> MidoriResult::StatementResult
																								{
																									return ParseDelimitedZeroOrMoreLimited<std::tuple<Token, std::shared_ptr<MidoriType>>>
																										(
																											[this]() -> std::expected<std::tuple<Token, std::shared_ptr<MidoriType>>, std::string>
																											{
																												return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected parameter name.")
																													.and_then
																													(
																														[this](Token&& param_name) -> std::expected<std::tuple<Token, std::shared_ptr<MidoriType>>, std::string>
																														{
																															return Consume(Token::Name::SINGLE_COLON, "Expected ':' after parameter name.")
																																.and_then
																																(
																																	[&param_name, this](Token&&) -> std::expected<std::tuple<Token, std::shared_ptr<MidoriType>>, std::string>
																																	{
																																		return ParseType()
																																			.and_then
																																			(
																																				[&param_name](std::shared_ptr<MidoriType>&& type) -> std::expected<std::tuple<Token, std::shared_ptr<MidoriType>>, std::string>
																																				{
																																					return std::make_tuple(std::move(param_name), std::move(type));
																																				}
																																			);
																																	}
																																);
																														}
																													);
																											},
																											[this]() { return Consume(Token::Name::COMMA, "Expected ',' between parameters."); },
																											[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after parameters."); }
																										)
																										.and_then
																										(
																											[&typeclass_name, &method_name, &method_name_str, this](std::vector<std::tuple<Token, std::shared_ptr<MidoriType>>>&& params) -> MidoriResult::StatementResult
																											{
																												return Consume(Token::Name::THIN_ARROW, "Expected '->' before return type.")
																													.and_then
																													(
																														[&typeclass_name, &method_name, &method_name_str, &params, this](Token&&) -> MidoriResult::StatementResult
																														{
																															return ParseType()
																																.and_then
																																(
																																	[&typeclass_name, &method_name, &method_name_str, &params, this](std::shared_ptr<MidoriType>&& return_type) -> MidoriResult::StatementResult
																																	{
																																		std::vector<std::shared_ptr<MidoriType>> param_types;
																																		std::vector<Token> param_tokens;
																																		param_types.reserve(params.size());
																																		param_tokens.reserve(params.size());

																																		for (std::tuple<Token, std::shared_ptr<MidoriType>>& tuple : params)
																																		{
																																			param_types.emplace_back(std::get<1>(tuple));
																																			param_tokens.emplace_back(std::move(std::get<0>(tuple)));
																																		}

																																		std::vector<std::shared_ptr<MidoriType>> param_types_copy = param_types;
																																		std::shared_ptr<MidoriType> return_type_copy = return_type;
																																		std::shared_ptr<MidoriType> method_type = MidoriType::MakeFunctionType(std::move(param_types_copy), std::move(return_type_copy));

																																		m_class_methods[typeclass_name.m_lexeme].insert(method_name_str);
																																		m_typeclass_method_types[typeclass_name.m_lexeme][method_name_str] = method_type;

																																		return std::make_unique<MidoriStatement>(MidoriStatement::DefineFunction(method_name, std::vector<Token>(), std::move(param_tokens), std::move(param_types), std::move(return_type), nullptr, std::nullopt, 0, std::vector<MidoriType::ClassConstraint>()));
																																	}
																																);
																														}
																													);
																											}
																										);
																								}
																							);
																					}
																				);
																		}
																	);
															}
														);
												},
												[this]() { return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after method signature."); },
												[this]() { return Consume(Token::Name::RIGHT_BRACE, "Expected '}' after class methods."); }
											)
											.and_then
											(
												[&typeclass_name, &type_params, this](std::vector<std::unique_ptr<MidoriStatement>>&& methods) -> MidoriResult::StatementResult
												{
													return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after class body.")
														.and_then
														(
															[&typeclass_name, &type_params, &methods, this](Token&&) -> MidoriResult::StatementResult
															{
																EndScope();

																return std::make_unique<MidoriStatement>(MidoriStatement::Class(std::move(typeclass_name), std::move(type_params), std::vector<MidoriType::ClassConstraint>(), std::move(methods)));
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseInstanceDeclaration()
{
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected class name.")
		.and_then
		(
			[this](Token&& typeclass_name) -> MidoriResult::StatementResult
			{
				if (!m_class_methods.contains(typeclass_name.m_lexeme))
				{
					return std::unexpected<std::string>(GenerateParserError("Unknown class '" + typeclass_name.m_lexeme + "'.", typeclass_name));
				}

				return Consume(Token::Name::LEFT_ANGLE, "Expected '<' before type arguments.")
					.and_then
					(
						[&typeclass_name, this](Token&&) -> MidoriResult::StatementResult
						{
							return ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
								(
									[this]() { return ParseType(); },
									[this]() { return Consume(Token::Name::COMMA, "Expected ',' between type arguments."); },
									[this]() { return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after type arguments."); }
								)
								.and_then
								(
									[&typeclass_name, this](std::vector<std::shared_ptr<MidoriType>>&& type_args) -> MidoriResult::StatementResult
									{
										if (type_args.empty())
										{
											return std::unexpected<std::string>(GenerateParserError("Instance must have at least one type argument.", typeclass_name));
										}

										return Consume(Token::Name::LEFT_BRACE, "Expected '{' before instance methods.")
											.and_then
											(
												[&typeclass_name, &type_args, this](Token&&) -> MidoriResult::StatementResult
												{
													return ParseZeroOrMoreLimited<std::unique_ptr<MidoriStatement>>
														(
															[this]() -> MidoriResult::StatementResult
															{
																if (!Match(Token::Name::DEFUN))
																{
																	return std::unexpected<std::string>("Expected 'defun' for method implementation.");
																}

																// Parse instance method manually to avoid DefineName() scope conflicts
																return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected method name.")
																	.and_then
																	(
																		[this](Token&& method_name) -> MidoriResult::StatementResult
																		{
																			std::vector<Token> generic_params;
																			bool has_generic_params = false;

																			if (Match(Token::Name::LEFT_ANGLE))
																			{
																				has_generic_params = true;
																				BeginScope();

																				MidoriResult::TokenListResult generic_parse_result = ParseGenericParameters(nullptr);
																				if (!generic_parse_result.has_value())
																				{
																					EndScope();
																					return std::unexpected<std::string>(generic_parse_result.error());
																				}

																				generic_params = std::move(generic_parse_result.value());
																			}

																			return Consume(Token::Name::LEFT_PAREN, "Expected '(' before method parameters.")
																				.and_then
																				(
																					[&method_name, &generic_params, has_generic_params, this](Token&&) -> MidoriResult::StatementResult
																					{
																						m_function_depth += 1;
																						int prev_total_locals = m_total_locals_in_curr_scope;
																						m_total_locals_in_curr_scope = 0;
																						BeginScope();

																						MidoriResult::FunctionParamsResult params_parse_result = ParseFunctionParameters();

																						if (!params_parse_result.has_value())
																						{
																							EndScope();
																							m_total_locals_in_curr_scope = prev_total_locals;
																							m_function_depth -= 1;

																							if (has_generic_params)
																							{
																								EndScope();
																							}

																							return std::unexpected<std::string>(params_parse_result.error());
																						}

																						std::vector<std::pair<Token, std::shared_ptr<MidoriType>>> param_tuples = std::move(params_parse_result.value());
																						std::vector<Token> params;
																						std::vector<std::shared_ptr<MidoriType>> param_types;

																						std::ranges::transform(param_tuples, std::back_inserter(params), [](auto&& tuple) { return std::move(std::get<0>(tuple)); });
																						std::ranges::transform(param_tuples, std::back_inserter(param_types), [](auto&& tuple) { return std::move(std::get<1>(tuple)); });

																						return Consume(Token::Name::SINGLE_COLON, "Expected ':' before return type.")
																							.and_then
																							(
																								[&method_name, &generic_params, &params, &param_types, has_generic_params, prev_total_locals, this](Token&&) -> MidoriResult::StatementResult
																								{
																									return ParseType()
																										.and_then
																										(
																											[&method_name, &generic_params, &params, &param_types, has_generic_params, prev_total_locals, this](std::shared_ptr<MidoriType>&& return_type) -> MidoriResult::StatementResult
																											{
																												return Consume(Token::Name::FAT_ARROW, "Expected '=>' before method body.")
																													.and_then
																													(
																														[&method_name, &generic_params, &params, &param_types, &return_type, has_generic_params, prev_total_locals, this](Token&&) -> MidoriResult::StatementResult
																														{
																															return ParseExpression()
																																.and_then
																																(
																																	[&method_name, &generic_params, &params, &param_types, &return_type, has_generic_params, prev_total_locals, this](std::unique_ptr<MidoriExpression>&& body) -> MidoriResult::StatementResult
																																	{
																																		return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after method body.")
																																			.and_then
																																			(
																																				[&method_name, &generic_params, &params, &param_types, &return_type, &body, has_generic_params, prev_total_locals, this](Token&&) -> MidoriResult::StatementResult
																																				{
																																					EndScope();
																																					m_total_locals_in_curr_scope = prev_total_locals;
																																					m_function_depth -= 1;

																																					if (has_generic_params)
																																					{
																																						EndScope();
																																					}

																																					return std::make_unique<MidoriStatement>(
																																						MidoriStatement::DefineFunction(
																																							method_name,
																																							std::move(generic_params),
																																							std::move(params),
																																							std::move(param_types),
																																							std::move(return_type),
																																							std::move(body),
																																							std::nullopt,
																																							0,
																																							std::vector<MidoriType::ClassConstraint>()
																																						)
																																					);
																																				}
																																			);
																																	}
																																);
																														}
																													);
																											}
																										);
																								}
																							);
																					}
																				);
																		}
																	);
															},
															[this]() { return Consume(Token::Name::RIGHT_BRACE, "Expected '}' after instance methods."); }
														)
														.and_then
														(
															[&typeclass_name, &type_args, this](std::vector<std::unique_ptr<MidoriStatement>>&& methods) -> MidoriResult::StatementResult
															{
																return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after instance body.")
																	.and_then
																	(
																		[&typeclass_name, &type_args, &methods, this](Token&&) -> MidoriResult::StatementResult
																		{
																			for (std::unique_ptr<MidoriStatement>& method_stmt : methods)
																			{
																				if (method_stmt->IsStatement<MidoriStatement::DefineFunction>())
																				{
																					MidoriStatement::DefineFunction& defun = method_stmt->GetStatement<MidoriStatement::DefineFunction>();

																					std::string method_name = defun.m_name.m_lexeme;
																					std::string mangled_name = std::string(1, INTERNAL_NAME_PREFIX) + method_name + "_" + typeclass_name.m_lexeme;
																					for (const std::shared_ptr<MidoriType>& type_arg : type_args)
																					{
																						mangled_name += "_" + type_arg->ToString();
																					}

																					// Track the mangled instance method WITH module suffix for cross-module resolution
																					std::string mangled_name_with_module = mangled_name;
																					if (m_current_module && m_current_module->m_has_module_declaration)
																					{
																						mangled_name_with_module += ModuleSeparator + m_current_module->m_module_name;
																					}
																					m_class_instances[typeclass_name.m_lexeme].push_back(mangled_name_with_module);

																					// Update the method's name to the mangled version (without module suffix - CodeGenerator adds it)
																					defun.m_name.m_lexeme = mangled_name;
																				}
																			}

																			return std::make_unique<MidoriStatement>(MidoriStatement::Instance(std::move(typeclass_name), std::move(type_args), std::vector<MidoriType::ClassConstraint>(), std::move(methods)));
																		}
																	);
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseTypeAliasDeclaration()
{
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected type alias name.")
		.and_then
		(
			[this](Token&& alias_name) -> MidoriResult::StatementResult
			{
				alias_name.m_lexeme = Mangle(alias_name.m_lexeme);
				if (alias_name.m_lexeme[0u] != std::toupper(alias_name.m_lexeme[0u]))
				{
					return std::unexpected<std::string>(GenerateParserError("Type alias name must start with a capital letter.", alias_name));
				}

				constexpr bool is_variable = false;
				return DefineName(alias_name, is_variable)
					.and_then
					(
						[this](Token&& alias_name) -> MidoriResult::StatementResult
						{
							std::vector<Token> generic_params;
							std::vector<std::shared_ptr<MidoriType>> generic_param_types;
							bool has_generic_params = false;

							if (Match(Token::Name::LEFT_ANGLE))
							{
								has_generic_params = true;
								BeginScope();

								MidoriResult::TokenListResult generic_parse_result = ParseGenericParameters(&generic_param_types);
								if (!generic_parse_result.has_value())
								{
									EndScope();
									return std::unexpected<std::string>(generic_parse_result.error());
								}

								generic_params = std::move(generic_parse_result.value());
							}

							return Consume(Token::Name::SINGLE_EQUAL, "Expected '=' after type alias name.")
								.and_then
								(
									[&alias_name, &generic_params, has_generic_params, this](Token&&) -> MidoriResult::StatementResult
									{
										return ParseType()
											.and_then
											(
												[&alias_name, &generic_params, has_generic_params, this](std::shared_ptr<MidoriType>&& aliased_type) -> MidoriResult::StatementResult
												{
													return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after type alias definition.")
														.and_then
														(
															[&alias_name, &generic_params, &aliased_type, has_generic_params, this](Token&&) -> MidoriResult::StatementResult
															{
																if (has_generic_params)
																{
																	EndScope();
																}

																m_scopes.back().m_defined_types[alias_name.m_lexeme] = aliased_type;

																return std::make_unique<MidoriStatement>(MidoriStatement::TypeAlias(std::move(alias_name), std::move(generic_params), std::move(aliased_type)));
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseContinueStatement()
{
	Token& keyword = Previous();

	if (m_local_count_before_loop.empty())
	{
		return std::unexpected<std::string>(GenerateParserError("'continue' must be used inside a loop.", keyword));
	}

	return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after \"continue\".")
		.and_then
		(
			[&keyword, this](Token&&) ->MidoriResult::StatementResult
			{
				return std::make_unique<MidoriStatement>(MidoriStatement::Continue(keyword, m_total_variables - m_local_count_before_loop.top() - 1));
			}
		);
}

MidoriResult::StatementResult Parser::ParseSimpleStatement()
{
	return ParseExpression()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& expr) ->MidoriResult::StatementResult
			{
				return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after expression.")
					.and_then
					(
						[&expr](Token&& semi_colon) ->MidoriResult::StatementResult
						{
							return std::make_unique<MidoriStatement>(MidoriStatement::Simple(semi_colon, std::move(expr)));
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseForeignStatement()
{
	return Consume(Token::Name::TEXT_LITERAL, "Expected name used in library.")
		.and_then
		(
			[this](Token&& foreign_name) ->MidoriResult::StatementResult
			{
				return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected foreign function name.")
					.and_then
					(
						[this, &foreign_name](Token&& function_name) ->MidoriResult::StatementResult
						{
							function_name.m_lexeme = Mangle(function_name.m_lexeme);

							return Consume(Token::Name::SINGLE_COLON, "Expected ':' before foreign function type.")
								.and_then
								(
									[&foreign_name, &function_name, this](Token&&) ->MidoriResult::StatementResult
									{
										constexpr bool is_variable = true;
										return DefineName(function_name, is_variable)
											.and_then
											(
												[&foreign_name, &function_name, this](Token&& name) ->MidoriResult::StatementResult
												{
													std::optional<int> local_index = RegisterOrUpdateLocalVariable(name.m_lexeme);
													constexpr bool is_foreign = true;

													return ParseType(is_foreign)
														.and_then
														(
															[&foreign_name, &function_name, &local_index, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::StatementResult
															{
																if (!type->IsType<MidoriType::FunctionType>())
																{
																	return std::unexpected<std::string>(GenerateParserError("'foreign' only applies to function types.", function_name));
																}

																return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after foreign function type.")
																	.and_then
																	(
																		[&foreign_name, &function_name, &type, &local_index](Token&&) ->MidoriResult::StatementResult
																		{
																			return std::make_unique<MidoriStatement>(MidoriStatement::Foreign(function_name, foreign_name.m_lexeme, std::move(type), std::move(local_index)));
																		}
																	);
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseMatchExpression()
{
	Token& match_keyword = Previous();
	return ParseExpression()
		.and_then
		(
			[&match_keyword, this](std::unique_ptr<MidoriExpression>&& expr) ->MidoriResult::ExpressionResult
			{
				return Consume(Token::Name::WITH, "Expected 'with' after match expression.")
					.and_then
					(
						[&expr, &match_keyword, this](Token&&) ->MidoriResult::ExpressionResult
						{
							bool default_visited = false;
							std::unordered_set<std::string> visited_names;
							std::vector<std::unique_ptr<MidoriExpression>> cases;

							while (Check(Token::Name::CASE, 0) || Check(Token::Name::DEFAULT, 0))
							{
								if (Match(Token::Name::CASE))
								{
									Token& case_keyword = Previous();
									MidoriResult::ExpressionResult case_result = ParseCaseExpression(visited_names, case_keyword);
									if (!case_result.has_value())
									{
										return std::unexpected<std::string>(std::move(case_result.error()));
									}
									cases.emplace_back(std::move(case_result.value()));
								}
								else if (Match(Token::Name::DEFAULT))
								{
									Token& default_keyword = Previous();
									MidoriResult::ExpressionResult default_result = ParseDefaultExpression(default_visited, default_keyword);
									if (!default_result.has_value())
									{
										return std::unexpected<std::string>(std::move(default_result.error()));
									}
									cases.emplace_back(std::move(default_result.value()));
								}
							}

							if (cases.empty())
							{
								return std::unexpected<std::string>(GenerateParserError("Expected at least one case.", match_keyword));
							}

							return std::make_unique<MidoriExpression>(MidoriExpression::Match(match_keyword, std::move(expr), std::move(cases)));
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseIfElseExpression()
{
	Token& if_token = Previous();
	return ParseExpression()
		.and_then
		(
			[&if_token, this](std::unique_ptr<MidoriExpression>&& condition) -> MidoriResult::ExpressionResult
			{
				return Consume(Token::Name::THEN, "Expected 'then'.")
					.and_then
					(
						[&if_token, &condition, this](Token&& then_token) -> MidoriResult::ExpressionResult
						{
							return ParseExpression()
								.and_then
								(
									[&if_token, &condition, &then_token, this](std::unique_ptr<MidoriExpression>&& true_branch) -> MidoriResult::ExpressionResult
									{
										return Consume(Token::Name::ELSE, "Expected 'else'.")
											.and_then
											(
												[&if_token, &condition, &true_branch, &then_token, this](Token&& else_token) -> MidoriResult::ExpressionResult
												{
													return ParseExpression()
														.and_then
														(
															[&if_token, &condition, &true_branch, &then_token, &else_token](std::unique_ptr<MidoriExpression>&& else_branch) -> MidoriResult::ExpressionResult
															{
																return std::make_unique<MidoriExpression>(MidoriExpression::IfElse(if_token, then_token, else_token, std::move(condition), std::move(true_branch), std::move(else_branch), MidoriExpression::ConditionOperandType::OTHER));
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseFunctionExpression()
{
	Token& keyword = Previous();
	return Consume(Token::Name::LEFT_PAREN, "Expected '(' before function parameters.")
		.and_then
		(
			[&keyword, this](Token&&) -> MidoriResult::ExpressionResult
			{
				m_function_depth += 1;
				int prev_total_locals = m_total_locals_in_curr_scope;
				m_total_locals_in_curr_scope = 0;
				BeginScope();

				MidoriResult::FunctionParamsResult params_parse_result = ParseFunctionParameters();
				if (!params_parse_result.has_value())
				{
					EndScope();
					m_total_locals_in_curr_scope = prev_total_locals;
					m_function_depth -= 1;
					return std::unexpected<std::string>(params_parse_result.error());
				}
				else
				{
					std::vector<std::pair<Token, std::shared_ptr<MidoriType>>> param_tuples = std::move(params_parse_result.value());

					std::vector<Token> params;
					std::vector<std::shared_ptr<MidoriType>> param_types;
					std::ranges::transform(param_tuples, std::back_inserter(params), [](auto&& tuple) { return std::move(std::get<0>(tuple)); });
					std::ranges::transform(param_tuples, std::back_inserter(param_types), [](auto&& tuple) { return std::move(std::get<1>(tuple)); });

					return Consume(Token::Name::SINGLE_COLON, "Expected ':' before return type.")
						.and_then
						(
							[&keyword, &params, &param_types, prev_total_locals, this](Token&&) ->MidoriResult::ExpressionResult
							{
								return ParseType()
									.and_then
									(
										[&keyword, &params, &param_types, prev_total_locals, this](std::shared_ptr<MidoriType>&& return_type) ->MidoriResult::ExpressionResult
										{
											return Consume(Token::Name::FAT_ARROW, "Expected '=>' before function body.")
												.and_then
												(
													[&params, &param_types, &return_type, &keyword, prev_total_locals, this](Token&&) ->MidoriResult::ExpressionResult
													{
														// If body is a block, parse just the block without continuing to parse calls
														// This prevents `fn() => {}()` from parsing `()` as part of the function body
														if (Match(Token::Name::LEFT_BRACE))
														{
															return ParseBlockExpression()
																.and_then
																(
																	[&params, &param_types, &return_type, &keyword, prev_total_locals, this](std::unique_ptr<MidoriExpression>&& return_value) ->MidoriResult::ExpressionResult
																	{
																		EndScope();
																		m_total_locals_in_curr_scope = prev_total_locals;
																		m_function_depth -= 1;
																		return std::make_unique<MidoriExpression>(MidoriExpression::Function(keyword, std::vector<Token>(), std::move(params), std::move(param_types), std::move(return_type), std::move(return_value), m_total_variables));
																	}
																);
														}
														else
														{
															return ParseExpression()
																.and_then
																(
																	[&params, &param_types, &return_type, &keyword, prev_total_locals, this](std::unique_ptr<MidoriExpression>&& return_value) ->MidoriResult::ExpressionResult
																	{
																		EndScope();
																		m_total_locals_in_curr_scope = prev_total_locals;
																		m_function_depth -= 1;
																		return std::make_unique<MidoriExpression>(MidoriExpression::Function(keyword, std::vector<Token>(), std::move(params), std::move(param_types), std::move(return_type), std::move(return_value), m_total_variables));
																	}
																);
														}
													}
												);
										}
									);
							}
						);
				}
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseAsyncExpression()
{
	Token& keyword = Previous();
	return ParseExpression()
		.and_then
		(
			[&keyword](std::unique_ptr<MidoriExpression>&& expr) -> MidoriResult::ExpressionResult
			{
				return std::make_unique<MidoriExpression>(MidoriExpression::Async(keyword, std::move(expr)));
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseAwaitExpression()
{
	Token& keyword = Previous();
	return ParseUnaryArithmetic()
		.and_then
		(
			[&keyword](std::unique_ptr<MidoriExpression>&& expr) -> MidoriResult::ExpressionResult
			{
				return std::make_unique<MidoriExpression>(MidoriExpression::Await(keyword, std::move(expr)));
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseCaseExpression(std::unordered_set<std::string>& visited_members, Token& keyword)
{
	std::function<MidoriResult::ExpressionResult(Token&, std::vector<std::string>&&, Token&&)> handle_body = [this](Token& keyword, std::vector<std::string>&& binding_names, Token&& member_name) -> MidoriResult::ExpressionResult
		{
			return Consume(Token::Name::FAT_ARROW, "Expected '=>' after case.")
				.and_then
				(
					[&keyword, &binding_names, &member_name, this](Token&&)->MidoriResult::ExpressionResult
					{
						return ParseExpression()
							.and_then
							(
								[&keyword, &binding_names, &member_name, this](std::unique_ptr<MidoriExpression>&& case_expr)->MidoriResult::ExpressionResult
								{
									EndScope();
									return std::make_unique<MidoriExpression>(MidoriExpression::Case(keyword, std::move(binding_names), member_name.m_lexeme, std::move(case_expr), 0));
								}
							);
					}
				);
		};
	return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected constructor name.")
		.and_then
		(
			[&handle_body, &visited_members, &keyword, this](Token&&) ->MidoriResult::ExpressionResult
			{
				return MatchNameResolution()
					.and_then
					(
						[&handle_body, &visited_members, &keyword, this](Token&& member_name) ->MidoriResult::ExpressionResult
						{
							member_name.m_lexeme = Mangle(member_name.m_lexeme);

							if (visited_members.contains(member_name.m_lexeme))
							{
								return std::unexpected<std::string>(GenerateParserError("Duplicate case in match statement.", member_name));
							}
							else
							{
								visited_members.emplace(member_name.m_lexeme);
							}

							BeginScope();
							if (Match(Token::Name::LEFT_PAREN))
							{
								return ParseDelimitedZeroOrMoreLimited<std::string>
									(
										[this]() -> std::expected<std::string, std::string>
										{
											return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected field name.")
												.and_then
												(
													[this](Token&& field_name) -> std::expected<std::string, std::string>
													{
														field_name.m_lexeme = Mangle(field_name.m_lexeme);
														constexpr bool is_variable = true;

														return DefineName(field_name, is_variable)
															.and_then
															(
																[this](Token&& field_name) -> std::expected<std::string, std::string>
																{
																	RegisterOrUpdateLocalVariable(field_name.m_lexeme);
																	return field_name.m_lexeme;
																}
															);
													}
												);
										},
										[this]() { return Consume(Token::Name::COMMA, "Expected ',' after parameter."); },
										[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after constructor."); }
									)
									.and_then
									(
										[&keyword, &member_name, &handle_body](std::vector<std::string>&& binding_names) ->MidoriResult::ExpressionResult
										{
											return handle_body(keyword, std::move(binding_names), std::move(member_name));
										}
									);
							}
							else
							{
								return handle_body(keyword, std::vector<std::string>(), std::move(member_name));
							}
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseDefaultExpression(bool& default_visited, Token& keyword)
{
	if (default_visited)
	{
		return std::unexpected<std::string>(GenerateParserError("Cannot have more than one default case.", Previous()));
	}
	else
	{
		default_visited = true;
		return Consume(Token::Name::FAT_ARROW, "Expected '=>' after default.")
			.and_then
			(
				[&keyword, this](Token&&)->MidoriResult::ExpressionResult
				{
					return ParseExpression()
						.and_then
						(
							[&keyword](std::unique_ptr<MidoriExpression>&& case_expr) -> MidoriResult::ExpressionResult
							{
								return std::make_unique<MidoriExpression>(MidoriExpression::Default(keyword, std::move(case_expr)));
							}
						);
				}
			);
	}
}

MidoriResult::StatementResult Parser::ParseStatement()
{
	if (Match(Token::Name::CONTINUE))
	{
		return ParseContinueStatement();
	}
	else
	{
		return ParseSimpleStatement();
	}
}

MidoriResult::TypeResult Parser::ParseType(bool is_foreign)
{
	if (Match(Token::Name::TEXT))
	{
		return MidoriType::MakeLiteralType<MidoriType::TextType>();
	}
	else if (Match(Token::Name::FLOAT))
	{
		return MidoriType::MakeLiteralType<MidoriType::FloatType>();
	}
	else if (Match(Token::Name::INTEGER))
	{
		return MidoriType::MakeLiteralType<MidoriType::IntegerType>();
	}
	else if (Match(Token::Name::BYTE))
	{
		return MidoriType::MakeLiteralType<MidoriType::ByteType>();
	}
	else if (Match(Token::Name::WORD))
	{
		return MidoriType::MakeLiteralType<MidoriType::WordType>();
	}
	else if (Match(Token::Name::BOOL))
	{
		return MidoriType::MakeLiteralType<MidoriType::BoolType>();
	}
	else if (Match(Token::Name::UNIT))
	{
		return MidoriType::MakeLiteralType<MidoriType::UnitType>();
	}
	else if (Match(Token::Name::NEVER))
	{
		return MidoriType::MakeLiteralType<MidoriType::NeverType>();
	}
	else if (Match(Token::Name::ARRAY))
	{
		return Consume(Token::Name::LEFT_ANGLE, "Expected '<' after 'Array'.")
			.and_then
			(
				[this](Token&&) ->MidoriResult::TypeResult
				{
					return ParseType()
						.and_then
						(
							[this](std::shared_ptr<MidoriType>&& type) ->MidoriResult::TypeResult
							{
								return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after array type.")
									.and_then
									(
										[&type](Token&&)->MidoriResult::TypeResult
										{
											return MidoriType::MakeArrayType(type);
										}
									);
							}
						);
				}
			);
	}
	else if (Match(Token::Name::FUTURE))
	{
		return Consume(Token::Name::LEFT_ANGLE, "Expected '<' after 'Future'.")
			.and_then
			(
				[this](Token&&) ->MidoriResult::TypeResult
				{
					return ParseType()
						.and_then
						(
							[this](std::shared_ptr<MidoriType>&& type) ->MidoriResult::TypeResult
							{
								return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after future type.")
									.and_then
									(
										[&type](Token&&)->MidoriResult::TypeResult
										{
											return MidoriType::MakeFutureType(type);
										}
									);
							}
						);
				}
			);
	}
	else if (Match(Token::Name::FUNCTION))
	{
		std::function<MidoriResult::TypeResult(std::vector<std::shared_ptr<MidoriType>>&&)> func_type_aux_func = [is_foreign, this](std::vector<std::shared_ptr<MidoriType>>&& types) ->MidoriResult::TypeResult
			{
				return Consume(Token::Name::THIN_ARROW, "Expected '->' before return type token.")
					.and_then
					(
						[&types, is_foreign, this](Token&&) ->MidoriResult::TypeResult
						{
							return ParseType()
								.and_then
								(
									[&types, is_foreign](std::shared_ptr<MidoriType>&& return_type) ->MidoriResult::TypeResult
									{
										return MidoriType::MakeFunctionType(std::move(types), std::move(return_type), is_foreign);
									}
								);
						}
					);
			};
		return Consume(Token::Name::LEFT_PAREN, "Expected '(' before function argument types.")
			.and_then
			(
				[&func_type_aux_func, this](Token&&)
				{
					if (!Match(Token::Name::RIGHT_PAREN))
					{
						return ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
							(
								[this]() { return ParseType(); },
								[this]() { return Consume(Token::Name::COMMA, "Expected ',' after argument type"); },
								[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after argument types."); }
							)
							.and_then
							(
								[&func_type_aux_func](std::vector<std::shared_ptr<MidoriType>>&& types) ->MidoriResult::TypeResult
								{
									return func_type_aux_func(std::move(types));
								}
							);
					}
					else
					{
						return func_type_aux_func({});
					}
				}
			);
	}
	else if (Match(Token::Name::LEFT_PAREN))
	{
		// Parse tuple type: (Type1, Type2, ...)
		// Empty tuple is Unit
		if (Match(Token::Name::RIGHT_PAREN))
		{
			return MidoriType::MakeLiteralType<MidoriType::UnitType>();
		}

		return ParseType()
			.and_then
			(
				[this](std::shared_ptr<MidoriType>&& first_type) -> MidoriResult::TypeResult
				{
					if (Match(Token::Name::COMMA))
					{
						// It's a tuple - parse remaining types
						std::vector<std::shared_ptr<MidoriType>> element_types;
						element_types.push_back(std::move(first_type));

						// Parse remaining tuple element types
						do
						{
							MidoriResult::TypeResult elem_result = ParseType();
							if (!elem_result)
							{
								return elem_result;
							}
							element_types.push_back(std::move(elem_result.value()));
						} while (Match(Token::Name::COMMA));

						return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after tuple types.")
							.and_then
							(
								[&element_types](Token&&) -> MidoriResult::TypeResult
								{
									return MidoriType::MakeTupleType(std::move(element_types));
								}
							);
					}
					else
					{
						// Just a parenthesized type - return it
						return Consume
						(
							Token::Name::RIGHT_PAREN, "Expected ')' after type.")
							.and_then([&first_type](Token&&) -> MidoriResult::TypeResult
								{
									return first_type;
								}
							);
					}
				}
			);
	}
	else if (Match(Token::Name::IDENTIFIER_LITERAL))
	{
		return MatchNameResolution()
			.and_then
			(
				[this](Token&& type_name) ->MidoriResult::TypeResult
				{
					std::string mangled_name = Mangle(type_name.m_lexeme);
					std::vector<Scope>::const_reverse_iterator found_scope_it = FindTypeScope(type_name.m_lexeme);

					std::shared_ptr<MidoriType> base_type = nullptr;

					if (found_scope_it != m_scopes.crend())
					{
						base_type = found_scope_it->m_defined_types.at(type_name.m_lexeme);
					}
					else
					{
						// Check imported type signatures via use imports
						for (const UseImport& use_import : m_current_use_imports)
						{
							if (use_import.m_symbol_name == type_name.m_lexeme)
							{
								std::unordered_map<std::string, TypeEnvironment>::const_iterator module_it = m_imported_type_signatures.find(use_import.m_module_name);
								if (module_it != m_imported_type_signatures.cend())
								{
									TypeEnvironment::const_iterator type_it = module_it->second.find(type_name.m_lexeme);
									if (type_it != module_it->second.cend())
									{
										base_type = type_it->second;
										break;
									}
								}
							}
						}

						// Check for qualified access (ModuleName::TypeName)
						if (base_type == nullptr)
						{
							std::string qualifier = ExtractQualifier(type_name.m_lexeme);
							if (!qualifier.empty())
							{
								std::string symbol_name = ExtractSymbolName(type_name.m_lexeme);
								std::unordered_map<std::string, TypeEnvironment>::const_iterator module_it = m_imported_type_signatures.find(qualifier);
								if (module_it != m_imported_type_signatures.cend())
								{
									TypeEnvironment::const_iterator type_it = module_it->second.find(symbol_name);
									if (type_it != module_it->second.cend())
									{
										base_type = type_it->second;
									}
								}
							}
						}

						if (base_type == nullptr)
						{
							return std::unexpected<std::string>(GenerateParserError("Undefined struct or union.", type_name));
						}
					}

					// Check if there are generic type arguments
					if (Match(Token::Name::LEFT_ANGLE))
					{
						MidoriResult::TypeListResult type_args_result = ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
							(
								[this]() { return ParseType(); },
								[this]() { return Consume(Token::Name::COMMA, "Expected ',' after type argument."); },
								[this]() { return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after type arguments."); }
							);

						if (!type_args_result.has_value())
						{
							return std::unexpected<std::string>(type_args_result.error());
						}

						std::vector<std::shared_ptr<MidoriType>> type_args = std::move(type_args_result.value());

						// Get generic parameters from the base type
						std::vector<std::string> generic_params;
						if (base_type->IsType<MidoriType::StructType>())
						{
							generic_params = base_type->GetType<MidoriType::StructType>().m_generic_params;
						}
						else if (base_type->IsType<MidoriType::UnionType>())
						{
							generic_params = base_type->GetType<MidoriType::UnionType>().m_generic_params;
						}

						// Check argument count matches parameter count
						if (type_args.size() != generic_params.size())
						{
							return std::unexpected<std::string>(GenerateParserError(
								"Type argument count mismatch: expected " + std::to_string(generic_params.size()) +
								", got " + std::to_string(type_args.size()), type_name));
						}

						// Build substitution map
						std::unordered_map<std::string, std::shared_ptr<MidoriType>> substitutions;
						for (size_t i = 0; i < generic_params.size(); ++i)
						{
							substitutions[generic_params[i]] = type_args[i];
						}

						// Substitute generic parameters with concrete types
						return MidoriType::SubstituteTypeParams(base_type, substitutions);
					}

					return base_type;
				}
			);
	}
	else
	{
		return std::unexpected<std::string>(GenerateParserError("Expected type token.", Peek(0)));
	}
}

MidoriResult::StatementResult Parser::ParseDeclaration()
{
	if (Match(Token::Name::DEF))
	{
		return ParseDefineStatement();
	}
	else if (Match(Token::Name::DEFUN))
	{
		return ParseDefineFunctionStatement();
	}
	else if (Match(Token::Name::STRUCT))
	{
		return ParseStructDeclaration();
	}
	else if (Match(Token::Name::UNION))
	{
		return ParseUnionDeclaration();
	}
	else if (Match(Token::Name::CLASS))
	{
		return ParseClassDeclaration();
	}
	else if (Match(Token::Name::INSTANCE))
	{
		return ParseInstanceDeclaration();
	}
	else if (Match(Token::Name::FOREIGN))
	{
		return ParseForeignStatement();
	}
	else if (Match(Token::Name::TYPE))
	{
		return ParseTypeAliasDeclaration();
	}
	else
	{
		return ParseStatement();
	}
}

MidoriResult::ParserResult Parser::Parse()
{
	MidoriProgramTree programTree;
	std::string errors;

	while (!IsAtEnd())
	{
		MidoriResult::StatementResult result = ParseDeclaration();
		if (result.has_value())
		{
			programTree.emplace_back(std::move(result.value()));
		}
		else
		{
			errors.append(result.error()).push_back('\n');
			break;
		}
	}

	return errors.empty()
		? MidoriResult::ParserResult(std::move(programTree))
		: std::unexpected<std::string>(std::move(errors));
}

MidoriResult::TokenResult Parser::MatchNameResolution()
{
	Token resolved_name = Previous();
	std::string& resolved_name_str = resolved_name.m_lexeme;

	while (Match(Token::Name::DOUBLE_COLON))
	{
		// We found the separator, now we must have an identifier
		if (!Match(Token::Name::IDENTIFIER_LITERAL))
		{
			return std::unexpected<std::string>(GenerateParserError(std::format("Expected identifier after '{}'.", NameSeparator), Previous()));
		}

		resolved_name_str.append(NameSeparator).append(Previous().m_lexeme);
	}

	return resolved_name;
}

MidoriResult::TokenListResult Parser::ParseGenericParameters(std::vector<std::shared_ptr<MidoriType>>* out_types)
{
	return ParseDelimitedZeroOrMoreLimited<Token>
		(
			[this, out_types]() -> MidoriResult::TokenResult
			{
				return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected generic parameter name.")
					.and_then
					(
						[this, out_types](Token&& param_name) -> MidoriResult::TokenResult
						{
							constexpr bool is_variable = false;
							return DefineName(param_name, is_variable)
								.and_then
								(
									[this, out_types](Token&& param_name) -> MidoriResult::TokenResult
									{
										std::shared_ptr<MidoriType> param_type = MidoriType::MakeGenericType(param_name.m_lexeme);
										m_scopes.back().m_defined_types[param_name.m_lexeme] = param_type;

										// Keep the type alive if requested
										if (out_types != nullptr)
										{
											out_types->push_back(param_type);
										}

										return param_name;
									}
								);
						}
					);
			},
			[this]() { return Consume(Token::Name::COMMA, "Expected ',' between generic parameters."); },
			[this]() { return Consume(Token::Name::RIGHT_ANGLE, "Expected '>' after generic parameters."); }
		);
}

MidoriResult::FunctionParamsResult Parser::ParseFunctionParameters()
{
	return ParseDelimitedZeroOrMoreLimited<std::pair<Token, std::shared_ptr<MidoriType>>>
		(
			[this]() -> MidoriResult::FunctionParamResult
			{
				return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected parameter name.")
					.and_then
					(
						[this](Token&& param_name) -> MidoriResult::FunctionParamResult
						{
							return DefineName(param_name, true)
								.and_then
								(
									[this](Token&& param_name) -> MidoriResult::FunctionParamResult
									{
										return Consume(Token::Name::SINGLE_COLON, "Expected ':' after parameter name.")
											.and_then
											(
												[&param_name, this](Token&&) -> MidoriResult::FunctionParamResult
												{
													return ParseType()
														.and_then
														(
															[&param_name, this](std::shared_ptr<MidoriType>&& type) -> MidoriResult::FunctionParamResult
															{
																RegisterOrUpdateLocalVariable(param_name.m_lexeme);
																return std::make_pair(std::move(param_name), std::move(type));
															}
														);
												}
											);
									}
								);
						}
					);
			},
			[this]() { return Consume(Token::Name::COMMA, "Expected ',' after function parameter."); },
			[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after function parameters."); }
		);
}

void Parser::Synchronize()
{
	if (IsAtEnd())
	{
		return;
	}
	else
	{
		switch (Peek(0).m_token_name)
		{
			case Token::Name::DEF:
			case Token::Name::STRUCT:
			case Token::Name::UNION:
				return;
			default:
				Advance();
				Synchronize();
		}
	}
}

Parser::VariableContext::VariableContext(int relative_index, int absolute_index, int function_depth)
	: m_relative_index(relative_index),
	m_absolute_index(absolute_index),
	m_function_depth(function_depth)
{
}

const Parser::TypeclassMethodMap& Parser::GetTypeclassMethods() const
{
	return m_class_methods;
}

CompiledModule::TypeclassMetadataMap Parser::GetTypeclassMetadata() const
{
	CompiledModule::TypeclassMetadataMap result;
	for (const auto& [tc_name, methods] : m_class_methods)
	{
		CompiledModule::TypeclassMetadata metadata;
		metadata.m_method_names = methods;
		if (m_typeclass_type_params.contains(tc_name))
		{
			metadata.m_type_param_names = m_typeclass_type_params.at(tc_name);
		}
		if (m_class_instances.contains(tc_name))
		{
			metadata.m_instance_methods = m_class_instances.at(tc_name);
		}
		if (m_typeclass_method_types.contains(tc_name))
		{
			metadata.m_method_types = m_typeclass_method_types.at(tc_name);
		}
		result[tc_name] = std::move(metadata);
	}
	return result;
}
