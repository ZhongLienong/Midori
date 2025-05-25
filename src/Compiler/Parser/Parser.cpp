#include <algorithm>
#include <filesystem>
#include <fstream>
#include <queue>
#include <sstream>

#include "Compiler/Lexer/Lexer.h"
#include "Parser.h"

namespace
{
	constexpr std::string_view NameSeparator = "::";
}

Parser::Parser(TokenStream&& tokens, const std::string& file_name) : m_tokens(std::move(tokens))
{
	std::string absolute_file_path = std::filesystem::absolute(file_name).string();
	m_file_name = std::move(absolute_file_path);
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
	return MidoriError::GenerateParserError(std::move(message), token);
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
	for (auto end_idx : std::views::iota(0u, m_namespaces.size()))
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
		return std::unexpected<std::string>(MidoriError::GenerateParserError(message, Peek(0)));
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
		std::string_view top_namespace = name.substr(0, sep_idx);
		std::vector<std::string>::const_iterator find_result = std::find(m_namespaces.cbegin(), m_namespaces.cend(), top_namespace);

		if (find_result != m_namespaces.cend()) 
		{
			// Found the namespace in our stack - resolve to absolute path
			std::string mangled_name;

			// Prepend namespaces up to (but not including) the found one
			for (std::vector<std::string>::const_iterator it = m_namespaces.cbegin(); it != find_result; ++it)
			{
				mangled_name.append(*it).append(NameSeparator);
			}

			// Append the original name (which already includes the found namespace)
			mangled_name.append(name);
			return mangled_name;
		}
		else 
		{
			// Namespace not in our stack - return as-is (already absolute)
			return std::string(name);
		}
	}
	else 
	{
		// No separator - prepend all current namespaces
		std::string mangled_name;
		for (const std::string& namespace_name : m_namespaces) 
		{
			mangled_name.append(namespace_name).append(NameSeparator);
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

MidoriResult::ExpressionResult Parser::ParseTerm()
{
	return ParseBinary(&Parser::ParseFactor, Token::Name::SINGLE_PLUS, Token::Name::DOUBLE_PLUS, Token::Name::SINGLE_MINUS);
}

MidoriResult::ExpressionResult Parser::ParseComparison()
{
	return ParseBinary(&Parser::ParseShift, Token::Name::LEFT_ANGLE, Token::Name::LESS_EQUAL, Token::Name::RIGHT_ANGLE, Token::Name::GREATER_EQUAL);
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
	return ParseLogicalOr()
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
										auto find_result = found_scope_it->m_variables.find(variable_expr.m_name.m_lexeme);
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

				return left_expr;
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseUnaryLogicalBitwise()
{
	if (Match(Token::Name::BANG, Token::Name::TILDE))
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

MidoriResult::ExpressionResult Parser::ParseArrayAccess()
{
	return ParsePrimary()
		.and_then
		(
			[this](std::unique_ptr<MidoriExpression>&& arr_var) -> MidoriResult::ExpressionResult
			{
				return Check(Token::Name::LEFT_BRACKET, 0)
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
			else if (Match(Token::Name::DOT))
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
			MidoriResult::TokenResult data_name_token = MatchNameResolution();
			Token& data_name_token_value = data_name_token.value();

			data_name_token_value.m_lexeme = Mangle(data_name_token_value.m_lexeme);

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
				return std::unexpected<std::string>(GenerateParserError("Undefined struct.", data_name_token.value()));
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
								[&data_name_token_value, this](std::string&&) ->MidoriResult::ExpressionResult
								{
									return std::unexpected<std::string>(GenerateParserError("Failed to parse constructor arguments.", data_name_token_value));
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
		return Match(Token::Name::RIGHT_PAREN)
			? std::make_unique<MidoriExpression>(MidoriExpression::UnitLiteral(Previous()))
			: ParseExpression()
			.and_then
			(
				[this](std::unique_ptr<MidoriExpression>&& expr_in) -> MidoriResult::ExpressionResult
				{
					return Consume(Token::Name::RIGHT_PAREN, "Expected right parentheses.")
						.and_then
						(
							[&expr_in](Token&&) -> MidoriResult::ExpressionResult
							{
								return std::make_unique<MidoriExpression>(MidoriExpression::Group(std::move(expr_in)));
							}
						);
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
					std::vector<Scope>::const_reverse_iterator found_scope_it = FindVariableScope(variable.m_lexeme);

					if (found_scope_it != m_scopes.rend())
					{
						Scope::VariableTable::const_iterator find_result = found_scope_it->m_variables.find(variable.m_lexeme);

						// global
						if (IsGlobalName(found_scope_it))
						{
							return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(variable, MidoriExpression::NameContext::Global()));
						}
						// local
						else if (IsLocalName(find_result))
						{
							return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(variable, MidoriExpression::NameContext::Local(find_result->second.m_relative_index.value())));
						}
						// cell
						else
						{
							return std::make_unique<MidoriExpression>(MidoriExpression::BoundedName(variable, MidoriExpression::NameContext::Cell(find_result->second.m_absolute_index.value())));
						}
					}

					return std::unexpected<std::string>(GenerateParserError("Undefined name.", variable));
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
		return std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(Previous()));
	}
	else if (Match(Token::Name::TEXT_LITERAL))
	{
		return std::make_unique<MidoriExpression>(MidoriExpression::TextLiteral(Previous()));
	}
	else if (Match(Token::Name::LEFT_BRACKET))
	{
		Token& op = Previous();
		return ParseDelimitedZeroOrMoreLimited<std::unique_ptr<MidoriExpression>>
		(
			[this]() { return ParseExpression(); },
			[this]() { return Consume(Token::Name::COMMA, "Expected ',' after expression."); },
			[this]() { return Consume(Token::Name::RIGHT_BRACKET, "Expected ']' for array expression."); }
		)
			.and_then
			(
				[&op](std::vector<std::unique_ptr<MidoriExpression>>&& expressions) ->MidoriResult::ExpressionResult
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::Array(op, std::move(expressions)));
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

MidoriResult::ExpressionResult Parser::ParseBlockExpression()
{
	auto build_block = [this](std::vector<std::unique_ptr<MidoriStatement>>&& stmts, std::unique_ptr<MidoriExpression>&& final_expr = nullptr) -> MidoriResult::ExpressionResult
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
								return build_block(std::move(stmts));
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

MidoriResult::StatementResult Parser::ParseDefineStatement()
{
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
							auto def_aux_func = [&define_name, this](std::optional<std::shared_ptr<MidoriType>> type_annotation)
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
							return Consume(Token::Name::LEFT_BRACE, "Expected '{' before struct body.")
								.and_then
								(
									[&struct_name, this](Token&&) ->MidoriResult::StatementResult
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
																						return type->IsType<MidoriType::StructType>() && (type->GetType<MidoriType::StructType>().m_name == struct_name.m_lexeme)
																							? std::unexpected<std::string>(GenerateParserError("Recursive struct is not allowed.", identifier))
																							: std::expected<std::tuple<std::shared_ptr<MidoriType>, std::string>, std::string>(std::make_tuple(std::move(type), identifier.m_lexeme));
																					}
																				);
																		}
																	);
															}
														);
												},
												[this]() { return Consume(Token::Name::COMMA, "Expected ',' struct member.");  },
												[this]() { return Consume(Token::Name::RIGHT_BRACE, "Expected '}' struct members."); }
											)
											.and_then
											(
												[&struct_name, this](std::vector<std::tuple<std::shared_ptr<MidoriType>, std::string>>&& tuples) ->MidoriResult::StatementResult
												{
													return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after struct body.")
														.and_then
														(
															[&tuples, &struct_name, this](Token&&) ->MidoriResult::StatementResult
															{
																std::vector<std::shared_ptr<MidoriType>> member_types;
																std::vector<std::string> member_names;

																std::ranges::transform(tuples, std::back_inserter(member_types), [](auto&& tuple) { return std::move(std::get<0>(tuple)); });
																std::ranges::transform(tuples, std::back_inserter(member_names), [](auto&& tuple) { return std::move(std::get<1>(tuple)); });

																std::shared_ptr<MidoriType> struct_type = MidoriType::MakeStructType(struct_name.m_lexeme, std::move(member_types), std::move(member_names));
																m_scopes.back().m_struct_constructors[struct_name.m_lexeme] = struct_type;
																m_scopes.back().m_defined_types[struct_name.m_lexeme] = struct_type;

																return std::make_unique<MidoriStatement>(MidoriStatement::Struct(std::move(struct_name), std::move(struct_type)));
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
							int tag = 0;
							std::shared_ptr<MidoriType> union_type = MidoriType::MakeUnionType(union_name.m_lexeme);
							MidoriType::UnionType& union_type_ref = union_type->GetType<MidoriType::UnionType>();
							m_scopes.back().m_defined_types[union_name.m_lexeme] = union_type;
							m_namespaces.emplace_back(union_name_before_mangle);

							return Consume(Token::Name::SINGLE_EQUAL, "Expected '=' before union body.")
								.and_then
								(
									[&union_type_ref, &union_type, &union_name, &tag, this](Token&&) mutable -> MidoriResult::StatementResult
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
																			tag += 1;
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
																							return std::make_tuple(member_name.m_lexeme, std::move(types), tag);
																						}
																					);
																			}
																			else
																			{
																				return std::make_tuple(member_name.m_lexeme, std::vector<std::shared_ptr<MidoriType>>(), tag);
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
												[&union_type_ref, &union_type, &union_name, this](std::vector<std::tuple<std::string, std::vector<std::shared_ptr<MidoriType>>, int>>&& result)
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
														[union_type, this](const auto& member_info_entry)
														{
															m_scopes.back().m_union_constructors[member_info_entry.first] = union_type;
														}
													);

													return Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after union body.")
														.and_then
														(
															[&union_name, &union_type, this](Token&&) -> MidoriResult::StatementResult
															{
																m_namespaces.pop_back();
																return std::make_unique<MidoriStatement>(MidoriStatement::Union(std::move(union_name), std::move(union_type)));
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
								return ParseZeroOrMoreUnlimited<std::unique_ptr<MidoriExpression>>
									(
										[&visited_names, &default_visited, this]() -> MidoriResult::ExpressionResult
										{
											if (Match(Token::Name::CASE))
											{
												Token& case_keyword = Previous();
												return ParseCaseExpression(visited_names, case_keyword);
											}
											else if (Match(Token::Name::DEFAULT))
											{
												Token& default_keyword = Previous();
												return ParseDefaultExpression(default_visited, default_keyword);
											}
											else
											{
												return std::unexpected<std::string>(GenerateParserError("Expected 'case' or 'default'.", Previous()));
											}
										}
									)
									.and_then
									(
										[&match_keyword, &expr, this](std::vector<std::unique_ptr<MidoriExpression>>&& cases)-> MidoriResult::ExpressionResult
										{
											return cases.empty()
												? std::unexpected<std::string>(GenerateParserError("Expected at least one case.", match_keyword))
												: MidoriResult::ExpressionResult(std::make_unique<MidoriExpression>(MidoriExpression::Match(match_keyword, std::move(expr), std::move(cases))));
										}
									);
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

				return ParseDelimitedZeroOrMoreLimited<Token>
					(
						[this]()
						{
							return Consume(Token::Name::IDENTIFIER_LITERAL, "Expected parameter name.")
								.and_then
								(
									[this](Token&& param_name)
									{
										return DefineName(param_name, true)
											.and_then
											(
												[this](Token&& param_name) -> std::expected<Token, std::string>
												{
													RegisterOrUpdateLocalVariable(param_name.m_lexeme);
													return param_name;
												}
											);
									}
								);
						},
						[this]() { return Consume(Token::Name::COMMA, "Expected ',' after function parameter."); },
						[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after function parameters."); }
					)
					.and_then
					(
						[&keyword, prev_total_locals, this](std::vector<Token>&& params) ->MidoriResult::ExpressionResult
						{
							return Consume(Token::Name::THIN_ARROW, "Expected '->' before function body.")
								.and_then
								(
									[&params, &keyword, prev_total_locals, this](Token&&) ->MidoriResult::ExpressionResult
									{
										return ParseExpression()
											.and_then
											(
												[&params, &keyword, prev_total_locals, this](std::unique_ptr<MidoriExpression>&& return_value) ->MidoriResult::ExpressionResult
												{
													Token& right_brace = Previous();
													int block_local_count = EndScope();
													m_total_locals_in_curr_scope = prev_total_locals;
													m_function_depth -= 1;

													return std::make_unique<MidoriExpression>(MidoriExpression::Function(keyword, std::move(params), std::move(return_value), m_total_variables));
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::StatementResult Parser::ParseNamespaceStatement()
{
	return !IsAtGlobalScope()
		? std::unexpected<std::string>(GenerateParserError("Namespaces can only be declared at global scope.", Previous()))
		: Consume(Token::Name::IDENTIFIER_LITERAL, "Expected namespace name.")
		.and_then
		(
			[this](Token&& name) ->MidoriResult::StatementResult
			{
				return Consume(Token::Name::LEFT_BRACE, "Expected '{' before namespace body.")
					.and_then
					(
						[&name, this](Token&&) ->MidoriResult::StatementResult
						{
							std::string namespace_name = name.m_lexeme;

							name.m_lexeme = Mangle(name.m_lexeme);
							m_namespaces.emplace_back(std::move(namespace_name));

							return ParseZeroOrMoreLimited<std::unique_ptr<MidoriStatement>>
							(
								[this]() { return ParseDeclaration(); },
								[this]() { return Consume(Token::Name::RIGHT_BRACE, "Expected '}' after namespace body."); }
							)
								.and_then
								(
									[&name, this](std::vector<std::unique_ptr<MidoriStatement>>&& decls) -> MidoriResult::StatementResult
									{
										m_namespaces.pop_back();
										return std::make_unique<MidoriStatement>(MidoriStatement::Namespace(name, std::move(decls)));
									}
								)
								.or_else
								(
									[this](std::string&& error) ->MidoriResult::StatementResult
									{
										m_namespaces.pop_back();
										return std::unexpected<std::string>(std::move(error));
									}
								);
						}
					);
			}
		);
}

MidoriResult::ExpressionResult Parser::ParseCaseExpression(std::unordered_set<std::string>& visited_members, Token& keyword)
{
	auto handle_body = [this](Token& keyword, std::vector<std::string>&& binding_names, Token&& member_name) -> MidoriResult::ExpressionResult
		{
			return Consume(Token::Name::THIN_ARROW, "Expected '->' after case.")
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
		return Consume(Token::Name::THIN_ARROW, "Expected '->' after default.")
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
	else if (Match(Token::Name::LEFT_PAREN))
	{
		auto func_type_aux_func = [is_foreign, this](std::vector<std::shared_ptr<MidoriType>>&& types) ->MidoriResult::TypeResult
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
		if (!Match(Token::Name::RIGHT_PAREN))
		{
			return ParseDelimitedZeroOrMoreLimited<std::shared_ptr<MidoriType>>
				(
					[this]() { return ParseType(); },
					[this]() { return Consume(Token::Name::COMMA, "Expected ',' after argument type");  },
					[this]() { return Consume(Token::Name::RIGHT_PAREN, "Expected ')' after argument types.");  }
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
	else if (Match(Token::Name::IDENTIFIER_LITERAL))
	{
		return MatchNameResolution()
			.and_then
			(
				[this](Token&& type_name) ->MidoriResult::TypeResult
				{
					std::string mangled_name = Mangle(type_name.m_lexeme);
					std::vector<Scope>::const_reverse_iterator found_scope_it = FindTypeScope(type_name.m_lexeme);

					if (found_scope_it == m_scopes.crend())
					{
						return std::unexpected<std::string>(GenerateParserError("Undefined struct or union.", type_name));
					}

					return found_scope_it->m_defined_types.at(type_name.m_lexeme).lock();
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
	else if (Match(Token::Name::STRUCT))
	{
		return ParseStructDeclaration();
	}
	else if (Match(Token::Name::UNION))
	{
		return ParseUnionDeclaration();
	}
	else if (Match(Token::Name::FOREIGN))
	{
		return ParseForeignStatement();
	}
	else if (Match(Token::Name::NAMESPACE))
	{
		return ParseNamespaceStatement();
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
	bool should_match_double_colon = true;

	while (Match(Token::Name::DOUBLE_COLON)) 
	{
		// We found the separator, now we must have an identifier
		if (!Match(Token::Name::IDENTIFIER_LITERAL)) 
		{
			return std::unexpected<std::string>(GenerateParserError(std::format("Expected identifier after '{}'.", NameSeparator), Previous()));
		}

		resolved_name_str.append(NameSeparator).append(Previous().m_lexeme);
	}

	resolved_name.m_lexeme = std::move(resolved_name_str);
	return resolved_name;
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
