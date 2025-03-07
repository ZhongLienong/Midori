#pragma once

#include <expected>
#include <stack>
#include <unordered_map>

#include "Common/Error/Error.h"
#include "Common/Result/Result.h"
#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"

class Parser
{
private:
	struct VariableContext
	{
		std::optional<int> m_relative_index = std::nullopt;
		std::optional<int> m_absolute_index = std::nullopt;
		std::optional<int> m_function_depth = std::nullopt;

		VariableContext() = default;

		VariableContext(int relative_index, int absolute_index, int function_depth);
	};

	struct Scope
	{
		using VariableTable = std::unordered_map<std::string, VariableContext>;
		using StructConstructorTable = std::unordered_map<std::string, std::weak_ptr<MidoriType>>;
		using UnionConstructorTable = std::unordered_map<std::string, std::weak_ptr<MidoriType>>;
		using DefinedTypeTable = std::unordered_map<std::string, std::weak_ptr<MidoriType>>;
		using DefinedNames = std::unordered_set<std::string>;

		VariableTable m_variables;
		StructConstructorTable m_struct_constructors;
		UnionConstructorTable m_union_constructors;
		DefinedTypeTable m_defined_types;
		DefinedNames m_defined_names;
	};

	using DependencyGraph = std::unordered_map<std::string, std::vector<std::string>>;
	using Scopes = std::vector<Scope>;
	
	inline static DependencyGraph s_dependency_graph{};
	TokenStream m_tokens;
	std::string m_file_name;
	Scopes m_scopes{ Scope() };
	std::stack<int> m_local_count_before_loop;
	std::vector<std::string> m_namespaces;
	int m_function_depth = 0;
	int m_current_token_index = 0;
	int m_total_locals_in_curr_scope = 0;
	int m_total_variables = 0;

public:
	Parser(TokenStream&& tokens, const std::string& file_name);

	MidoriResult::ParserResult Parse();

private:
	
	template <typename... T>
		requires (std::is_same_v<T, Token::Name> && ...)
	bool Match(T... tokens)
	{
		if ((... || Check(tokens, 0)))
		{
			Advance();
			return true;
		}
		return false;
	}
	
	template <typename... T>
		requires (std::is_same_v<T, Token::Name> && ...)
	MidoriResult::ExpressionResult ParseBinary(MidoriResult::ExpressionResult(Parser::* operand)(), T... tokens)
	{
		return (this->*operand)()
			.and_then
			(
				[operand, ...tokens = std::move(tokens), this](std::unique_ptr<MidoriExpression>&& lower_expr) ->MidoriResult::ExpressionResult
				{
					while (Match(tokens...))
					{
						Token& op = Previous();
						MidoriResult::ExpressionResult right = (this->*operand)();
						if (!right.has_value())
						{
							return std::unexpected<std::string>(std::move(right.error()));
						}

						lower_expr = std::make_unique<MidoriExpression>(MidoriExpression::Binary(std::move(op), std::move(lower_expr), std::move(right.value())));
					}

					return lower_expr;
				}
			);
	}

	template<typename OutputType, typename ParseFunc>
	std::expected<OutputType, std::string> TryParser(ParseFunc&& func)
	{
		int prev_index = m_current_token_index;
		return func()
			.or_else
			(
				[prev_index, this](std::string&& error) -> std::expected<OutputType, std::string> 
				{
					m_current_token_index = prev_index;
					return std::unexpected(error);
				}
			);
	}

	template<typename OutputType, typename ParseFunc, typename Delim, typename EndCond>
	std::expected<std::vector<OutputType>, std::string> ParseDelimitedZeroOrMore(ParseFunc&& func, Delim&& delim, EndCond&& end_cond, std::vector<OutputType>&& acc)
	{
		return TryParser<OutputType>(std::forward<ParseFunc>(func))
			.and_then
			(
				[&func, &end_cond, &delim, &acc, this](OutputType&& elem)
				{
					acc.emplace_back(std::move(elem));
					return delim()
						.and_then
						(
							[&func, &end_cond, &delim, &acc, this](Token&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								return ParseDelimitedZeroOrMore(func, delim, end_cond, std::move(acc));
							}
						)
						.or_else(
							[&end_cond, &acc](std::string&& delim_error) -> std::expected<std::vector<OutputType>, std::string>
							{
								return end_cond()
									.and_then
									(
										[&acc](Token&&) -> std::expected<std::vector<OutputType>, std::string>
										{
											return std::move(acc);
										}
									)
									.or_else
									(
										[&delim_error](std::string&&) -> std::expected<std::vector<OutputType>, std::string>
										{
											return std::unexpected<std::string>(std::move(delim_error));
										}
									);
							}
						);
				}
			)
			.or_else
			(
				[&acc, &end_cond, this](std::string&& try_parser_error)
				{
					return end_cond()
						.and_then
						(
							[&acc](Token&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								return std::move(acc);
							}
						)
						.or_else
						(
							[&try_parser_error, this](std::string&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								Advance();
								Synchronize();
								return std::unexpected(std::move(try_parser_error));
							}
						);
				}
			);
	}

	template<typename OutputType, typename ParseFunc, typename EndCond>
	std::expected<std::vector<OutputType>, std::string> ParseZeroOrMore(ParseFunc&& func, EndCond&& end_cond, std::vector<OutputType>&& acc)
	{
		return TryParser<OutputType>(std::forward<ParseFunc>(func))
			.and_then
			(
				[&acc, &func, &end_cond, this](OutputType&& elem)
				{
					acc.emplace_back(std::move(elem));
					return ParseZeroOrMore(func, end_cond, std::move(acc))
						.or_else
						(
							[&end_cond, &acc](std::string&& error) -> std::expected<std::vector<OutputType>, std::string>
							{
								return end_cond()
									.and_then
									(
										[&acc](Token&&) -> std::expected<std::vector<OutputType>, std::string>
										{
											return std::move(acc);
										}
									)
									.or_else
									(
										[&error](std::string&&) ->std::expected<std::vector<OutputType>, std::string>
										{
											return std::unexpected<std::string>(std::move(error));
										}
									);
							}
						);
				}
			)
			.or_else
			(
				[&acc, &end_cond, this](std::string&& try_parser_error)
				{
					return end_cond()
						.and_then
						(
							[&acc](Token&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								return std::move(acc);
							}
						)
						.or_else
						(
							[&try_parser_error, this](std::string&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								Advance();
								Synchronize();
								return std::unexpected(std::move(try_parser_error));
							}
						);
				}
			);
	}

	bool IsGlobalName(const std::vector<Scope>::const_reverse_iterator& found_scope_it) const;

	bool IsLocalName(const Scope::VariableTable::const_iterator& found_scope_it) const;
	
	void Synchronize();

	std::string GenerateParserError(std::string&& message, const Token& token);

	bool IsAtEnd();

	bool Check(Token::Name type, int offset);

	bool IsAtGlobalScope() const;

	std::vector<Scope>::const_reverse_iterator FindVariableScope(std::string& name);

	std::vector<Scope>::const_reverse_iterator FindTypeScope(std::string& name);

	Token& Peek(int offset);

	Token& Previous();

	Token& Advance();

	MidoriResult::TokenResult Consume(Token::Name type, std::string_view message);

	MidoriResult::TokenResult MatchNameResolution();

	void BeginScope();

	int EndScope();

	std::string Mangle(std::string_view name);

	MidoriResult::TokenResult DefineName(Token& name, bool is_variable);

	std::optional<int> RegisterOrUpdateLocalVariable(const std::string& name);

	bool HasReturnStatement(const MidoriStatement& stmt);

	MidoriResult::TypeResult ParseType(bool is_foreign = false);

	MidoriResult::ExpressionResult ParseExpression();

	MidoriResult::ExpressionResult ParseFactor();

	MidoriResult::ExpressionResult ParseShift();

	MidoriResult::ExpressionResult ParseTerm();

	MidoriResult::ExpressionResult ParseComparison();

	MidoriResult::ExpressionResult ParseEquality();

	MidoriResult::ExpressionResult ParseBitwiseAnd();

	MidoriResult::ExpressionResult ParseBitwiseXor();

	MidoriResult::ExpressionResult ParseBitwiseOr();

	MidoriResult::ExpressionResult ParseBind();

	MidoriResult::ExpressionResult ParseUnaryLogicalBitwise();

	MidoriResult::ExpressionResult ParseUnaryArithmetic();

	MidoriResult::ExpressionResult ParseArrayAccessHelper(std::unique_ptr<MidoriExpression>&& arr_var);

	MidoriResult::ExpressionResult ParseArrayAccess();

	MidoriResult::ExpressionResult ParseTernary();

	MidoriResult::ExpressionResult ParseCall();

	MidoriResult::ExpressionResult ParseAs();

	MidoriResult::ExpressionResult ParseConstruct();

	MidoriResult::ExpressionResult FinishCall(std::unique_ptr<MidoriExpression>&& callee);

	MidoriResult::ExpressionResult ParsePrimary();

	MidoriResult::ExpressionResult ParseLogicalAnd();

	MidoriResult::ExpressionResult ParseLogicalOr();

	MidoriResult::StatementResult ParseDeclarationCommon(bool allow_stmt);

	MidoriResult::StatementResult ParseLocalDeclaration();

	MidoriResult::StatementResult ParseGlobalDeclaration();

	MidoriResult::StatementResult ParseBlockStatement();

	MidoriResult::StatementResult ParseDefineStatement();

	MidoriResult::StatementResult ParseStructDeclaration();

	MidoriResult::StatementResult ParseUnionDeclaration();

	MidoriResult::StatementResult ParseIfStatement();

	MidoriResult::StatementResult ParseLoopStatement();

	MidoriResult::StatementResult ParseBreakStatement();

	MidoriResult::StatementResult ParseContinueStatement();

	MidoriResult::StatementResult ParseSimpleStatement();

	MidoriResult::StatementResult ParseReturnStatement();

	MidoriResult::StatementResult ParseForeignStatement();

	MidoriResult::StatementResult ParseSwitchStatement();

	MidoriResult::StatementResult ParseNamespaceStatement();

	MidoriResult::CaseResult ParseCaseStatement(std::unordered_set<std::string>& visited_members, Token&& keyword);

	MidoriResult::CaseResult ParseDefaultStatement(bool& default_visited, Token&& keyword);

	MidoriResult::StatementResult ParseStatement();

	MidoriResult::TokenResult HandleDirective();

	bool HasCircularDependency() const; 
};
