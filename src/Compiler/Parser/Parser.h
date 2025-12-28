#pragma once

#include <expected>
#include <stack>
#include <unordered_map>

#include "Common/Error/Error.h"
#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"
#include "Compiler/Module/CompiledModule.h"
#include "Compiler/Module/Module.h"
#include "Compiler/Result/Result.h"

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
		using StructConstructorTable = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;
		using UnionConstructorTable = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;
		using DefinedTypeTable = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;
		using DefinedNames = std::unordered_set<std::string>;

		VariableTable m_variables;
		StructConstructorTable m_struct_constructors;
		UnionConstructorTable m_union_constructors;
		DefinedTypeTable m_defined_types;
		DefinedNames m_defined_names;
	};

	using Scopes = std::vector<Scope>;

	using TypeclassMethodMap = std::unordered_map<std::string, std::unordered_set<std::string>>;
	using TypeclassInstanceMap = std::unordered_map<std::string, std::vector<std::string>>;
	using TypeEnvironment = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;
	using TypeclassMethodTypeMap = std::unordered_map<std::string, std::unordered_map<std::string, std::shared_ptr<MidoriType>>>;

	std::unordered_map<std::string, CompiledModule::SymbolTable> m_imported_symbols;
	std::unordered_map<std::string, TypeEnvironment> m_imported_type_signatures;
	TypeclassMethodMap m_class_methods;
	TypeclassInstanceMap m_typeclass_type_params;
	TypeclassInstanceMap m_class_instances;
	TypeclassMethodTypeMap m_typeclass_method_types;
	TokenStream m_tokens;
	std::string m_file_name;
	Scopes m_scopes{ Scope() };
	std::stack<int> m_local_count_before_loop;
	std::vector<UseImport> m_current_use_imports;
	std::vector<std::string> m_namespaces;
	std::vector<MidoriType::ClassConstraint> m_active_constraints;  // Constraints active in current parsing context
	const ModuleDeclaration* m_current_module;
	const std::unordered_map<std::string, ModuleDeclaration>* m_module_declarations;
	const std::unordered_map<std::string, std::vector<UseImport>>* m_use_imports;
	const std::vector<std::string>& m_source_lines;
	int m_function_depth = 0;
	int m_current_token_index = 0;
	int m_total_locals_in_curr_scope = 0;
	int m_total_variables = 0;

public:
	Parser(TokenStream&& tokens, std::string_view file_name, const std::vector<std::string>& source_lines, const std::unordered_map<std::string, CompiledModule::SymbolTable>& imports, const std::unordered_map<std::string, TypeEnvironment>& imported_type_signatures, const std::vector<UseImport>& use_imports, const ModuleDeclaration* module_decl, const CompiledModule::TypeclassMetadataMap& imported_typeclass_metadata = {});

	MidoriResult::ParserResult Parse();

	const TypeclassMethodMap& GetTypeclassMethods() const;

	CompiledModule::TypeclassMetadataMap GetTypeclassMetadata() const;

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

						lower_expr = std::make_unique<MidoriExpression>(MidoriExpression::Binary(op, std::move(lower_expr), std::move(right.value())));
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
	std::expected<std::vector<OutputType>, std::string> ParseDelimitedZeroOrMoreLimited(ParseFunc&& func, Delim&& delim, EndCond&& end_cond, std::vector<OutputType>&& acc = {})
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
								return ParseDelimitedZeroOrMoreLimited(std::forward<ParseFunc>(func), std::forward<Delim>(delim), std::forward<EndCond>(end_cond), std::move(acc));
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

	template<typename OutputType, typename ParseFunc, typename Delim>
	std::expected<std::vector<OutputType>, std::string> ParseDelimitedZeroOrMoreUnlimited(ParseFunc&& func, Delim&& delim, std::vector<OutputType>&& acc = {})
	{
		return TryParser<OutputType>(std::forward<ParseFunc>(func))
			.and_then
			(
				[&func, &delim, &acc, this](OutputType&& elem)
				{
					acc.emplace_back(std::move(elem));
					return delim()
						.and_then
						(
							[&func, &delim, &acc, this](Token&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								return ParseDelimitedZeroOrMoreUnlimited(std::forward<ParseFunc>(func), std::forward<Delim>(delim), std::move(acc));
							}
						)
						.or_else
						(
							[&acc](std::string&&) -> std::expected<std::vector<OutputType>, std::string>
							{
								return std::move(acc);
							}
						);
				}
			)
			.or_else
			(
				[&acc](std::string&&) -> std::expected<std::vector<OutputType>, std::string>
				{
					return std::move(acc);
				}
			);
	}

	template<typename OutputType, typename ParseFunc, typename EndCond>
	std::expected<std::vector<OutputType>, std::string> ParseZeroOrMoreLimited(ParseFunc&& func, EndCond&& end_cond, std::vector<OutputType>&& acc = {})
	{
		return TryParser<OutputType>(std::forward<ParseFunc>(func))
			.and_then
			(
				[&acc, &func, &end_cond, this](OutputType&& elem)
				{
					acc.emplace_back(std::move(elem));
					return ParseZeroOrMoreLimited(std::forward<ParseFunc>(func), std::forward<EndCond>(end_cond), std::move(acc))
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

	template<typename OutputType, typename ParseFunc>
	std::expected<std::vector<OutputType>, std::string> ParseZeroOrMoreUnlimited(ParseFunc&& func, std::vector<OutputType>&& acc = {})
	{
		return TryParser<OutputType>(std::forward<ParseFunc>(func))
			.and_then
			(
				[&func, &acc, this](OutputType&& elem) mutable
				{
					acc.emplace_back(std::move(elem));
					return ParseZeroOrMoreUnlimited<OutputType>(std::forward<ParseFunc>(func), std::move(acc));
				}
			)
			.or_else
			(
				[&acc, this](std::string&&) mutable -> std::expected<std::vector<OutputType>, std::string>
				{
					return std::move(acc);
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

	bool CanAccessSymbol(const std::string& symbol_name) const;

	bool IsInUseImports(const std::string& symbol_name, std::string& out_module_name) const;

	bool ResolveQualifiedSymbol(const std::string& module_name, const std::string& symbol_name) const;

	bool SharesNamespace(const std::string& namespace1, const std::string& namespace2) const;

	std::string ExtractSymbolName(const std::string& qualified_name) const;

	std::string ExtractQualifier(const std::string& qualified_name) const;

	MidoriResult::ExpressionResult ResolveQualifiedName(const Token& name_token, const std::string& mangled_name);

	Token& Peek(int offset);

	Token& Previous();

	Token& Advance();

	MidoriResult::TokenResult Consume(Token::Name type, std::string_view message);

	MidoriResult::TokenResult MatchNameResolution();

	MidoriResult::TokenListResult ParseGenericParameters(std::vector<std::shared_ptr<MidoriType>>* out_types = nullptr);

	MidoriResult::FunctionParamsResult ParseFunctionParameters();

	void BeginScope();

	int EndScope();

	std::string Mangle(std::string_view name);

	MidoriResult::TokenResult DefineName(Token& name, bool is_variable);

	std::optional<int> RegisterOrUpdateLocalVariable(const std::string& name);

	std::optional<int> DetectArrayComprehension();

	MidoriResult::TypeResult ParseType(bool is_foreign = false);

	MidoriResult::ExpressionResult ParseExpression();

	MidoriResult::ExpressionResult ParseFactor();

	MidoriResult::ExpressionResult ParseShift();

	MidoriResult::ExpressionResult ParseRange();

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

	MidoriResult::ExpressionResult ParseCall();

	MidoriResult::ExpressionResult ParseAs();

	MidoriResult::ExpressionResult ParseConstruct();

	MidoriResult::ExpressionResult FinishCall(std::unique_ptr<MidoriExpression>&& callee);

	MidoriResult::ExpressionResult ParsePrimary();

	MidoriResult::ExpressionResult ParseLogicalAnd();

	MidoriResult::ExpressionResult ParseLogicalOr();

	MidoriResult::ExpressionResult ParsePipe();

	MidoriResult::ExpressionResult ParseBlockExpression();

	MidoriResult::ExpressionResult ParseLoopExpression();

	MidoriResult::ExpressionResult ParseForExpression();

	MidoriResult::ExpressionResult ParseArrayComprehension(Token& bracket);

	MidoriResult::ExpressionResult ParseReturnExpression();

	MidoriResult::ExpressionResult ParseBreakExpression();

	MidoriResult::ExpressionResult ParseMatchExpression();

	MidoriResult::ExpressionResult ParseIfElseExpression();

	MidoriResult::ExpressionResult ParseFunctionExpression();

	MidoriResult::ExpressionResult ParseCaseExpression(std::unordered_set<std::string>& visited_members, Token& keyword);

	MidoriResult::ExpressionResult ParseDefaultExpression(bool& default_visited, Token& keyword);

	MidoriResult::StatementResult ParseDeclaration();

	MidoriResult::StatementResult ParseDefineStatement();

	MidoriResult::StatementResult ParseDefineFunctionStatement();

	MidoriResult::StatementResult ParseStructDeclaration();

	MidoriResult::StatementResult ParseUnionDeclaration();

	MidoriResult::StatementResult ParseClassDeclaration();

	MidoriResult::StatementResult ParseInstanceDeclaration();

	MidoriResult::StatementResult ParseTypeAliasDeclaration();

	MidoriResult::StatementResult ParseContinueStatement();

	MidoriResult::StatementResult ParseSimpleStatement();

	MidoriResult::StatementResult ParseForeignStatement();

	MidoriResult::StatementResult ParseStatement();
};
