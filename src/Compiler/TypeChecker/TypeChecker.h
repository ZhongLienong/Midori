#pragma once

#include <array>
#include <unordered_map>

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"

class TypeChecker
{
private:
	using TypeEnvironment = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;
	using TypeEnvironmentStack = std::vector<TypeEnvironment>;
	using TypeSubstitution = std::unordered_map<int, std::shared_ptr<MidoriType>>;
	using GenericFunctionNames = std::unordered_set<std::string>;
	using GenericStructNames = std::unordered_set<std::string>;
	using GenericUnionNames = std::unordered_set<std::string>;
	struct FresheningContext
	{
		std::unordered_map<std::string, std::shared_ptr<MidoriType>> generic_params;  // Maps generic param names to fresh type vars
		std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>> type_cache;  // Maps type pointers to their fresh versions (for cycle detection)
	};

	MidoriProgramTree m_program_tree;
	TypeEnvironmentStack m_name_type_table;
	TypeSubstitution m_type_substitution;
	GenericFunctionNames m_generic_functions;
	GenericStructNames m_generic_structs;
	GenericUnionNames m_generic_unions;
	int m_next_type_var_id;
	std::shared_ptr<MidoriType> m_expected_return_type;  // Expected return type for the current function
	const std::array<Token::Name, 5u> m_binary_arithmetic_operators{ Token::Name::SINGLE_PLUS, Token::Name::SINGLE_MINUS, Token::Name::STAR, Token::Name::SLASH, Token::Name::PERCENT };
	const std::array<Token::Name, 1u> m_binary_concatenation_operators{ Token::Name::DOUBLE_PLUS };
	const std::array<Token::Name, 4u> m_binary_partial_order_comparison_operators{ Token::Name::LEFT_ANGLE, Token::Name::LESS_EQUAL, Token::Name::RIGHT_ANGLE, Token::Name::GREATER_EQUAL };
	const std::array<Token::Name, 2u> m_binary_equality_operators{ Token::Name::DOUBLE_EQUAL, Token::Name::BANG_EQUAL };
	const std::array<Token::Name, 2u> m_binary_logical_operators{ Token::Name::DOUBLE_AMPERSAND, Token::Name::DOUBLE_BAR };
	const std::array<Token::Name, 5u> m_binary_bitwise_operators{ Token::Name::CARET, Token::Name::SINGLE_AMPERSAND, Token::Name::SINGLE_BAR, Token::Name::RIGHT_SHIFT, Token::Name::LEFT_SHIFT };

public:

	TypeChecker(MidoriProgramTree&& parser_result);

	MidoriResult::TypeCheckerResult TypeCheck();

private:

	void BeginScope();

	void EndScope();

	void UpdateConditionOperandType(MidoriExpression::ConditionOperandType& op_type, const std::unique_ptr<MidoriExpression>& expr);

	std::shared_ptr<MidoriType> FreshTypeVar();

	std::shared_ptr<MidoriType> Freshen(const std::shared_ptr<MidoriType>& type);

	std::shared_ptr<MidoriType> Freshen(const std::shared_ptr<MidoriType>& type, FresheningContext& context);

	std::shared_ptr<MidoriType> ApplySubstitution(const std::shared_ptr<MidoriType>& type);

	std::shared_ptr<MidoriType> ApplySubstitution(const std::shared_ptr<MidoriType>& type, std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>& cache);

	bool OccursCheck(int var_id, const std::shared_ptr<MidoriType>& type);

	MidoriResult::TypeResult Unify(const Token& token, std::shared_ptr<MidoriType>& left, std::shared_ptr<MidoriType>& right);

	MidoriResult::TypeResult operator()(MidoriStatement::Simple& simple);

	MidoriResult::TypeResult operator()(MidoriStatement::Define& def);

	MidoriResult::TypeResult operator()(MidoriStatement::DefineFunction& defun);

	MidoriResult::TypeResult operator()(MidoriStatement::Continue& continue_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Foreign& foreign_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Struct& struct_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Union& union_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Namespace& namespace_stmt);

	MidoriResult::TypeResult operator()(MidoriExpression::As& as);

	MidoriResult::TypeResult operator()(MidoriExpression::Binary& binary);

	MidoriResult::TypeResult operator()(MidoriExpression::Group& group);

	MidoriResult::TypeResult operator()(MidoriExpression::UnaryPrefix& unary);

	MidoriResult::TypeResult operator()(MidoriExpression::UnarySuffix& unary);

	MidoriResult::TypeResult operator()(MidoriExpression::Call& call);

	MidoriResult::TypeResult operator()(MidoriExpression::Get& get);

	MidoriResult::TypeResult operator()(MidoriExpression::Set& set);

	MidoriResult::TypeResult operator()(MidoriExpression::BoundedName& variable);

	MidoriResult::TypeResult operator()(MidoriExpression::Bind& bind);

	MidoriResult::TypeResult operator()(MidoriExpression::TextLiteral& text);

	MidoriResult::TypeResult operator()(MidoriExpression::BoolLiteral& bool_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::FloatLiteral& float_literal);

	MidoriResult::TypeResult operator()(MidoriExpression::IntegerLiteral& integer);

	MidoriResult::TypeResult operator()(MidoriExpression::UnitLiteral& unit);

	MidoriResult::TypeResult operator()(MidoriExpression::Function& function);

	MidoriResult::TypeResult operator()(MidoriExpression::Construct& construct);

	MidoriResult::TypeResult operator()(MidoriExpression::Array& array);

	MidoriResult::TypeResult operator()(MidoriExpression::ArrayGet& array_get);

	MidoriResult::TypeResult operator()(MidoriExpression::ArraySet& array_set);

	MidoriResult::TypeResult operator()(MidoriExpression::IfElse& if_else);

	MidoriResult::TypeResult operator()(MidoriExpression::Block& block);

	MidoriResult::TypeResult operator()(MidoriExpression::Match& match);

	MidoriResult::TypeResult operator()(MidoriExpression::Case& case_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Default& default_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Loop& loop);

	MidoriResult::TypeResult operator()(MidoriExpression::Return& return_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Break& break_expr);
};
