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

	std::string m_errors;
	const std::array<Token::Name, 5u> m_binary_arithmetic_operators{ Token::Name::SINGLE_PLUS, Token::Name::SINGLE_MINUS, Token::Name::STAR, Token::Name::SLASH, Token::Name::PERCENT };
	const std::array<Token::Name, 1u> m_binary_concatenation_operators{ Token::Name::DOUBLE_PLUS };
	const std::array<Token::Name, 4u> m_binary_partial_order_comparison_operators{ Token::Name::LEFT_ANGLE, Token::Name::LESS_EQUAL, Token::Name::RIGHT_ANGLE, Token::Name::GREATER_EQUAL };
	const std::array<Token::Name, 2u> m_binary_equality_operators{ Token::Name::DOUBLE_EQUAL, Token::Name::BANG_EQUAL };
	const std::array<Token::Name, 2u> m_binary_logical_operators{ Token::Name::DOUBLE_AMPERSAND, Token::Name::DOUBLE_BAR };
	const std::array<Token::Name, 5u> m_binary_bitwise_operators{ Token::Name::CARET, Token::Name::SINGLE_AMPERSAND, Token::Name::SINGLE_BAR, Token::Name::RIGHT_SHIFT, Token::Name::LEFT_SHIFT };
	TypeEnvironmentStack m_name_type_table;
	std::weak_ptr<MidoriType> m_curr_function_return_type;

public:

	MidoriResult::TypeCheckerResult TypeCheck(MidoriProgramTree&& program_tree);

private:

	void Unify(std::shared_ptr<MidoriType>& left, std::shared_ptr<MidoriType>& right);

	void AddError(std::string&& error);

	void BeginScope();

	void EndScope();

	void UpdateConditionOperandType(MidoriExpression::ConditionOperandType& op_type, const std::unique_ptr<MidoriExpression>& expr);

	void operator()(Block& block);

	void operator()(Simple& simple);

	void operator()(Define& def);

	void operator()(If& if_stmt);

	void operator()(While& while_stmt);

	void operator()(For& for_stmt);

	void operator()(Break& break_stmt);

	void operator()(Continue& continue_stmt);

	void operator()(Return& return_stmt);

	void operator()(Foreign& foreign_stmt);

	void operator()(Struct& struct_stmt);

	void operator()(Union& union_stmt);

	void operator()(Switch& switch_stmt);

	void operator()(Namespace& namespace_stmt);

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

	MidoriResult::TypeResult operator()(MidoriExpression::FractionLiteral& fraction);

	MidoriResult::TypeResult operator()(MidoriExpression::IntegerLiteral& integer);

	MidoriResult::TypeResult operator()(MidoriExpression::UnitLiteral& unit);

	MidoriResult::TypeResult operator()(MidoriExpression::Function& function);

	MidoriResult::TypeResult operator()(MidoriExpression::Construct& construct);

	MidoriResult::TypeResult operator()(MidoriExpression::Array& array);

	MidoriResult::TypeResult operator()(MidoriExpression::ArrayGet& array_get);

	MidoriResult::TypeResult operator()(MidoriExpression::ArraySet& array_set);

	MidoriResult::TypeResult operator()(MidoriExpression::Ternary& ternary);
};
