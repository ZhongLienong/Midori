#include "AbstractSyntaxTree.h"

MidoriExpression::As::As(Token&& as_keyword, std::shared_ptr<MidoriType> to_type, std::unique_ptr<MidoriExpression>&& expr)
	: m_as_keyword(std::move(as_keyword)),
	m_to_type(std::move(to_type)),
	m_expr(std::move(expr))
{
}

MidoriExpression::Binary::Binary(Token&& op, std::unique_ptr<MidoriExpression>&& left, std::unique_ptr<MidoriExpression>&& right)
	: m_op(std::move(op)),
	m_left(std::move(left)),
	m_right(std::move(right))
{
}

MidoriExpression::Group::Group(std::unique_ptr<MidoriExpression>&& expr_in)
	: m_expr_in(std::move(expr_in))
{
}

MidoriExpression::TextLiteral::TextLiteral(Token&& token)
	: m_token(std::move(token))
{
}

MidoriExpression::BoolLiteral::BoolLiteral(Token&& token)
	: m_token(std::move(token))
{
}

MidoriExpression::FractionLiteral::FractionLiteral(Token&& token)
	: m_token(std::move(token))
{
}

MidoriExpression::IntegerLiteral::IntegerLiteral(Token&& token)
	: m_token(std::move(token))
{
}

MidoriExpression::UnitLiteral::UnitLiteral(Token&& token)
	: m_token(std::move(token))
{
}

MidoriExpression::UnaryPrefix::UnaryPrefix(Token&& op, std::unique_ptr<MidoriExpression>&& expr)
	: m_op(std::move(op)),
	m_expr(std::move(expr))
{
}

MidoriExpression::UnarySuffix::UnarySuffix(Token&& op, std::unique_ptr<MidoriExpression>&& expr)
	: m_op(std::move(op)),
	m_expr(std::move(expr))
{
}

MidoriExpression::Bind::Bind(Token&& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& name_ctx)
	: m_name(std::move(name)),
	m_value(std::move(value)),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::BoundedName::BoundedName(Token&& name, NameContext::Tag&& name_ctx)
	: m_name(std::move(name)),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::Call::Call(Token&& paren, std::unique_ptr<MidoriExpression>&& callee, std::vector<std::unique_ptr<MidoriExpression>>&& arguments, bool is_foreign)
	: m_paren(std::move(paren)),
	m_callee(std::move(callee)),
	m_arguments(std::move(arguments)),
	m_is_foreign(is_foreign)
{
}

MidoriExpression::Function::Function(Token&& function_keyword, std::vector<Token>&& params, std::unique_ptr<MidoriStatement>&& body, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, int captured_count)
	: m_function_keyword(std::move(function_keyword)),
	m_params(std::move(params)),
	m_param_types(std::move(param_types)),
	m_return_type(std::move(return_type)),
	m_body(std::move(body)),
	m_captured_count(captured_count)
{
}

MidoriExpression::Construct::Construct(Token&& data_name, std::vector<std::unique_ptr<MidoriExpression>>&& params, std::shared_ptr<MidoriType>&& return_type, ConstructContext&& construct_ctx)
	: m_data_name(std::move(data_name)),
	m_params(std::move(params)),
	m_return_type(std::move(return_type)),
	m_construct_ctx(std::move(construct_ctx))
{
}

MidoriExpression::Ternary::Ternary(Token&& question, Token&& colon, std::unique_ptr<MidoriExpression>&& condition, std::unique_ptr<MidoriExpression>&& true_branch, std::unique_ptr<MidoriExpression>&& else_branch, ConditionOperandType condition_operand_type)
	: m_question(std::move(question)),
	m_colon(std::move(colon)),
	m_condition(std::move(condition)),
	m_true_branch(std::move(true_branch)),
	m_else_branch(std::move(else_branch)),
	m_condition_operand_type(condition_operand_type)
{
}

MidoriExpression::Get::Get(Token&& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, int index)
	: m_member_name(std::move(member_name)),
	m_struct(std::move(struct_expr)),
	m_index(index)
{
}

MidoriExpression::Set::Set(Token&& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, std::unique_ptr<MidoriExpression>&& value, int index)
	: m_member_name(std::move(member_name)),
	m_struct(std::move(struct_expr)),
	m_value(std::move(value)),
	m_index(index)
{
}

MidoriExpression::Array::Array(Token&& op, std::vector<std::unique_ptr<MidoriExpression>>&& elems)
	: m_op(std::move(op)),
	m_elems(std::move(elems))
{
}

MidoriExpression::ArrayGet::ArrayGet(Token&& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var)
	: m_op(std::move(op)),
	m_indices(std::move(indices)),
	m_arr_var(std::move(arr_var))
{
}

MidoriExpression::ArraySet::ArraySet(Token&& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var, std::unique_ptr<MidoriExpression>&& value)
	: m_op(std::move(op)),
	m_indices(std::move(indices)),
	m_arr_var(std::move(arr_var)),
	m_value(std::move(value))
{
}

bool Switch::IsMemberCase(const Case& c)
{
	return std::holds_alternative<MemberCase>(c);
}

bool Switch::IsDefaultCase(const Case& c)
{
	return std::holds_alternative<DefaultCase>(c);
}

Switch::MemberCase& Switch::GetMemberCase(const Case& c)
{
	return const_cast<MemberCase&>(std::get<MemberCase>(c));
}

Switch::DefaultCase& Switch::GetDefaultCase(const Case& c)
{
	return const_cast<DefaultCase&>(std::get<DefaultCase>(c));
}

const Token& Switch::GetKeyword(const Case& c)
{
	return std::visit
	(
		[](auto&& arg) -> const Token&
		{
			return arg.m_keyword;
		}, 
		c
	);
}

const std::unique_ptr<MidoriStatement>& Switch::GetCaseStatement(const Case& c)
{
	return std::visit
	(
		[](auto&& arg) -> const std::unique_ptr<MidoriStatement>&
		{ 
			return arg.m_stmt; 
		}, 
		c
	);
}

MidoriExpression::ExpressionUnion& MidoriExpression::operator*()
{
	return m_expr_data;
}

std::shared_ptr<MidoriType>& MidoriExpression::GetType()
{
	return std::visit([](auto&& arg) -> std::shared_ptr<MidoriType>& { return arg.m_type_data; }, m_expr_data);
}
