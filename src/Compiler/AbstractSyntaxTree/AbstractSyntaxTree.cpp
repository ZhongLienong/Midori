#include "AbstractSyntaxTree.h"

MidoriPattern::Binding::Binding(const Token& name, std::optional<int> local_index)
	: m_name(name),
	m_local_index(std::move(local_index))
{
}

MidoriPattern::Literal::Literal(const Token& token, LiteralKind kind)
	: m_token(token),
	m_kind(kind)
{
}

MidoriPattern::Tuple::Tuple(const Token& left_paren, std::vector<std::unique_ptr<MidoriPattern>>&& elements)
	: m_left_paren(left_paren),
	m_elements(std::move(elements))
{
}

MidoriPattern::Array::Array(const Token& left_bracket, std::vector<std::unique_ptr<MidoriPattern>>&& elements)
	: m_left_bracket(left_bracket),
	m_elements(std::move(elements))
{
}

MidoriPattern::Constructor::Constructor(const Token& name_token, std::string&& name, std::vector<std::unique_ptr<MidoriPattern>>&& args, bool is_union)
	: m_name_token(name_token),
	m_name(std::move(name)),
	m_args(std::move(args)),
	m_is_union(is_union)
{
}

MidoriExpression::As::As(const Token& as_keyword, std::shared_ptr<MidoriType> to_type, std::unique_ptr<MidoriExpression>&& expr)
	: m_as_keyword(as_keyword),
	m_to_type(std::move(to_type)),
	m_expr(std::move(expr))
{
}

MidoriExpression::Binary::Binary(const Token& op, std::unique_ptr<MidoriExpression>&& left, std::unique_ptr<MidoriExpression>&& right)
	: m_op(op),
	m_left(std::move(left)),
	m_right(std::move(right))
{
}

MidoriExpression::Group::Group(std::unique_ptr<MidoriExpression>&& expr_in)
	: m_expr_in(std::move(expr_in))
{
}

MidoriExpression::Tuple::Tuple(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& elements)
	: m_op(op), m_elements(std::move(elements))
{
}

MidoriExpression::TextLiteral::TextLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::BoolLiteral::BoolLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::FloatLiteral::FloatLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::IntegerLiteral::IntegerLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::ByteLiteral::ByteLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::WordLiteral::WordLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::UnitLiteral::UnitLiteral(const Token& token)
	: m_token(token)
{
}

MidoriExpression::UnaryPrefix::UnaryPrefix(const Token& op, std::unique_ptr<MidoriExpression>&& expr)
	: m_op(op),
	m_expr(std::move(expr))
{
}

MidoriExpression::UnarySuffix::UnarySuffix(const Token& op, std::unique_ptr<MidoriExpression>&& expr)
	: m_op(op),
	m_expr(std::move(expr))
{
}

MidoriExpression::Assignment::Assignment(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& name_ctx)
	: m_name(name),
	m_value(std::move(value)),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::AppendAssign::AppendAssign(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& name_ctx)
	: m_name(name),
	m_value(std::move(value)),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::PrependAssign::PrependAssign(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& name_ctx)
	: m_name(name),
	m_value(std::move(value)),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::CompoundAssign::CompoundAssign(const Token& name, const Token& op, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& name_ctx)
	: m_name(name),
	m_op(op),
	m_value(std::move(value)),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::NameAccess::NameAccess(const Token& name, NameContext::Tag&& name_ctx)
	: m_name(name),
	m_name_ctx(std::move(name_ctx))
{
}

MidoriExpression::Call::Call(const Token& paren, std::unique_ptr<MidoriExpression>&& callee, std::vector<std::unique_ptr<MidoriExpression>>&& arguments, bool is_foreign)
	: m_paren(paren),
	m_callee(std::move(callee)),
	m_arguments(std::move(arguments)),
	m_is_foreign(is_foreign)
{
}

MidoriExpression::Function::Function(const Token& function_keyword, std::vector<Token>&& generic_params, std::vector<Token>&& params, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, std::unique_ptr<MidoriExpression>&& body, int captured_count)
	: m_function_keyword(function_keyword),
	m_generic_params(std::move(generic_params)),
	m_params(std::move(params)),
	m_param_types(std::move(param_types)),
	m_return_type(std::move(return_type)),
	m_body(std::move(body)),
	m_captured_count(captured_count)
{
}

MidoriExpression::Construct::Construct(const Token& data_name, std::vector<std::unique_ptr<MidoriExpression>>&& params, std::shared_ptr<MidoriType>&& return_type, ConstructContext&& construct_ctx)
	: m_data_name(data_name),
	m_params(std::move(params)),
	m_return_type(std::move(return_type)),
	m_construct_ctx(std::move(construct_ctx))
{
}

MidoriExpression::IfElse::IfElse(const Token& if_token, const Token& then_token, const Token& else_token, std::unique_ptr<MidoriExpression>&& condition, std::unique_ptr<MidoriExpression>&& true_branch, std::unique_ptr<MidoriExpression>&& else_branch, ConditionOperandType condition_operand_type)
	:
	m_if_token(if_token),
	m_then_token(then_token),
	m_else_token(else_token),
	m_condition(std::move(condition)),
	m_true_branch(std::move(true_branch)),
	m_else_branch(std::move(else_branch)),
	m_condition_operand_type(condition_operand_type)
{
}

MidoriExpression::MemberAccess::MemberAccess(const Token& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, int index)
	: m_member_name(member_name),
	m_struct(std::move(struct_expr)),
	m_index(index)
{
}

MidoriExpression::MemberAssignment::MemberAssignment(const Token& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, std::unique_ptr<MidoriExpression>&& value, int index)
	: m_member_name(member_name),
	m_struct(std::move(struct_expr)),
	m_value(std::move(value)),
	m_index(index)
{
}

MidoriExpression::Array::Array(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& elems)
	: m_op(op),
	m_elems(std::move(elems))
{
}

MidoriExpression::IndexAccess::IndexAccess(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var)
	: m_op(op),
	m_indices(std::move(indices)),
	m_arr_var(std::move(arr_var))
{
}

MidoriExpression::IndexAssignment::IndexAssignment(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var, std::unique_ptr<MidoriExpression>&& value)
	: m_op(op),
	m_indices(std::move(indices)),
	m_arr_var(std::move(arr_var)),
	m_value(std::move(value))
{
}

MidoriExpression::ArrayComprehension::ArrayComprehension(const Token& bracket, const Token& loop_variable, const Token& in_keyword,
	std::unique_ptr<MidoriExpression>&& transform_expr, std::unique_ptr<MidoriExpression>&& range)
	: m_bracket(bracket),
	m_loop_variable(loop_variable),
	m_in_keyword(in_keyword),
	m_transform_expr(std::move(transform_expr)),
	m_range(std::move(range))
{
}

MidoriExpression::RangeBinary::RangeBinary(const Token& range_op, std::unique_ptr<MidoriExpression>&& start, std::unique_ptr<MidoriExpression>&& end)
	: m_range_op(range_op),
	m_start(std::move(start)),
	m_end(std::move(end))
{
}

MidoriExpression::RangeTernary::RangeTernary(const Token& first_op, const Token& second_op, std::unique_ptr<MidoriExpression>&& start, std::unique_ptr<MidoriExpression>&& step, std::unique_ptr<MidoriExpression>&& end)
	: m_first_range_op(first_op),
	m_second_range_op(second_op),
	m_start(std::move(start)),
	m_step(std::move(step)),
	m_end(std::move(end))
{
}

MidoriExpression::Block::Block(const Token& right_brace, std::vector<std::unique_ptr<MidoriStatement>>&& stmts, int local_count, std::unique_ptr<MidoriExpression>&& final_expr)
	: m_right_brace(right_brace),
	m_stmts(std::move(stmts)),
	m_final_expr(final_expr == nullptr ? std::nullopt : std::optional<std::unique_ptr<MidoriExpression>>(std::move(final_expr))),
	m_local_count(local_count)
{
}

MidoriExpression::Match::Match(const Token& switch_keyword, std::unique_ptr<MidoriExpression>&& arg_expr, std::vector<std::unique_ptr<MidoriExpression>>&& cases)
	: m_match_keyword(switch_keyword),
	m_arg_expr(std::move(arg_expr)),
	m_cases(std::move(cases))
{
}

MidoriExpression::Case::Case(const Token& keyword, std::unique_ptr<MidoriPattern>&& pattern, std::unique_ptr<MidoriExpression>&& expr, int binding_count)
	: m_keyword(keyword),
	m_pattern(std::move(pattern)),
	m_expr(std::move(expr)),
	m_binding_count(binding_count)
{
}

MidoriExpression::Default::Default(const Token& keyword, std::unique_ptr<MidoriExpression>&& expr)
	: m_keyword(keyword),
	m_expr(std::move(expr))
{
}

MidoriExpression::Loop::Loop(const Token& loop_keyword, std::unique_ptr<MidoriExpression>&& body)
	: m_loop_keyword(loop_keyword),
	m_body(std::move(body))
{
}

MidoriExpression::For::For(const Token& for_keyword, const Token& loop_variable, const Token& in_keyword,
	std::unique_ptr<MidoriExpression>&& range, std::unique_ptr<MidoriExpression>&& body)
	: m_for_keyword(for_keyword),
	m_loop_variable(loop_variable),
	m_in_keyword(in_keyword),
	m_range(std::move(range)),
	m_body(std::move(body))
{
}

MidoriExpression::Return::Return(const Token& keyword, std::unique_ptr<MidoriExpression>&& value)
	: m_keyword(keyword),
	m_value(std::move(value))
{
}

MidoriExpression::Break::Break(const Token& keyword, int number_to_pop, std::unique_ptr<MidoriExpression>&& value)
	: m_keyword(keyword),
	m_number_to_pop(number_to_pop),
	m_value(std::move(value))
{
}

MidoriExpression::Async::Async(const Token& keyword, std::unique_ptr<MidoriExpression>&& expr)
	: m_keyword(keyword),
	m_expr(std::move(expr))
{
}

MidoriExpression::Await::Await(const Token& keyword, std::unique_ptr<MidoriExpression>&& expr)
	: m_keyword(keyword),
	m_expr(std::move(expr))
{
}

bool MidoriExpression::Block::HasDefine() const
{
	return std::ranges::any_of(m_stmts, [](const std::unique_ptr<MidoriStatement>& stmt) { return stmt->IsStatement<MidoriStatement::VariableDefinition>(); });
}

MidoriPattern::PatternUnion& MidoriPattern::operator*()
{
	return m_variant;
}

const MidoriPattern::PatternUnion& MidoriPattern::operator*() const
{
	return m_variant;
}

std::shared_ptr<MidoriType>& MidoriPattern::GetType()
{
	return std::visit([](auto&& arg) -> std::shared_ptr<MidoriType>& { return arg.m_type_data; }, m_variant);
}

const std::shared_ptr<MidoriType>& MidoriPattern::GetType() const
{
	return std::visit([](auto&& arg) -> const std::shared_ptr<MidoriType>& { return arg.m_type_data; }, m_variant);
}

MidoriExpression::ExpressionUnion& MidoriExpression::operator*()
{
	return m_variant;
}

std::shared_ptr<MidoriType>& MidoriExpression::GetType()
{
	return std::visit([](auto&& arg) -> std::shared_ptr<MidoriType>& { return arg.m_type_data; }, m_variant);
}

MidoriStatement::StatementUnion& MidoriStatement::operator*()
{
	return m_variant;
}

MidoriStatement::ExpressionStatement::ExpressionStatement(const Token& semicolon, std::unique_ptr<MidoriExpression>&& expr)
	: m_semicolon(semicolon), m_expr(std::move(expr))
{
}

MidoriStatement::VariableDefinition::VariableDefinition(const Token& name, std::unique_ptr<MidoriExpression>&& value, std::optional<std::shared_ptr<MidoriType>>&& annotated_type, std::optional<int>&& local_index)
	: m_name(name),
	m_value(std::move(value)),
	m_annotated_type(std::move(annotated_type)),
	m_local_index(std::move(local_index))
{
}

MidoriStatement::TupleDefinition::TupleDefinition(std::vector<Token>&& names, std::unique_ptr<MidoriExpression>&& value, std::vector<std::optional<int>>&& local_indices)
	: m_names(std::move(names)),
	m_value(std::move(value)),
	m_local_indices(std::move(local_indices))
{
}

MidoriStatement::Continue::Continue(const Token& keyword, int number_to_pop)
	: m_keyword(keyword),
	m_number_to_pop(number_to_pop)
{
}

MidoriStatement::ForeignDefinition::ForeignDefinition(const Token& function_name, const std::string& foreign_name, std::shared_ptr<MidoriType>&& type, std::optional<int>&& local_index)
	: m_function_name(function_name),
	m_foreign_name(foreign_name),
	m_type(std::move(type)),
	m_local_index(std::move(local_index))
{
}

MidoriStatement::Struct::Struct(const Token& name, std::vector<Token>&& generic_params, std::vector<MidoriType::ClassConstraint>&& constraints, std::shared_ptr<MidoriType>&& self_type)
	: m_name(name),
	m_generic_params(std::move(generic_params)),
	m_constraints(std::move(constraints)),
	m_self_type(std::move(self_type))
{
}

MidoriStatement::Union::Union(const Token& name, std::vector<Token>&& generic_params, std::vector<MidoriType::ClassConstraint>&& constraints, std::shared_ptr<MidoriType>&& self_type)
	: m_name(name),
	m_generic_params(std::move(generic_params)),
	m_constraints(std::move(constraints)),
	m_self_type(std::move(self_type))
{
}

MidoriStatement::FunctionDefinition::FunctionDefinition(const Token& name, std::vector<Token>&& generic_params, std::vector<Token>&& params, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, std::unique_ptr<MidoriExpression>&& body, std::optional<int>&& local_index, int captured_count, std::vector<MidoriType::ClassConstraint>&& constraints)
	: m_name(name),
	m_generic_params(std::move(generic_params)),
	m_params(std::move(params)),
	m_param_types(std::move(param_types)),
	m_return_type(std::move(return_type)),
	m_body(std::move(body)),
	m_local_index(std::move(local_index)),
	m_captured_count(captured_count),
	m_constraints(std::move(constraints))
{
}

MidoriStatement::Class::Class(const Token& name, std::vector<Token>&& type_params, std::vector<MidoriType::ClassConstraint>&& superclasses, std::vector<std::unique_ptr<MidoriStatement>>&& methods)
	: m_name(name),
	m_type_params(std::move(type_params)),
	m_superclasses(std::move(superclasses)),
	m_methods(std::move(methods))
{
}

MidoriStatement::Instance::Instance(const Token& class_name, std::vector<std::shared_ptr<MidoriType>>&& type_args, std::vector<MidoriType::ClassConstraint>&& constraints, std::vector<std::unique_ptr<MidoriStatement>>&& methods)
	: m_class_name(class_name),
	m_type_args(std::move(type_args)),
	m_constraints(std::move(constraints)),
	m_methods(std::move(methods))
{
}

MidoriStatement::TypeAlias::TypeAlias(const Token& name, std::vector<Token>&& generic_params, std::shared_ptr<MidoriType>&& aliased_type)
	: m_name(name),
	m_generic_params(std::move(generic_params)),
	m_aliased_type(std::move(aliased_type))
{
}
