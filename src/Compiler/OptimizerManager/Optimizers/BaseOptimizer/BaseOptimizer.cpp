#include "BaseOptimizer.h"
#include "Common/BuildConfig/BuildConfig.h"

void MidoriOptimizer::ResetPassState()
{
	m_did_change = false;
	m_pending_replacement.reset();

#if MIDORI_ENABLE_OPTIMIZER_STATS
	m_optimizations_performed = 0;
#endif
}

bool MidoriOptimizer::DidChange() const
{
	return m_did_change;
}

void MidoriOptimizer::VisitAndReplace(std::unique_ptr<MidoriExpression>& expr)
{
	MidoriAbstractSyntaxTreeWalker::VisitExpression(expr);

	if (m_pending_replacement)
	{
		expr = std::move(m_pending_replacement);
		m_pending_replacement.reset();
		MarkOptimization();
	}
}

void MidoriOptimizer::Replace(std::unique_ptr<MidoriExpression>&& new_node, std::unique_ptr<MidoriExpression>& old_node)
{
	old_node = std::move(new_node);
	MarkOptimization();
}

void MidoriOptimizer::operator()(MidoriStatement::ExpressionStatement& simple)
{
	VisitAndReplace(simple.m_expr);
}

void MidoriOptimizer::operator()(MidoriStatement::VariableDefinition& def)
{
	if (def.m_is_elided)
	{
		return;
	}

	VisitAndReplace(def.m_value);
}

void MidoriOptimizer::operator()(MidoriStatement::TupleDefinition& def_tuple)
{
	VisitAndReplace(def_tuple.m_value);
}

void MidoriOptimizer::operator()(MidoriStatement::FunctionDefinition& defun)
{
	VisitAndReplace(defun.m_body);
}

void MidoriOptimizer::operator()(MidoriStatement::Continue&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::ForeignDefinition&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::Struct&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::Union&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::Class&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::Instance&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::TypeAlias&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::As& as)
{
	VisitAndReplace(as.m_expr);
}

void MidoriOptimizer::operator()(MidoriExpression::Binary& binary)
{
	VisitAndReplace(binary.m_left);
	VisitAndReplace(binary.m_right);
}

void MidoriOptimizer::operator()(MidoriExpression::Group& group)
{
	VisitAndReplace(group.m_expr_in);
}

void MidoriOptimizer::operator()(MidoriExpression::Tuple& tuple)
{
	for (std::unique_ptr<MidoriExpression>& element : tuple.m_elements)
	{
		VisitAndReplace(element);
	}
}

void MidoriOptimizer::operator()(MidoriExpression::UnaryPrefix& unary)
{
	VisitAndReplace(unary.m_expr);
}

void MidoriOptimizer::operator()(MidoriExpression::UnarySuffix& unary)
{
	VisitAndReplace(unary.m_expr);
}

void MidoriOptimizer::operator()(MidoriExpression::Call& call)
{
	VisitAndReplace(call.m_callee);
	for (std::unique_ptr<MidoriExpression>& argument : call.m_arguments)
	{
		VisitAndReplace(argument);
	}
}

void MidoriOptimizer::operator()(MidoriExpression::MemberAccess& get)
{
	VisitAndReplace(get.m_struct);
}

void MidoriOptimizer::operator()(MidoriExpression::MemberAssignment& set)
{
	VisitAndReplace(set.m_struct);
	VisitAndReplace(set.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::NameAccess&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::Assignment& bind)
{
	VisitAndReplace(bind.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::AppendAssign& append_assign)
{
	VisitAndReplace(append_assign.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::ExtendAssign& extend_assign)
{
	VisitAndReplace(extend_assign.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::PrependAssign& prepend_assign)
{
	VisitAndReplace(prepend_assign.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::CompoundAssign& compound_assign)
{
	VisitAndReplace(compound_assign.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::TextLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::BoolLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::FloatLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::IntegerLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::ByteLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::WordLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::UnitLiteral&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::Function& function)
{
	VisitAndReplace(function.m_body);
}

void MidoriOptimizer::operator()(MidoriExpression::Construct& construct)
{
	for (std::unique_ptr<MidoriExpression>& parameter : construct.m_params)
	{
		VisitAndReplace(parameter);
	}
}

void MidoriOptimizer::operator()(MidoriExpression::Array& array)
{
	for (std::unique_ptr<MidoriExpression>& element : array.m_elems)
	{
		VisitAndReplace(element);
	}
}

void MidoriOptimizer::operator()(MidoriExpression::IndexAccess& array_get)
{
	VisitAndReplace(array_get.m_arr_var);
	for (std::unique_ptr<MidoriExpression>& index : array_get.m_indices)
	{
		VisitAndReplace(index);
	}
}

void MidoriOptimizer::operator()(MidoriExpression::IndexAssignment& array_set)
{
	VisitAndReplace(array_set.m_arr_var);
	for (std::unique_ptr<MidoriExpression>& index : array_set.m_indices)
	{
		VisitAndReplace(index);
	}
	VisitAndReplace(array_set.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::ArrayComprehension& comp)
{
	VisitAndReplace(comp.m_transform_expr);
	VisitAndReplace(comp.m_range);
}

void MidoriOptimizer::operator()(MidoriExpression::RangeBinary& range_binary)
{
	VisitAndReplace(range_binary.m_start);
	VisitAndReplace(range_binary.m_end);
}

void MidoriOptimizer::operator()(MidoriExpression::RangeTernary& range_ternary)
{
	VisitAndReplace(range_ternary.m_start);
	VisitAndReplace(range_ternary.m_step);
	VisitAndReplace(range_ternary.m_end);
}

void MidoriOptimizer::operator()(MidoriExpression::IfElse& if_else)
{
	VisitAndReplace(if_else.m_condition);
	VisitAndReplace(if_else.m_true_branch);
	VisitAndReplace(if_else.m_else_branch);
}

void MidoriOptimizer::operator()(MidoriExpression::Block& block)
{
	for (std::unique_ptr<MidoriStatement>& statement : block.m_stmts)
	{
		VisitStatement(statement);
	}

	if (block.m_final_expr.has_value())
	{
		VisitAndReplace(block.m_final_expr.value());
	}
}

void MidoriOptimizer::operator()(MidoriExpression::Match& match)
{
	VisitAndReplace(match.m_arg_expr);
	for (std::unique_ptr<MidoriExpression>& case_expr : match.m_cases)
	{
		VisitAndReplace(case_expr);
	}
}

void MidoriOptimizer::operator()(MidoriExpression::Case& case_expr)
{
	VisitAndReplace(case_expr.m_expr);
}

void MidoriOptimizer::operator()(MidoriExpression::Default& default_expr)
{
	VisitAndReplace(default_expr.m_expr);
}

void MidoriOptimizer::operator()(MidoriExpression::Loop& loop)
{
	VisitAndReplace(loop.m_body);
}

void MidoriOptimizer::operator()(MidoriExpression::For& for_expr)
{
	VisitAndReplace(for_expr.m_range);
	VisitAndReplace(for_expr.m_body);
}

void MidoriOptimizer::operator()(MidoriExpression::Break& break_expr)
{
	VisitAndReplace(break_expr.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::Return& return_expr)
{
	VisitAndReplace(return_expr.m_value);
}

#if MIDORI_ENABLE_OPTIMIZER_STATS
int MidoriOptimizer::GetOptimizationsPerformed() const
{
	return m_optimizations_performed;
}

void MidoriOptimizer::MarkOptimization()
{
	m_did_change = true;
	m_optimizations_performed += 1;
}

#endif
