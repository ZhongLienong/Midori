#include "BaseOptimizer.h"
#include "Common/BuildConfig/BuildConfig.h"

void MidoriOptimizer::VisitAndReplace(std::unique_ptr<MidoriExpression>& expr)
{
	std::visit([this](auto&& arg) { (*this)(arg); }, **expr);

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

void MidoriOptimizer::operator()(MidoriStatement::Simple& simple)
{
	VisitAndReplace(simple.m_expr);
}

void MidoriOptimizer::operator()(MidoriStatement::Define& def)
{
	VisitAndReplace(def.m_value);
}

void MidoriOptimizer::operator()(MidoriStatement::DefineTuple& def_tuple)
{
	VisitAndReplace(def_tuple.m_value);
}

void MidoriOptimizer::operator()(MidoriStatement::DefineFunction& defun)
{
	VisitAndReplace(defun.m_body);
}

void MidoriOptimizer::operator()(MidoriStatement::Continue&)
{
}

void MidoriOptimizer::operator()(MidoriStatement::Foreign&)
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
	std::ranges::for_each
	(
		tuple.m_elements,
		[this](std::unique_ptr<MidoriExpression>& element)
		{
			VisitAndReplace(element);
		}
	);
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
	std::ranges::for_each
	(
		call.m_arguments,
		[this](std::unique_ptr<MidoriExpression>& arg)
		{
			VisitAndReplace(arg);
		}
	);
}

void MidoriOptimizer::operator()(MidoriExpression::Get& get)
{
	VisitAndReplace(get.m_struct);
}

void MidoriOptimizer::operator()(MidoriExpression::Set& set)
{
	VisitAndReplace(set.m_struct);
	VisitAndReplace(set.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::BoundedName&)
{
}

void MidoriOptimizer::operator()(MidoriExpression::Bind& bind)
{
	VisitAndReplace(bind.m_value);
}

void MidoriOptimizer::operator()(MidoriExpression::AppendAssign& append_assign)
{
	VisitAndReplace(append_assign.m_value);
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
	std::ranges::for_each
	(
		construct.m_params,
		[this](std::unique_ptr<MidoriExpression>& arg)
		{
			VisitAndReplace(arg);
		}
	);
}

void MidoriOptimizer::operator()(MidoriExpression::Array& array)
{
	std::ranges::for_each
	(
		array.m_elems,
		[this](std::unique_ptr<MidoriExpression>& element)
		{
			VisitAndReplace(element);
		}
	);
}

void MidoriOptimizer::operator()(MidoriExpression::ArrayGet& array_get)
{
	VisitAndReplace(array_get.m_arr_var);
	std::ranges::for_each
	(
		array_get.m_indices,
		[this](std::unique_ptr<MidoriExpression>& index)
		{
			VisitAndReplace(index);
		}
	);
}

void MidoriOptimizer::operator()(MidoriExpression::ArraySet& array_set)
{
	VisitAndReplace(array_set.m_arr_var);
	std::ranges::for_each
	(
		array_set.m_indices,
		[this](std::unique_ptr<MidoriExpression>& index)
		{
			VisitAndReplace(index);
		}
	);
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
	std::ranges::for_each
	(
		block.m_stmts,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **stmt);
		}
	);

	if (block.m_final_expr.has_value())
	{
		VisitAndReplace(block.m_final_expr.value());
	}
}

void MidoriOptimizer::operator()(MidoriExpression::Match& match)
{
	VisitAndReplace(match.m_arg_expr);
	std::ranges::for_each
	(
		match.m_cases,
		[this](std::unique_ptr<MidoriExpression>& case_expr)
		{
			VisitAndReplace(case_expr);
		}
	);
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

void MidoriOptimizer::operator()(MidoriExpression::Async& async_expr)
{
	VisitAndReplace(async_expr.m_expr);
}

void MidoriOptimizer::operator()(MidoriExpression::Await& await_expr)
{
	VisitAndReplace(await_expr.m_expr);
}

#if MIDORI_ENABLE_OPTIMIZER_STATS
int MidoriOptimizer::GetOptimizationsPerformed() const
{
	return m_optimizations_performed;
}

void MidoriOptimizer::MarkOptimization()
{
	m_optimizations_performed += 1;
}

void MidoriOptimizer::ResetCounter()
{
	m_optimizations_performed = 0;
}
#endif