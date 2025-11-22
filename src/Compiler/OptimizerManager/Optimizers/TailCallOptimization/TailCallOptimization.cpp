#include "TailCallOptimization.h"
#include "Common/BuildConfig/BuildConfig.h"

int TailCallOptimization::Optimize(MidoriProgramTree& program_tree)
{
#if MIDORI_ENABLE_OPTIMIZER_STATS
	ResetCounter();
#endif
	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **stmt);
		}
	);
#if MIDORI_ENABLE_OPTIMIZER_STATS
	return GetOptimizationsPerformed();
#else
	return 0;
#endif
}

std::string_view TailCallOptimization::GetName() const
{
	return "TailCallOptimization";
}

void TailCallOptimization::operator()(MidoriStatement::DefineFunction& defun)
{
	m_current_function = defun.m_name.m_lexeme;
	m_has_tail_recursion = false;

	if (IsTailRecursive(defun.m_body, m_current_function))
	{
		MarkOptimization();
	}

	m_current_function.clear();
}

bool TailCallOptimization::IsTailCall(std::unique_ptr<MidoriExpression>& expr, std::string_view function_name)
{
	if (!expr->IsExpression<MidoriExpression::Call>())
	{
		return false;
	}

	MidoriExpression::Call& call = expr->GetExpression<MidoriExpression::Call>();
	if (call.m_callee->IsExpression<MidoriExpression::BoundedName>())
	{
		MidoriExpression::BoundedName& callee_name = call.m_callee->GetExpression<MidoriExpression::BoundedName>();
		if (callee_name.m_name.m_lexeme == function_name)
		{
			call.m_is_tail_call = true;
			return true;
		}
	}
	return false;
}

bool TailCallOptimization::ContainsRecursiveCall(std::unique_ptr<MidoriExpression>& expr, std::string_view function_name)
{
	if (expr->IsExpression<MidoriExpression::Call>())
	{
		MidoriExpression::Call& call = expr->GetExpression<MidoriExpression::Call>();
		if (call.m_callee->IsExpression<MidoriExpression::BoundedName>())
		{
			MidoriExpression::BoundedName& callee = call.m_callee->GetExpression<MidoriExpression::BoundedName>();
			if (callee.m_name.m_lexeme == function_name)
			{
				return true;
			}
		}
		for (std::unique_ptr<MidoriExpression>& arg : call.m_arguments)
		{
			if (ContainsRecursiveCall(arg, function_name))
			{
				return true;
			}
		}
	}
	else if (expr->IsExpression<MidoriExpression::Binary>())
	{
		MidoriExpression::Binary& binary = expr->GetExpression<MidoriExpression::Binary>();
		return ContainsRecursiveCall(binary.m_left, function_name) || ContainsRecursiveCall(binary.m_right, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::UnaryPrefix>())
	{
		MidoriExpression::UnaryPrefix& unary = expr->GetExpression<MidoriExpression::UnaryPrefix>();
		return ContainsRecursiveCall(unary.m_expr, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::UnarySuffix>())
	{
		MidoriExpression::UnarySuffix& unary = expr->GetExpression<MidoriExpression::UnarySuffix>();
		return ContainsRecursiveCall(unary.m_expr, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::Group>())
	{
		MidoriExpression::Group& group = expr->GetExpression<MidoriExpression::Group>();
		return ContainsRecursiveCall(group.m_expr_in, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::IfElse>())
	{
		MidoriExpression::IfElse& if_else = expr->GetExpression<MidoriExpression::IfElse>();
		return ContainsRecursiveCall(if_else.m_condition, function_name) || ContainsRecursiveCall(if_else.m_true_branch, function_name) || ContainsRecursiveCall(if_else.m_else_branch, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::Block>())
	{
		MidoriExpression::Block& block = expr->GetExpression<MidoriExpression::Block>();
		if (block.m_final_expr.has_value())
		{
			return ContainsRecursiveCall(block.m_final_expr.value(), function_name);
		}
	}
	else if (expr->IsExpression<MidoriExpression::Array>())
	{
		MidoriExpression::Array& array = expr->GetExpression<MidoriExpression::Array>();
		for (std::unique_ptr<MidoriExpression>& elem : array.m_elems)
		{
			if (ContainsRecursiveCall(elem, function_name))
			{
				return true;
			}
		}
	}
	else if (expr->IsExpression<MidoriExpression::Construct>())
	{
		MidoriExpression::Construct& construct = expr->GetExpression<MidoriExpression::Construct>();
		for (std::unique_ptr<MidoriExpression>& param : construct.m_params)
		{
			if (ContainsRecursiveCall(param, function_name))
			{
				return true;
			}
		}
	}
	else if (expr->IsExpression<MidoriExpression::Return>())
	{
		MidoriExpression::Return& ret = expr->GetExpression<MidoriExpression::Return>();
		return ContainsRecursiveCall(ret.m_value, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::Break>())
	{
		MidoriExpression::Break& brk = expr->GetExpression<MidoriExpression::Break>();
		return ContainsRecursiveCall(brk.m_value, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::Match>())
	{
		MidoriExpression::Match& match = expr->GetExpression<MidoriExpression::Match>();
		if (ContainsRecursiveCall(match.m_arg_expr, function_name))
		{
			return true;
		}
		for (std::unique_ptr<MidoriExpression>& case_expr : match.m_cases)
		{
			if (ContainsRecursiveCall(case_expr, function_name))
			{
				return true;
			}
		}
	}
	else if (expr->IsExpression<MidoriExpression::Case>())
	{
		MidoriExpression::Case& case_expr = expr->GetExpression<MidoriExpression::Case>();
		return ContainsRecursiveCall(case_expr.m_expr, function_name);
	}
	else if (expr->IsExpression<MidoriExpression::Default>())
	{
		MidoriExpression::Default& default_expr = expr->GetExpression<MidoriExpression::Default>();
		return ContainsRecursiveCall(default_expr.m_expr, function_name);
	}

	return false;
}

bool TailCallOptimization::IsTailRecursive(std::unique_ptr<MidoriExpression>& expr, std::string_view function_name)
{
	if (IsTailCall(expr, function_name))
	{
		return true;
	}

	if (expr->IsExpression<MidoriExpression::Return>())
	{
		MidoriExpression::Return& return_expr = expr->GetExpression<MidoriExpression::Return>();
		return IsTailRecursive(return_expr.m_value, function_name);
	}
	if (expr->IsExpression<MidoriExpression::IfElse>())
	{
		MidoriExpression::IfElse& if_else = expr->GetExpression<MidoriExpression::IfElse>();

		bool then_has_call = ContainsRecursiveCall(if_else.m_true_branch, function_name);
		bool else_has_call = ContainsRecursiveCall(if_else.m_else_branch, function_name);
		bool then_is_tail = IsTailRecursive(if_else.m_true_branch, function_name);
		bool else_is_tail = IsTailRecursive(if_else.m_else_branch, function_name);

		// If a branch has a call but it's NOT in tail position, reject the whole function
		if ((then_has_call && !then_is_tail) || (else_has_call && !else_is_tail))
		{
			return false;
		}

		// At least one branch must have a tail call
		return then_is_tail || else_is_tail;
	}

	if (expr->IsExpression<MidoriExpression::Block>())
	{
		MidoriExpression::Block& block = expr->GetExpression<MidoriExpression::Block>();
		if (block.m_final_expr.has_value())
		{
			return IsTailRecursive(block.m_final_expr.value(), function_name);
		}
		return false;
	}
	if (expr->IsExpression<MidoriExpression::Case>())
	{
		MidoriExpression::Case& case_expr = expr->GetExpression<MidoriExpression::Case>();
		return IsTailRecursive(case_expr.m_expr, function_name);
	}
	if (expr->IsExpression<MidoriExpression::Default>())
	{
		MidoriExpression::Default& default_expr = expr->GetExpression<MidoriExpression::Default>();
		return IsTailRecursive(default_expr.m_expr, function_name);
	}
	if (expr->IsExpression<MidoriExpression::Break>())
	{
		MidoriExpression::Break& break_expr = expr->GetExpression<MidoriExpression::Break>();
		return IsTailRecursive(break_expr.m_value, function_name);
	}
	if (expr->IsExpression<MidoriExpression::Group>())
	{
		MidoriExpression::Group& group = expr->GetExpression<MidoriExpression::Group>();
		return IsTailRecursive(group.m_expr_in, function_name);
	}
	if (expr->IsExpression<MidoriExpression::Match>())
	{
		MidoriExpression::Match& match_expr = expr->GetExpression<MidoriExpression::Match>();

		bool has_tail_call = false;
		for (std::unique_ptr<MidoriExpression>& case_expr : match_expr.m_cases)
		{
			bool case_has_call = ContainsRecursiveCall(case_expr, function_name);
			bool case_is_tail = IsTailRecursive(case_expr, function_name);

			// If this case has a call but it's NOT in tail position, reject
			if (case_has_call && !case_is_tail)
			{
				return false;
			}

			if (case_is_tail)
			{
				has_tail_call = true;
			}
		}

		return has_tail_call;
	}

	// For any other expression type, if it contains a recursive call, it's NOT in tail position
	if (ContainsRecursiveCall(expr, function_name))
	{
		return false;
	}

	return false;
}