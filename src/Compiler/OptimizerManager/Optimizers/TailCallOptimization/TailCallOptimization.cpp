#include "TailCallOptimization.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Compiler/OptimizerManager/Analysis/OptimizerAnalysis.h"

namespace
{
	bool ContainsRecursiveCallImpl(const MidoriExpression& expr, std::string_view function_name);

	bool ContainsRecursiveCallInStatement(const MidoriStatement& stmt, std::string_view function_name)
	{
		if (stmt.IsStatement<MidoriStatement::ExpressionStatement>())
		{
			return ContainsRecursiveCallImpl(*stmt.GetStatement<MidoriStatement::ExpressionStatement>().m_expr, function_name);
		}
		if (stmt.IsStatement<MidoriStatement::VariableDefinition>())
		{
			return ContainsRecursiveCallImpl(*stmt.GetStatement<MidoriStatement::VariableDefinition>().m_value, function_name);
		}
		if (stmt.IsStatement<MidoriStatement::TupleDefinition>())
		{
			return ContainsRecursiveCallImpl(*stmt.GetStatement<MidoriStatement::TupleDefinition>().m_value, function_name);
		}
		return false;
	}

	bool ContainsRecursiveCallInBlockStatements(const MidoriExpression::Block& block, std::string_view function_name)
	{
		for (const std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
		{
			if (ContainsRecursiveCallInStatement(*stmt, function_name))
			{
				return true;
			}
		}
		return false;
	}

	bool ContainsRecursiveCallImpl(const MidoriExpression& expr, std::string_view function_name)
	{
		struct RecursiveCallVisitor
		{
			std::string_view m_function_name;

			bool operator()(const MidoriExpression::Call& node) const
			{
				const MidoriExpression* callee_expr = OptimizerAnalysis::StripRedundantGroups(node.m_callee.get());
				if (callee_expr && callee_expr->IsExpression<MidoriExpression::NameAccess>())
				{
					const MidoriExpression::NameAccess& callee_name = callee_expr->GetExpression<MidoriExpression::NameAccess>();
					if (callee_name.m_name.m_lexeme == m_function_name)
					{
						return true;
					}
				}

				if (ContainsRecursiveCallImpl(*node.m_callee, m_function_name))
				{
					return true;
				}

				for (const std::unique_ptr<MidoriExpression>& arg : node.m_arguments)
				{
					if (ContainsRecursiveCallImpl(*arg, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::As& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr, m_function_name);
			}

			bool operator()(const MidoriExpression::Binary& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_left, m_function_name)
					|| ContainsRecursiveCallImpl(*node.m_right, m_function_name);
			}

			bool operator()(const MidoriExpression::Group& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr_in, m_function_name);
			}

			bool operator()(const MidoriExpression::Tuple& node) const
			{
				for (const std::unique_ptr<MidoriExpression>& elem : node.m_elements)
				{
					if (ContainsRecursiveCallImpl(*elem, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::UnaryPrefix& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr, m_function_name);
			}

			bool operator()(const MidoriExpression::UnarySuffix& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr, m_function_name);
			}

			bool operator()(const MidoriExpression::Assignment& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::AppendAssign& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::ExtendAssign& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::PrependAssign& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::CompoundAssign& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::NameAccess&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::Function&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::Construct& node) const
			{
				for (const std::unique_ptr<MidoriExpression>& param : node.m_params)
				{
					if (ContainsRecursiveCallImpl(*param, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::IfElse& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_condition, m_function_name)
					|| ContainsRecursiveCallImpl(*node.m_true_branch, m_function_name)
					|| ContainsRecursiveCallImpl(*node.m_else_branch, m_function_name);
			}

			bool operator()(const MidoriExpression::MemberAccess& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_struct, m_function_name);
			}

			bool operator()(const MidoriExpression::MemberAssignment& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_struct, m_function_name) || ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::Array& node) const
			{
				for (const std::unique_ptr<MidoriExpression>& elem : node.m_elems)
				{
					if (ContainsRecursiveCallImpl(*elem, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::IndexAccess& node) const
			{
				if (ContainsRecursiveCallImpl(*node.m_arr_var, m_function_name))
				{
					return true;
				}
				for (const std::unique_ptr<MidoriExpression>& index : node.m_indices)
				{
					if (ContainsRecursiveCallImpl(*index, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::IndexAssignment& node) const
			{
				if (ContainsRecursiveCallImpl(*node.m_arr_var, m_function_name) || ContainsRecursiveCallImpl(*node.m_value, m_function_name))
				{
					return true;
				}
				for (const std::unique_ptr<MidoriExpression>& index : node.m_indices)
				{
					if (ContainsRecursiveCallImpl(*index, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::ArrayComprehension& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_transform_expr, m_function_name) || ContainsRecursiveCallImpl(*node.m_range, m_function_name);
			}

			bool operator()(const MidoriExpression::RangeBinary& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_start, m_function_name) || ContainsRecursiveCallImpl(*node.m_end, m_function_name);
			}

			bool operator()(const MidoriExpression::RangeTernary& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_start, m_function_name)
					|| ContainsRecursiveCallImpl(*node.m_step, m_function_name)
					|| ContainsRecursiveCallImpl(*node.m_end, m_function_name);
			}

			bool operator()(const MidoriExpression::Block& node) const
			{
				if (ContainsRecursiveCallInBlockStatements(node, m_function_name))
				{
					return true;
				}
				return node.m_final_expr.has_value() && ContainsRecursiveCallImpl(*node.m_final_expr.value(), m_function_name);
			}

			bool operator()(const MidoriExpression::Match& node) const
			{
				if (ContainsRecursiveCallImpl(*node.m_arg_expr, m_function_name))
				{
					return true;
				}
				for (const std::unique_ptr<MidoriExpression>& case_expr : node.m_cases)
				{
					if (ContainsRecursiveCallImpl(*case_expr, m_function_name))
					{
						return true;
					}
				}
				return false;
			}

			bool operator()(const MidoriExpression::Case& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr, m_function_name);
			}

			bool operator()(const MidoriExpression::Default& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr, m_function_name);
			}

			bool operator()(const MidoriExpression::Loop& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_body, m_function_name);
			}

			bool operator()(const MidoriExpression::For& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_range, m_function_name) || ContainsRecursiveCallImpl(*node.m_body, m_function_name);
			}

			bool operator()(const MidoriExpression::Return& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::Break& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_value, m_function_name);
			}

			bool operator()(const MidoriExpression::TextLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::BoolLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::FloatLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::IntegerLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::ByteLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::WordLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::UnitLiteral&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::Async&) const
			{
				return false;
			}

			bool operator()(const MidoriExpression::Await& node) const
			{
				return ContainsRecursiveCallImpl(*node.m_expr, m_function_name);
			}
		};

		return std::visit(RecursiveCallVisitor{ function_name }, *expr);
	}
}

MidoriResult::OptimizerResult TailCallOptimization::Optimize(MidoriProgramTree program_tree)
{
	ResetPassState();
	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			VisitStatement(stmt);
		}
	);
	return std::move(program_tree);
}

std::string_view TailCallOptimization::GetName() const
{
	return "TailCallOptimization";
}

void TailCallOptimization::operator()(MidoriStatement::FunctionDefinition& defun)
{
	m_current_function = defun.m_name.m_lexeme;
	m_has_tail_recursion = false;
	m_marked_new_tail_call = false;

	m_has_tail_recursion = IsTailRecursive(defun.m_body, m_current_function);
	if (m_marked_new_tail_call)
	{
		MarkOptimization();
	}

	m_current_function.clear();

	VisitAndReplace(defun.m_body);
}

void TailCallOptimization::operator()(MidoriExpression::Block& block)
{
	for (std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
	{
		if (stmt->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			MidoriStatement::FunctionDefinition& nested_defun = stmt->GetStatement<MidoriStatement::FunctionDefinition>();
			(*this)(nested_defun);
		}
	}

	if (block.m_final_expr.has_value())
	{
		VisitAndReplace(block.m_final_expr.value());
	}
}

bool TailCallOptimization::IsTailCall(std::unique_ptr<MidoriExpression>& expr, std::string_view function_name)
{
	if (!expr->IsExpression<MidoriExpression::Call>())
	{
		return false;
	}

	MidoriExpression::Call& call = expr->GetExpression<MidoriExpression::Call>();
	const MidoriExpression* callee_expr = OptimizerAnalysis::StripRedundantGroups(call.m_callee.get());
	if (callee_expr && callee_expr->IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& callee_name = callee_expr->GetExpression<MidoriExpression::NameAccess>();
		if (callee_name.m_name.m_lexeme == function_name)
		{
			if (!call.m_is_tail_call)
			{
				call.m_is_tail_call = true;
				m_marked_new_tail_call = true;
			}
			return true;
		}
	}
	return false;
}

bool TailCallOptimization::ContainsRecursiveCall(std::unique_ptr<MidoriExpression>& expr, std::string_view function_name)
{
	return ContainsRecursiveCallImpl(*expr, function_name);
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

		if (ContainsRecursiveCallImpl(*if_else.m_condition, function_name))
		{
			return false;
		}

		bool then_has_call = ContainsRecursiveCallImpl(*if_else.m_true_branch, function_name);
		bool else_has_call = ContainsRecursiveCallImpl(*if_else.m_else_branch, function_name);
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
		bool has_tail_call = false;

		for (std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
		{
			if (stmt->IsStatement<MidoriStatement::ExpressionStatement>())
			{
				std::unique_ptr<MidoriExpression>& stmt_expr = stmt->GetStatement<MidoriStatement::ExpressionStatement>().m_expr;
				if (stmt_expr->IsExpression<MidoriExpression::Return>())
				{
					bool stmt_has_call = ContainsRecursiveCallImpl(*stmt_expr, function_name);
					bool stmt_is_tail = IsTailRecursive(stmt_expr, function_name);
					if (stmt_has_call && !stmt_is_tail)
					{
						return false;
					}
					if (stmt_is_tail)
					{
						has_tail_call = true;
					}
				}
				else if (ContainsRecursiveCallImpl(*stmt_expr, function_name))
				{
					return false;
				}
			}
			else if (stmt->IsStatement<MidoriStatement::VariableDefinition>())
			{
				if (ContainsRecursiveCallImpl(*stmt->GetStatement<MidoriStatement::VariableDefinition>().m_value, function_name))
				{
					return false;
				}
			}
			else if (stmt->IsStatement<MidoriStatement::TupleDefinition>())
			{
				if (ContainsRecursiveCallImpl(*stmt->GetStatement<MidoriStatement::TupleDefinition>().m_value, function_name))
				{
					return false;
				}
			}
		}

		if (block.m_final_expr.has_value())
		{
			bool final_has_call = ContainsRecursiveCallImpl(*block.m_final_expr.value(), function_name);
			bool final_is_tail = IsTailRecursive(block.m_final_expr.value(), function_name);
			if (final_has_call && !final_is_tail)
			{
				return false;
			}
			return has_tail_call || final_is_tail;
		}

		return has_tail_call;
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

		if (ContainsRecursiveCallImpl(*match_expr.m_arg_expr, function_name))
		{
			return false;
		}

		bool has_tail_call = false;
		for (std::unique_ptr<MidoriExpression>& case_expr : match_expr.m_cases)
		{
			bool case_has_call = ContainsRecursiveCallImpl(*case_expr, function_name);
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
