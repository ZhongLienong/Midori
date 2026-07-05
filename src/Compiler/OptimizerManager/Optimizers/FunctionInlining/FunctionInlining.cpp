#include "FunctionInlining.h"

#include <algorithm>

MidoriResult::OptimizerResult FunctionInlining::Optimize(MidoriProgramTree program_tree)
{
	ResetPassState();
	m_candidates.clear();
	CollectCandidates(program_tree);

	if (!m_candidates.empty())
	{
		std::ranges::for_each
		(
			program_tree,
			[this](std::unique_ptr<MidoriStatement>& stmt)
			{
				VisitStatement(stmt);
			}
		);
	}

	return std::move(program_tree);
}

std::string_view FunctionInlining::GetName() const
{
	return "FunctionInlining";
}

void FunctionInlining::CollectCandidates(const MidoriProgramTree& program_tree)
{
	for (const std::unique_ptr<MidoriStatement>& stmt : program_tree)
	{
		if (!stmt->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			continue;
		}

		const MidoriStatement::FunctionDefinition& defun = stmt->GetStatement<MidoriStatement::FunctionDefinition>();
		if (!defun.m_generic_params.empty()
			|| !defun.m_constraints.empty()
			|| defun.m_captured_count != 0
			|| defun.m_is_lift_wrapper
			|| defun.m_body == nullptr)
		{
			continue;
		}

		const MidoriExpression* body = UnwrapReturn(*defun.m_body);
		int node_budget = s_max_body_nodes;
		const int arity = static_cast<int>(defun.m_params.size());
		InlineCandidate candidate;
		candidate.m_body = body;
		candidate.m_arity = arity;
		candidate.m_param_use_counts.assign(static_cast<size_t>(arity), 0);
		candidate.m_param_use_conditional.assign(static_cast<size_t>(arity), false);
		if (!IsInlinableBody(*body, candidate, node_budget, false))
		{
			continue;
		}

		m_candidates[defun.m_name.m_lexeme] = std::move(candidate);
	}
}

const MidoriExpression* FunctionInlining::UnwrapReturn(const MidoriExpression& body)
{
	if (body.IsExpression<MidoriExpression::Return>())
	{
		return body.GetExpression<MidoriExpression::Return>().m_value.get();
	}

	return &body;
}

bool FunctionInlining::IsInlinableBody(const MidoriExpression& expr, InlineCandidate& candidate, int& node_budget, bool conditional)
{
	node_budget -= 1;
	if (node_budget < 0)
	{
		return false;
	}

	if (expr.IsExpression<MidoriExpression::IntegerLiteral>()
		|| expr.IsExpression<MidoriExpression::FloatLiteral>()
		|| expr.IsExpression<MidoriExpression::BoolLiteral>()
		|| expr.IsExpression<MidoriExpression::ByteLiteral>()
		|| expr.IsExpression<MidoriExpression::WordLiteral>()
		|| expr.IsExpression<MidoriExpression::UnitLiteral>()
		|| expr.IsExpression<MidoriExpression::TextLiteral>())
	{
		return true;
	}

	if (expr.IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& access = expr.GetExpression<MidoriExpression::NameAccess>();
		if (const MidoriExpression::NameContext::Local* local = std::get_if<MidoriExpression::NameContext::Local>(&access.m_name_ctx))
		{
			if (local->m_index < 0 || local->m_index >= candidate.m_arity)
			{
				return false;
			}

			const size_t param = static_cast<size_t>(local->m_index);
			candidate.m_param_use_counts[param] += 1;
			if (conditional)
			{
				candidate.m_param_use_conditional[param] = true;
			}
			return true;
		}

		return std::holds_alternative<MidoriExpression::NameContext::Global>(access.m_name_ctx);
	}

	if (expr.IsExpression<MidoriExpression::Binary>())
	{
		const MidoriExpression::Binary& binary = expr.GetExpression<MidoriExpression::Binary>();
		if (binary.m_uses_concatenable || binary.m_uses_equatable || binary.m_uses_orderable)
		{
			return false;
		}

		return IsInlinableBody(*binary.m_left, candidate, node_budget, conditional) && IsInlinableBody(*binary.m_right, candidate, node_budget, conditional);
	}

	if (expr.IsExpression<MidoriExpression::Group>())
	{
		return IsInlinableBody(*expr.GetExpression<MidoriExpression::Group>().m_expr_in, candidate, node_budget, conditional);
	}

	if (expr.IsExpression<MidoriExpression::UnaryPrefix>())
	{
		const MidoriExpression::UnaryPrefix& unary = expr.GetExpression<MidoriExpression::UnaryPrefix>();
		if (unary.m_uses_countable)
		{
			return false;
		}

		return IsInlinableBody(*unary.m_expr, candidate, node_budget, conditional);
	}

	if (expr.IsExpression<MidoriExpression::As>())
	{
		const MidoriExpression::As& as = expr.GetExpression<MidoriExpression::As>();
		if (as.m_uses_convertable)
		{
			return false;
		}

		return IsInlinableBody(*as.m_expr, candidate, node_budget, conditional);
	}

	if (expr.IsExpression<MidoriExpression::IfElse>())
	{
		const MidoriExpression::IfElse& if_else = expr.GetExpression<MidoriExpression::IfElse>();
		if (if_else.m_condition == nullptr || if_else.m_true_branch == nullptr || if_else.m_else_branch == nullptr)
		{
			return false;
		}

		return IsInlinableBody(*if_else.m_condition, candidate, node_budget, conditional)
			&& IsInlinableBody(*if_else.m_true_branch, candidate, node_budget, true)
			&& IsInlinableBody(*if_else.m_else_branch, candidate, node_budget, true);
	}

	return false;
}

bool FunctionInlining::IsSimpleArgument(const MidoriExpression& expr)
{
	return expr.IsExpression<MidoriExpression::IntegerLiteral>()
		|| expr.IsExpression<MidoriExpression::FloatLiteral>()
		|| expr.IsExpression<MidoriExpression::BoolLiteral>()
		|| expr.IsExpression<MidoriExpression::ByteLiteral>()
		|| expr.IsExpression<MidoriExpression::WordLiteral>()
		|| expr.IsExpression<MidoriExpression::UnitLiteral>()
		|| expr.IsExpression<MidoriExpression::TextLiteral>()
		|| expr.IsExpression<MidoriExpression::NameAccess>();
}

bool FunctionInlining::IsSubstitutablePureArgument(const MidoriExpression& expr)
{
	if (IsSimpleArgument(expr))
	{
		return true;
	}

	if (expr.IsExpression<MidoriExpression::Binary>())
	{
		const MidoriExpression::Binary& binary = expr.GetExpression<MidoriExpression::Binary>();
		if (binary.m_uses_concatenable || binary.m_uses_equatable || binary.m_uses_orderable)
		{
			return false;
		}

		// Division and modulo can fault; substitution may reorder evaluation
		// relative to other arguments, so keep faulting operators out.
		if (binary.m_op.m_token_name == Token::Name::SLASH || binary.m_op.m_token_name == Token::Name::PERCENT)
		{
			return false;
		}

		return IsSubstitutablePureArgument(*binary.m_left) && IsSubstitutablePureArgument(*binary.m_right);
	}

	if (expr.IsExpression<MidoriExpression::Group>())
	{
		return IsSubstitutablePureArgument(*expr.GetExpression<MidoriExpression::Group>().m_expr_in);
	}

	if (expr.IsExpression<MidoriExpression::UnaryPrefix>())
	{
		const MidoriExpression::UnaryPrefix& unary = expr.GetExpression<MidoriExpression::UnaryPrefix>();
		return !unary.m_uses_countable && IsSubstitutablePureArgument(*unary.m_expr);
	}

	return false;
}

std::unique_ptr<MidoriExpression> FunctionInlining::CloneSimple(const MidoriExpression& expr)
{
	std::unique_ptr<MidoriExpression> clone = nullptr;
	if (expr.IsExpression<MidoriExpression::IntegerLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(expr.GetExpression<MidoriExpression::IntegerLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::FloatLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(expr.GetExpression<MidoriExpression::FloatLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::BoolLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(expr.GetExpression<MidoriExpression::BoolLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::ByteLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::ByteLiteral(expr.GetExpression<MidoriExpression::ByteLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::WordLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::WordLiteral(expr.GetExpression<MidoriExpression::WordLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::UnitLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::UnitLiteral(expr.GetExpression<MidoriExpression::UnitLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::TextLiteral>())
	{
		clone = std::make_unique<MidoriExpression>(MidoriExpression::TextLiteral(expr.GetExpression<MidoriExpression::TextLiteral>().m_token));
	}
	else if (expr.IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& access = expr.GetExpression<MidoriExpression::NameAccess>();
		clone = std::make_unique<MidoriExpression>(MidoriExpression::NameAccess(access.m_name, MidoriExpression::NameContext::Tag(access.m_name_ctx)));
	}

	if (clone != nullptr)
	{
		clone->GetType() = expr.GetType();
	}
	return clone;
}

std::unique_ptr<MidoriExpression> FunctionInlining::CloneWithSubstitution(const MidoriExpression& expr, const std::vector<std::unique_ptr<MidoriExpression>>* arguments)
{
	if (expr.IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& access = expr.GetExpression<MidoriExpression::NameAccess>();
		const MidoriExpression::NameContext::Local* local = std::get_if<MidoriExpression::NameContext::Local>(&access.m_name_ctx);
		if (arguments != nullptr && local != nullptr)
		{
			const MidoriExpression& argument = *(*arguments)[static_cast<size_t>(local->m_index)];
			return IsSimpleArgument(argument) ? CloneSimple(argument) : CloneWithSubstitution(argument, nullptr);
		}

		return CloneSimple(expr);
	}

	if (expr.IsExpression<MidoriExpression::Binary>())
	{
		const MidoriExpression::Binary& binary = expr.GetExpression<MidoriExpression::Binary>();
		std::unique_ptr<MidoriExpression> left = CloneWithSubstitution(*binary.m_left, arguments);
		std::unique_ptr<MidoriExpression> right = CloneWithSubstitution(*binary.m_right, arguments);
		if (left == nullptr || right == nullptr)
		{
			return nullptr;
		}

		MidoriExpression::Binary copy(binary.m_op, std::move(left), std::move(right));
		copy.m_uses_concatenable = binary.m_uses_concatenable;
		copy.m_uses_equatable = binary.m_uses_equatable;
		copy.m_uses_orderable = binary.m_uses_orderable;
		copy.m_type_data = binary.m_type_data;
		return std::make_unique<MidoriExpression>(std::move(copy));
	}

	if (expr.IsExpression<MidoriExpression::Group>())
	{
		std::unique_ptr<MidoriExpression> inner = CloneWithSubstitution(*expr.GetExpression<MidoriExpression::Group>().m_expr_in, arguments);
		if (inner == nullptr)
		{
			return nullptr;
		}

		MidoriExpression::Group copy(std::move(inner));
		copy.m_type_data = expr.GetType();
		return std::make_unique<MidoriExpression>(std::move(copy));
	}

	if (expr.IsExpression<MidoriExpression::UnaryPrefix>())
	{
		const MidoriExpression::UnaryPrefix& unary = expr.GetExpression<MidoriExpression::UnaryPrefix>();
		std::unique_ptr<MidoriExpression> inner = CloneWithSubstitution(*unary.m_expr, arguments);
		if (inner == nullptr)
		{
			return nullptr;
		}

		MidoriExpression::UnaryPrefix copy(unary.m_op, std::move(inner));
		copy.m_uses_countable = unary.m_uses_countable;
		copy.m_type_data = unary.m_type_data;
		return std::make_unique<MidoriExpression>(std::move(copy));
	}

	if (expr.IsExpression<MidoriExpression::As>())
	{
		const MidoriExpression::As& as = expr.GetExpression<MidoriExpression::As>();
		std::unique_ptr<MidoriExpression> inner = CloneWithSubstitution(*as.m_expr, arguments);
		if (inner == nullptr)
		{
			return nullptr;
		}

		MidoriExpression::As copy(as.m_as_keyword, std::shared_ptr<MidoriType>(as.m_to_type), std::move(inner));
		copy.m_from_type = as.m_from_type;
		copy.m_uses_convertable = as.m_uses_convertable;
		copy.m_type_data = as.m_type_data;
		return std::make_unique<MidoriExpression>(std::move(copy));
	}

	if (expr.IsExpression<MidoriExpression::IfElse>())
	{
		const MidoriExpression::IfElse& if_else = expr.GetExpression<MidoriExpression::IfElse>();
		std::unique_ptr<MidoriExpression> condition = CloneWithSubstitution(*if_else.m_condition, arguments);
		std::unique_ptr<MidoriExpression> true_branch = CloneWithSubstitution(*if_else.m_true_branch, arguments);
		std::unique_ptr<MidoriExpression> else_branch = CloneWithSubstitution(*if_else.m_else_branch, arguments);
		if (condition == nullptr || true_branch == nullptr || else_branch == nullptr)
		{
			return nullptr;
		}

		MidoriExpression::IfElse copy(if_else.m_if_token, if_else.m_then_token, if_else.m_else_token, std::move(condition), std::move(true_branch), std::move(else_branch), if_else.m_condition_operand_type);
		copy.m_type_data = if_else.m_type_data;
		return std::make_unique<MidoriExpression>(std::move(copy));
	}

	return CloneSimple(expr);
}

void FunctionInlining::operator()(MidoriExpression::Call& call)
{
	VisitAndReplace(call.m_callee);
	std::ranges::for_each
	(
		call.m_arguments,
		[this](std::unique_ptr<MidoriExpression>& argument)
		{
			VisitAndReplace(argument);
		}
	);

	if (call.m_is_foreign || !call.m_callee->IsExpression<MidoriExpression::NameAccess>())
	{
		return;
	}

	const MidoriExpression::NameAccess& callee = call.m_callee->GetExpression<MidoriExpression::NameAccess>();
	if (!std::holds_alternative<MidoriExpression::NameContext::Global>(callee.m_name_ctx))
	{
		return;
	}

	std::unordered_map<std::string, InlineCandidate>::iterator candidate_it = m_candidates.find(callee.m_name.m_lexeme);
	if (candidate_it == m_candidates.end())
	{
		return;
	}

	const InlineCandidate& candidate = candidate_it->second;
	if (candidate.m_arity != static_cast<int>(call.m_arguments.size()))
	{
		return;
	}

	for (size_t param = 0u; param < call.m_arguments.size(); param += 1u)
	{
		const MidoriExpression& argument = *call.m_arguments[param];
		if (IsSimpleArgument(argument))
		{
			continue;
		}

		// A non-trivial argument may only be substituted where the body reads
		// the parameter exactly once and never behind a branch.
		if (candidate.m_param_use_counts[param] == 1
			&& !candidate.m_param_use_conditional[param]
			&& IsSubstitutablePureArgument(argument))
		{
			continue;
		}

		return;
	}

	std::unique_ptr<MidoriExpression> inlined = CloneWithSubstitution(*candidate.m_body, &call.m_arguments);
	if (inlined == nullptr)
	{
		return;
	}

	m_pending_replacement = std::move(inlined);
	MarkOptimization();
}
