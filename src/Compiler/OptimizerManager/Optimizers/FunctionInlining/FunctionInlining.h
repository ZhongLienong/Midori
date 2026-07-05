#pragma once

#include "Compiler/OptimizerManager/Optimizers/BaseOptimizer/BaseOptimizer.h"

#include <unordered_map>
#include <vector>

// Inlines calls to small, pure, same-module functions. A candidate body may
// contain only literals, parameter/global reads, arithmetic, groups, casts,
// and if-else - no calls, no local definitions, no assignments - so inlining
// is direct parameter substitution with no local-index renumbering. Each
// argument must be a literal or name access (safe to re-read), or - when the
// body reads that parameter exactly once and never behind a branch - any
// pure, fault-free expression.
class FunctionInlining final : public MidoriOptimizer
{
public:
	MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) override;

	std::string_view GetName() const override;

private:
	struct InlineCandidate
	{
		const MidoriExpression* m_body = nullptr;
		int m_arity = 0;
		// Per parameter: how often the body reads it, and whether any read sits
		// inside a conditional branch (which would skip a substituted argument).
		std::vector<int> m_param_use_counts;
		std::vector<bool> m_param_use_conditional;
	};

	static constexpr int s_max_body_nodes = 16;

	std::unordered_map<std::string, InlineCandidate> m_candidates;

	void CollectCandidates(const MidoriProgramTree& program_tree);

	static const MidoriExpression* UnwrapReturn(const MidoriExpression& body);

	static bool IsInlinableBody(const MidoriExpression& expr, InlineCandidate& candidate, int& node_budget, bool conditional);

	static bool IsSimpleArgument(const MidoriExpression& expr);

	static bool IsSubstitutablePureArgument(const MidoriExpression& expr);

	static std::unique_ptr<MidoriExpression> CloneSimple(const MidoriExpression& expr);

	static std::unique_ptr<MidoriExpression> CloneWithSubstitution(const MidoriExpression& expr, const std::vector<std::unique_ptr<MidoriExpression>>* arguments);

	void operator()(MidoriExpression::Call& call) override;
};
