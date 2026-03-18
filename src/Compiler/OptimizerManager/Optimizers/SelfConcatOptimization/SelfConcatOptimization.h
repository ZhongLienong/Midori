#pragma once

#include "Compiler/OptimizerManager/Optimizers/BaseOptimizer/BaseOptimizer.h"

#include <optional>
#include <unordered_map>
#include <vector>

class SelfConcatOptimization final : public MidoriOptimizer
{
public:
	MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) override;

	std::string_view GetName() const override;

protected:
	void operator()(MidoriStatement::ExpressionStatement& simple) override;
	void operator()(MidoriStatement::VariableDefinition& def) override;
	void operator()(MidoriStatement::TupleDefinition& def_tuple) override;
	void operator()(MidoriStatement::FunctionDefinition& defun) override;
	void operator()(MidoriExpression::Block& block) override;
	void operator()(MidoriExpression::Function& function) override;
	void operator()(MidoriExpression::Return& return_expr) override;
	void operator()(MidoriExpression::Break& break_expr) override;

private:
	enum class RootContext
	{
		Discarded,
		Escaping,
		Terminal,
	};

	struct UniqueLocalInfo
	{
		Token m_name;
		std::shared_ptr<MidoriType> m_type;
	};

	struct RootRewriteEffect
	{
		std::optional<int> m_preserved_local = std::nullopt;
		std::optional<int> m_refreshed_local = std::nullopt;
		std::optional<UniqueLocalInfo> m_refreshed_info = std::nullopt;
	};

	void PushFunctionState();
	void PopFunctionState();
	void PushLocalScope();
	void PopLocalScope();

	std::unordered_map<int, UniqueLocalInfo>& CurrentUniqueLocals();
	const std::unordered_map<int, UniqueLocalInfo>& CurrentUniqueLocals() const;

	void OptimizeRootExpression(std::unique_ptr<MidoriExpression>& expr, RootContext context);
	RootRewriteEffect TryRewriteRoot(std::unique_ptr<MidoriExpression>& expr, RootContext context);
	RootRewriteEffect AnalyzeRootEffects(const MidoriExpression& expr, RootContext context) const;
	void ApplyExpressionEffects(const MidoriExpression& expr, const RootRewriteEffect& effect);
	void TrackFreshDefinition(const MidoriStatement::VariableDefinition& def);
	void RefreshUniqueLocal(int local_index, const Token& name, const std::shared_ptr<MidoriType>& type);
	void InvalidateUniqueLocal(int local_index);
	void InvalidateAllUniqueLocals();

	std::vector<std::unordered_map<int, UniqueLocalInfo>> m_unique_locals_stack;
	std::vector<std::vector<int>> m_local_scope_stack;
};
