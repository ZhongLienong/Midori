#pragma once

#include "Compiler/OptimizerManager/Analysis/OptimizerAnalysis.h"
#include "Compiler/OptimizerManager/Optimizers/BaseOptimizer/BaseOptimizer.h"

class DeadCodeElimination final : public MidoriOptimizer
{
public:
	MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) override;

	std::string_view GetName() const override;

private:
	void ProcessTopLevelStatements(std::vector<std::unique_ptr<MidoriStatement>>& statements);

	void RemovePureExpressionStatements(std::vector<std::unique_ptr<MidoriStatement>>& statements);

	void TrimUnreachableBlockTail(MidoriExpression::Block& block);

	void ElideUnusedPureLocalDefinitions(MidoriExpression::Block& block);

	static bool IsTerminatingStatement(const MidoriStatement& statement);

	static bool HasNestedCallableBoundaryAfter(const OptimizerAnalysis::BlockLocalAccessSummary& access_summary, std::size_t statement_index);

	void operator()(MidoriStatement::FunctionDefinition& defun) override;

	void operator()(MidoriExpression::Block& block) override;
};
