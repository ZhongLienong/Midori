#pragma once

#include "Compiler/Result/Result.h"
#include "Compiler/OptimizerManager/Optimizers/ConstantFolding/ConstantFolding.h"

class MidoriOptimizer;

class OptimizerManager
{
private:
	MidoriProgramTree m_program_tree;
	std::vector<std::unique_ptr<MidoriOptimizer>> m_optimizers;

#ifdef DEBUG
	struct OptimizerStats
	{
		std::string_view m_name;
		int m_optimizations_performed;
		int m_passes_run;
	};
	std::vector<OptimizerStats> m_stats;
#endif

public:
	OptimizerManager(MidoriProgramTree&& program_tree);

	void AddOptimizer(std::unique_ptr<MidoriOptimizer> optimizer);

	MidoriResult::OptimizerResult Optimize();

#ifdef DEBUG
	void PrintStatistics() const;
#endif
};