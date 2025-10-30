#include "OptimizerManager.h"
#include "Optimizers/BaseOptimizer/BaseOptimizer.h"
#include "Compiler/OptimizerManager/Optimizers/ConstantFolding/ConstantFolding.h"
#include "Compiler/OptimizerManager/Optimizers/StrengthReduction/StrengthReduction.h"
#include "Compiler/OptimizerManager/Optimizers/TailCallOptimization/TailCallOptimization.h"

#ifdef DEBUG
#include "Common/Printer/Printer.h"
#include <format>
#endif

OptimizerManager::OptimizerManager(MidoriProgramTree&& program_tree)
	: m_program_tree(std::move(program_tree))
{
	AddOptimizer(std::make_unique<ConstantFolding>());
	AddOptimizer(std::make_unique<StrengthReduction>());
	AddOptimizer(std::make_unique<TailCallOptimization>());
}

void OptimizerManager::AddOptimizer(std::unique_ptr<MidoriOptimizer> optimizer)
{
	m_optimizers.emplace_back(std::move(optimizer));

#ifdef DEBUG
	m_stats.emplace_back("", 0, 0);
#endif
}

MidoriResult::OptimizerResult OptimizerManager::Optimize()
{
#ifdef DEBUG
	Printer::Print<Printer::Color::CYAN>("\n=== Optimization Pass ===\n");
#endif

	// Run each optimizer once (for now - later we can add fixpoint iteration)
	for (size_t i = 0u; i < m_optimizers.size(); i += 1u)
	{
		std::unique_ptr<MidoriOptimizer>& optimizer = m_optimizers[i];

#ifdef DEBUG
		std::string_view name = optimizer->GetName();
		m_stats[i].m_name = name;
		m_stats[i].m_passes_run++;
#endif

		int optimizations = optimizer->Optimize(m_program_tree);

#ifdef DEBUG
		m_stats[i].m_optimizations_performed += optimizations;
		if (optimizations > 0)
		{
			Printer::Print<Printer::Color::MAGENTA>(std::format("  {}: {} optimizations\n", name, optimizations));
		}
#endif
	}

#ifdef DEBUG
	Printer::Print<Printer::Color::CYAN>("=========================\n\n");
#endif

	return std::move(m_program_tree);
}

#ifdef DEBUG
void OptimizerManager::PrintStatistics() const
{
	Printer::Print<Printer::Color::CYAN>("\n=== Optimization Statistics ===\n");

	int total_optimizations = 0;
	for (const OptimizerManager::OptimizerStats& stat : m_stats)
	{
		if (stat.m_optimizations_performed > 0)
		{
			Printer::Print<Printer::Color::GREEN>(std::format("  {:20}: {:4} optimizations ({} passes)\n", stat.m_name, stat.m_optimizations_performed, stat.m_passes_run));
			total_optimizations += stat.m_optimizations_performed;
		}
	}

	Printer::Print<Printer::Color::YELLOW>(std::format("  {:20}: {:4} total optimizations\n", "TOTAL", total_optimizations));
	Printer::Print<Printer::Color::CYAN>("================================\n\n");
}
#endif