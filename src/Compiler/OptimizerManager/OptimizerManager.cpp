#include "OptimizerManager.h"
#include "Optimizers/BaseOptimizer/BaseOptimizer.h"
#include "Compiler/OptimizerManager/Optimizers/ConstantFolding/ConstantFolding.h"
#include "Compiler/OptimizerManager/Optimizers/StrengthReduction/StrengthReduction.h"
#include "Compiler/OptimizerManager/Optimizers/SelfConcatOptimization/SelfConcatOptimization.h"
#include "Compiler/OptimizerManager/Optimizers/ClosureLifting/ClosureLifting.h"
#include "Compiler/OptimizerManager/Optimizers/TailCallOptimization/TailCallOptimization.h"
#include "Common/BuildConfig/BuildConfig.h"

#include <algorithm>
#include <numeric>

#if MIDORI_ENABLE_OPTIMIZER_STATS
#include "Common/Printer/Printer.h"
#include <format>
#include <iterator>
#include <optional>
#endif

OptimizerManager::OptimizerManager(MidoriProgramTree&& program_tree)
	: m_program_tree(std::move(program_tree))
{
	AddOptimizer(std::make_unique<ConstantFolding>());
	AddOptimizer(std::make_unique<StrengthReduction>());
	AddOptimizer(std::make_unique<SelfConcatOptimization>());
	AddOptimizer(std::make_unique<ClosureLifting>());
	AddOptimizer(std::make_unique<TailCallOptimization>());
}

void OptimizerManager::AddOptimizer(std::unique_ptr<MidoriOptimizer> optimizer)
{
	m_optimizers.emplace_back(std::move(optimizer));

#if MIDORI_ENABLE_OPTIMIZER_STATS
	m_stats.emplace_back("", 0, 0);
#endif
}

#if MIDORI_ENABLE_OPTIMIZER_STATS
MidoriResult::OptimizerResult OptimizerManager::Optimize(OptimizerLog* log, std::mutex* print_mutex)
{
	std::string optimization_body;
	optimization_body.reserve(m_optimizers.size() * 32u);

	MidoriResult::OptimizerResult result = std::move(m_program_tree);
	size_t optimizer_index = 0u;
	std::for_each
	(
		m_optimizers.begin(), 
		m_optimizers.end(),
		[this, &optimizer_index, &optimization_body, &result](const std::unique_ptr<MidoriOptimizer>& optimizer)
		{
			const size_t current_index = optimizer_index;
			optimizer_index += 1u;

			result = std::move(result)
				.and_then
				(
					[this, &optimization_body, &optimizer, current_index](MidoriProgramTree&& program_tree)
					{
						const std::string_view name = optimizer->GetName();
						OptimizerStats& stat = m_stats[current_index];
						stat.m_name = name;
						stat.m_passes_run += 1;

						MidoriResult::OptimizerResult updated = optimizer->Optimize(std::move(program_tree));
						const int optimizations = optimizer->GetOptimizationsPerformed();
						stat.m_optimizations_performed += optimizations;
						if (optimizations > 0)
						{
							std::format_to(std::back_inserter(optimization_body), "  {}: {} optimizations\n", name, optimizations);
						}

						return updated;
					}
				);
		}
	);

	const auto print_output = [&optimization_body]()
	{
		Printer::Print<Printer::Color::CYAN>("\n=== Optimization Pass ===\n");
		if (!optimization_body.empty())
		{
			Printer::Print<Printer::Color::MAGENTA>(optimization_body);
		}
		Printer::Print<Printer::Color::CYAN>("=========================\n\n");
	};

	std::optional<OptimizerLog*> log_target = log ? std::optional<OptimizerLog*>{ log } : std::nullopt;
	log_target
		.and_then
		(
			[&optimization_body](OptimizerLog* log_ptr) -> std::optional<bool>
			{
				log_ptr->m_enabled = true;
				log_ptr->m_body = std::move(optimization_body);
				return true;
			}
		)
		.or_else
		(
			[&print_mutex, &print_output]()
			{
				if (print_mutex)
				{
					std::lock_guard<std::mutex> lock(*print_mutex);
					print_output();
				}
				else
				{
					print_output();
				}
				return std::optional<bool>{ true };
			}
		);

	return std::move(result);
}
#else
MidoriResult::OptimizerResult OptimizerManager::Optimize()
{
	return std::accumulate
	(
		m_optimizers.begin(),
		m_optimizers.end(),
		MidoriResult::OptimizerResult{ std::move(m_program_tree) },
		[](MidoriResult::OptimizerResult result, const std::unique_ptr<MidoriOptimizer>& optimizer)
		{
			return std::move(result)
				.and_then
				(
					[&optimizer](MidoriProgramTree&& program_tree)
					{
						return optimizer->Optimize(std::move(program_tree));
					}
				);
		}
	);
}
#endif

#if MIDORI_ENABLE_OPTIMIZER_STATS
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
