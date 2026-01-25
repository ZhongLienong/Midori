#include "BuildGraph.h"
#include <queue>
#include <map>
#include <algorithm>

std::vector<std::string> BuildGraph::GetStartingPoints() const
{
	std::vector<std::string> starting_points;
	for (const auto& [file, node] : m_nodes)
	{
		if (node.m_in_degree == 0 && !node.m_processed)
		{
			starting_points.emplace_back(file);
		}
	}
	return starting_points;
}

void BuildGraph::MarkProcessed(const std::string& file_name)
{
	BuildGraph::BuildNode& node = m_nodes[file_name];
	node.m_processed = true;

	for (const std::string& dependency : node.m_dependencies)
	{
		m_nodes[dependency].m_in_degree -= 1;
	}
}

bool BuildGraph::IsComplete() const
{
	for (const auto& [file, node] : m_nodes)
	{
		if (!node.m_processed)
		{
			return false;
		}
	}
	return true;
}

std::vector<std::vector<std::string>> BuildGraph::GetCompilationTiers() const
{
	std::unordered_map<std::string, std::vector<std::string>> dependents;
	std::unordered_map<std::string, int> in_degrees;

	for (const auto& [file, _] : m_nodes)
	{
		dependents[file] = {};
		in_degrees[file] = 0;
	}

	for (const auto& [file, node] : m_nodes)
	{
		for (const std::string& dependency : node.m_dependencies)
		{
			if (m_nodes.contains(dependency))
			{
				dependents[dependency].emplace_back(file);
				in_degrees[file]++;
			}
		}
	}

	// Compute dependency levels (tier number)
	std::unordered_map<std::string, int> levels;
	std::queue<std::string> zero_in_degree;

	for (const auto& [file, degree] : in_degrees)
	{
		if (degree == 0)
		{
			zero_in_degree.push(file);
			levels[file] = 0;
		}
	}

	std::unordered_map<std::string, int> temp_in_degrees = in_degrees;

	while (!zero_in_degree.empty())
	{
		std::string current = zero_in_degree.front();
		zero_in_degree.pop();

		for (const std::string& dependent : dependents[current])
		{
			temp_in_degrees[dependent]--;

			// Update level of dependent (it must be at least one more than current)
			levels[dependent] = std::max(levels[dependent], levels[current] + 1);

			if (temp_in_degrees[dependent] == 0)
			{
				zero_in_degree.push(dependent);
			}
		}
	}

	std::map<int, std::vector<std::string>> groups;
	for (const auto& [path, level] : levels)
	{
		groups[level].emplace_back(path);
	}

	for (auto& [_, tier] : groups)
	{
		std::sort(tier.begin(), tier.end());
	}

	std::vector<std::vector<std::string>> tiers;
	tiers.reserve(groups.size());
	for (auto& [_, tier] : groups)
	{
		tiers.emplace_back(std::move(tier));
	}

	return tiers;
}
