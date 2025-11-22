#include "Token.h"
#include "Common/Printer/Printer.h"

#include <queue>
#include <map>
#include <ranges>

Token::Token(std::string&& lexeme, Name token_name, int line) noexcept : m_lexeme(std::move(lexeme)), m_token_name(token_name), m_line(line)
{
}

TokenStream::iterator TokenStream::begin()
{ 
	return m_tokens.begin(); 
}

TokenStream::iterator TokenStream::end()
{ 
	return m_tokens.end();
}

TokenStream::const_iterator TokenStream::cbegin() const
{ 
	return m_tokens.cbegin(); 
}

TokenStream::const_iterator TokenStream::cend() const
{ 
	return m_tokens.cend(); 
}

void TokenStream::AddToken(Token&& token) 
{ 
	m_tokens.emplace_back(std::move(token)); 
}

Token& TokenStream::operator[](int index) const 
{ 
	return const_cast<Token&>(m_tokens[static_cast<size_t>(index)]);
}

int TokenStream::Size() const 
{ 
	return static_cast<int>(m_tokens.size()); 
}

void TokenStream::Insert(TokenStream::iterator iter, TokenStream&& tokens)
{
	m_tokens.insert(iter, tokens.begin(), tokens.end());
}

void TokenStream::Erase(TokenStream::iterator iter)
{
	m_tokens.erase(m_tokens.begin(), iter);
}

void TokenStream::PopBack() noexcept
{
    m_tokens.pop_back();
}

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

std::vector<std::vector<std::string>> BuildGraph::GetCompilationStreams() const
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

	// Compute dependency levels (stream number)
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

	// Convert levels to streams (group by level)
	std::map<int, std::vector<std::string>> groups;
	for (const auto& [path, level] : levels)
	{
		groups[level].emplace_back(path);
	}

	// Sort each stream alphabetically for deterministic ordering
	for (auto& [_, stream] : groups)
	{
		std::sort(stream.begin(), stream.end());
	}

	std::vector<std::vector<std::string>> streams;
	streams.reserve(groups.size());
	for (auto& [_, stream] : groups)
	{
		streams.emplace_back(std::move(stream));
	}

	return streams;
}