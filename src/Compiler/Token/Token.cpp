#include "Token.h"
#include "Common/Printer/Printer.h"

#include <queue>
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

TokenStream BuildGraph::Stitch()
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
                dependents[dependency].emplace_back (file);
                in_degrees[file]++;
            }
        }
    }

#ifdef DEBUG
    Printer::Print("Build Graph:\n");
    for (const auto& [file, degree] : in_degrees) 
    {
        Printer::Print(std::format("File: {} has in-degree: {}", file, degree));
        Printer::Print("  Has dependents: ");
        for (const std::string& dependent : dependents[file]) 
        {
            Printer::Print<Printer::Color::YELLOW>(std::format("{}, ", dependent));
        }
        Printer::Print("\n");
    }
#endif

    // Find nodes with zero in-degree (no dependencies)
    std::queue<std::string> zero_in_degree;
    for (const auto& [file, degree] : in_degrees) 
    {
        if (degree == 0) 
        {
            zero_in_degree.emplace(file);
        }
    }

    TokenStream stitched_stream;
    std::vector<std::string> topological_order;
    int processed_count = 0;

    while (!zero_in_degree.empty()) 
    {
        std::string current = zero_in_degree.front();
        zero_in_degree.pop();

        topological_order.push_back(current);
        processed_count++;

        for (const std::string& dependent : dependents[current]) 
        {
            in_degrees[dependent]--;

            if (in_degrees[dependent] == 0) 
            {
                zero_in_degree.push(dependent);
            }
        }
    }

#ifdef DEBUG
    Printer::Print(std::format("Processed {} nodes out of {}\n", processed_count, m_nodes.size()));
    Printer::Print("Final topological order: ");
    for (size_t idx : std::views::iota(0u, topological_order.size()))
    {
        if (idx != topological_order.size() - 1u)
        {
            Printer::Print<Printer::Color::YELLOW>(std::format("{} -> ", topological_order[idx]));
        }
        else
        {
            Printer::Print<Printer::Color::YELLOW>(std::format("{}", topological_order[idx]));
        }
    }
    Printer::Print("\n");
#endif

    for (const auto& file : topological_order) 
    {
        TokenStream node_tokens = m_nodes.at(file).m_tokens;
        stitched_stream.Insert(stitched_stream.end(), std::move(node_tokens));
    }

    return stitched_stream;
}