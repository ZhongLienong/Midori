#include "ImportManager.h"
#include "Common/Error/Error.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/Token/Token.h"

#include <queue>
#include <filesystem>
#include <fstream>
#include <sstream>

using namespace std::string_literals;

ImportManager::ImportManager(TokenStream&& main_file_tokens, std::string&& main_file_name) :
    m_main_token_stream(std::move(main_file_tokens)),
    m_main_file_name(std::move(main_file_name))
{
}

MidoriResult::ImportManagerResult ImportManager::GenerateBuildGraph()
{
    BuildGraph build_graph;

    // Process the main file first
    if (m_main_token_stream.Size() != 0)
    {
        std::vector<std::pair<std::string, int>> import_paths;

        // Parse imports if they exist
        if (m_main_token_stream[0].m_token_name == Token::Name::IMPORT)
        {
            /*
                at least it should be
                import
                {
                    "file"
                }

                so at least 4 tokens
            */
            if (m_main_token_stream.Size() < 4)
            {
                return std::unexpected(MidoriError::GenerateImportManagerError("Invalid import block.", m_main_token_stream[0].m_line));
            }

            int current_index = 1; // Start after "Import"
            if (m_main_token_stream[current_index].m_token_name != Token::Name::LEFT_BRACE)
            {
                return std::unexpected(MidoriError::GenerateImportManagerError("Expected '{' after 'Import'.", m_main_token_stream[current_index].m_line));
            }
            current_index += 1;

            while (current_index < m_main_token_stream.Size() && m_main_token_stream[current_index].m_token_name != Token::Name::RIGHT_BRACE)
            {
                if (m_main_token_stream[current_index].m_token_name != Token::Name::TEXT_LITERAL)
                {
                    return std::unexpected(MidoriError::GenerateImportManagerError("Expected text literal for import path.", m_main_token_stream[current_index].m_line));
                }

                import_paths.emplace_back(m_main_token_stream[current_index].m_lexeme, m_main_token_stream[current_index].m_line);
                current_index += 1;

                if (current_index < m_main_token_stream.Size() && m_main_token_stream[current_index].m_token_name == Token::Name::COMMA)
                {
                    current_index += 1;
                }
            }

            if (current_index >= m_main_token_stream.Size() || m_main_token_stream[current_index].m_token_name != Token::Name::RIGHT_BRACE)
            {
                return std::unexpected(MidoriError::GenerateImportManagerError("Expected '}' after import paths.", m_main_token_stream[current_index].m_line));
            }

            current_index += 1;
            m_main_token_stream.Erase(m_main_token_stream.begin() + current_index);
        }

        BuildGraph::BuildNode& main_node = build_graph.m_nodes[m_main_file_name];
        main_node.m_tokens = m_main_token_stream;
        main_node.m_file_name = m_main_file_name;

        for (const auto& [import_path, line] : import_paths)
        {
            std::string include_absolute_path_str = std::filesystem::absolute(import_path).string();

            m_dependency_graph[m_main_file_name].emplace_back(include_absolute_path_str);

            if (build_graph.m_nodes.contains(include_absolute_path_str))
            {
                continue;
            }

            std::ifstream include_file(include_absolute_path_str);
            if (!include_file.is_open())
            {
                return std::unexpected(MidoriError::GenerateImportManagerError("Could not open import file: "s + import_path, line));
            }

            if (HasCircularDependency())
            {
                return std::unexpected(MidoriError::GenerateImportManagerError("Circular dependency detected: "s + import_path, line));
            }

            std::ostringstream include_file_stream;
            include_file_stream << include_file.rdbuf();

            const bool is_main_program = false;
            MidoriResult::LexerResult lex_result = Lexer(include_file_stream.str(), is_main_program).Lex();
            if (!lex_result.has_value())
            {
                return std::unexpected(MidoriError::GenerateImportManagerError(lex_result.error(), line));
            }

            TokenStream imported_token_stream = std::move(lex_result.value());

            ImportManager import_manager(std::move(imported_token_stream), std::move(include_absolute_path_str));
            MidoriResult::ImportManagerResult nested_build_graph_result = import_manager.GenerateBuildGraph();
            if (!nested_build_graph_result.has_value())
            {
                return std::unexpected(MidoriError::GenerateImportManagerError(nested_build_graph_result.error(), line));
            }

            BuildGraph& nested_build_graph = nested_build_graph_result.value();
            for (const auto& [file_name, node] : nested_build_graph.m_nodes)
            {
                if (!build_graph.m_nodes.contains(file_name))
                {
                    build_graph.m_nodes[file_name] = node;
                }
            }

            for (const auto& [src, dependencies] : import_manager.m_dependency_graph)
            {
                for (const std::string& dependency : dependencies)
                {
                    if (!std::ranges::contains(m_dependency_graph[src], dependency))
                    {
                        m_dependency_graph[src].push_back(dependency);
                    }
                }
            }
        }
    }

    BuildDependencyGraph(build_graph);

    CalculateInDegrees(build_graph);

    if (HasCircularDependency())
    {
        return std::unexpected(MidoriError::GenerateImportManagerError("Circular dependency detected in final build graph", 0));
    }

    return build_graph;
}

bool ImportManager::HasCircularDependency() const
{
    std::unordered_set<std::string> visited;
    std::unordered_set<std::string> in_progress;
    std::queue<std::string> queue;

    queue.emplace(m_main_file_name);
    in_progress.emplace(m_main_file_name);

    while (!queue.empty())
    {
        std::string current = queue.front();
        queue.pop();
        in_progress.erase(current);
        visited.emplace(current);

        if (m_dependency_graph.contains(current))
        {
            for (const std::string& dependency : m_dependency_graph.at(current))
            {
                if (visited.contains(dependency))
                {
                    continue;
                }
                if (in_progress.contains(dependency))
                {
                    return true; // Cycle detected
                }
                queue.emplace(dependency);
                in_progress.insert(dependency);
            }
        }
    }

    return false;
}

void ImportManager::BuildDependencyGraph(BuildGraph& build_graph)
{
    // For each file in the dependency graph, update the BuildNode dependencies
    for (const auto& [src, dependencies] : m_dependency_graph)
    {
        if (build_graph.m_nodes.contains(src))
        {
            BuildGraph::BuildNode& node = build_graph.m_nodes[src];
            for (const std::string& dependency : dependencies)
            {
                if (build_graph.m_nodes.contains(dependency) && !std::ranges::contains(node.m_dependencies, dependency))
                {
                    node.m_dependencies.emplace_back(dependency);
                }
            }
        }
    }
}

void ImportManager::CalculateInDegrees(BuildGraph& build_graph)
{
    // Reset all in-degrees first
    for (auto& [file, node] : build_graph.m_nodes)
    {
        node.m_in_degree = 0;
    }

    // For each file, increment the in-degree of its dependencies
    for (const auto& [file, node] : build_graph.m_nodes)
    {
        for (const std::string& dependency : node.m_dependencies)
        {
            if (build_graph.m_nodes.contains(dependency))
            {
                build_graph.m_nodes.at(dependency).m_in_degree += 1;
            }
        }
    }
}