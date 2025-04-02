#pragma once

#include "Compiler/Result/Result.h"
#include "Compiler/Token/Token.h"

#include <unordered_map>
#include <vector>
#include <string>

class ImportManager
{
private:
    using DependencyGraph = std::unordered_map<std::string, std::vector<std::string>>;
    DependencyGraph m_dependency_graph;
    TokenStream m_main_token_stream;
    std::string m_main_file_name;

public:

    ImportManager(TokenStream&& main_file_tokens, std::string&& main_file_name);

    MidoriResult::ImportManagerResult GenerateBuildGraph();

private:
    bool HasCircularDependency() const;

    void BuildDependencyGraph(BuildGraph& build_graph);

    void CalculateInDegrees(BuildGraph& build_graph);
};