#pragma once

#include "Compiler/Result/Result.h"
#include "Compiler/Token/Token.h"
#include "Compiler/Module/Module.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <filesystem>

class ModuleManager
{
private:
	using ModuleDependencyGraph = std::unordered_map<std::string, std::vector<std::string>>;

	ModuleDependencyGraph m_dependency_graph;
	std::unordered_map<std::string, ModuleDeclaration> m_module_declarations;
	TokenStream m_main_token_stream;
	std::string m_main_file_name;
	std::vector<std::filesystem::path> m_search_paths;

public:
	ModuleManager(TokenStream&& main_file_tokens, std::string_view main_file_name);

	MidoriResult::ModuleManagerResult GenerateBuildGraph();

private:
	bool HasCircularDependency() const;

	void BuildDependencyGraph(BuildGraph& build_graph);

	void CalculateInDegrees(BuildGraph& build_graph);

	std::tuple<std::string, bool, std::vector<ModuleExport>, int> ParseModuleDeclaration(const TokenStream& tokens, const std::string& file_path);

	void InitializeSearchPaths();

	std::vector<std::filesystem::path> GetPossibleModulePaths(const std::string& module_name) const;

	void SkipWhiteSpace(const TokenStream& tokens, int& current_index);

};
