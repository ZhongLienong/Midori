#pragma once

#include "Compiler/Module/Module.h"
#include "Compiler/Result/Result.h"
#include "Compiler/Token/Token.h"

#include <filesystem>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class ModuleManager
{
private:
	using ModuleDependencyGraph = std::unordered_map<std::string, std::vector<std::string>>;

	enum class StatementType { MODULE, EXPORT, IMPORT, USE };

	struct StatementSpan
	{
		StatementType m_type;
		int m_start;
		int m_end;
		int m_line;
	};

	ModuleDependencyGraph m_dependency_graph;
	std::unordered_map<std::string, ModuleDeclaration> m_module_declarations;
	TokenStream m_main_token_stream;
	std::string m_main_file_name;

public:
	ModuleManager(TokenStream&& main_file_tokens, std::string_view main_file_name);

	MidoriResult::ModuleManagerResult GenerateBuildGraph();

private:
	MidoriResult::ModuleManagerResult GenerateBuildGraphImpl(BuildGraph& build_graph);

	bool HasCircularDependency() const;

	bool CheckCycle(const std::string& node, std::unordered_set<std::string>& visited, std::unordered_set<std::string>& recursion_stack) const;

	void BuildDependencyGraph(BuildGraph& build_graph);

	void CalculateInDegrees(BuildGraph& build_graph);

	std::vector<StatementSpan> ScanModuleStatements(const TokenStream& tokens);

	int ComputeStatementEnd(const TokenStream& tokens, int start, StatementType type);

	std::tuple<std::string, std::vector<ModuleExport>> ExtractModuleDeclaration(const TokenStream& tokens, const std::vector<StatementSpan>& spans);

	std::vector<std::pair<std::string, int>> ExtractImports(const TokenStream& tokens, const std::vector<StatementSpan>& spans);

	std::vector<UseImport> ExtractUseStatements(const TokenStream& tokens, const std::vector<StatementSpan>& spans);

	void SkipWhiteSpace(const TokenStream& tokens, int& current_index);

	static bool IsKeyword(Token::Name token_name);

};
