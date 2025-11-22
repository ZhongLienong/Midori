#include "ModuleManager.h"
#include "Common/Error/Error.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/Token/Token.h"

#include <filesystem>
#include <fstream>
#include <queue>
#include <sstream>
#include <algorithm>

using namespace std::string_literals;

ModuleManager::ModuleManager(TokenStream&& main_file_tokens, std::string_view main_file_name)
	: m_main_token_stream(std::move(main_file_tokens)),
	m_main_file_name(main_file_name)
{
	InitializeSearchPaths();
}

MidoriResult::ModuleManagerResult ModuleManager::GenerateBuildGraph()
{
	BuildGraph build_graph;

	// Process the main file first
	if (m_main_token_stream.Size() != 0)
	{
		// Parse module declaration (if present)
		auto [module_name, has_module_decl, exports, token_pos] = ParseModuleDeclaration(m_main_token_stream, m_main_file_name);

		ModuleDeclaration module_decl(module_name, m_main_file_name);
		module_decl.m_has_module_declaration = has_module_decl;
		module_decl.m_exports = std::move(exports);
		m_module_declarations[m_main_file_name] = std::move(module_decl);

		// Remove module and export declarations from token stream
		// Erase all tokens from index 0 to token_pos-1
		if (token_pos > 0)
		{
			m_main_token_stream.Erase(m_main_token_stream.begin() + token_pos);
		}

		std::vector<std::pair<std::string, int>> import_paths;

		// Skip whitespace tokens at the beginning
		int whitespace_count = 0;
		while (m_main_token_stream.Size() > whitespace_count && m_main_token_stream[whitespace_count].m_token_name == Token::Name::WHITESPACE)
		{
			whitespace_count += 1;
		}
		if (whitespace_count > 0)
		{
			m_main_token_stream.Erase(m_main_token_stream.begin() + whitespace_count);
		}

		// Parse imports if they exist (after module declaration was removed)
		if (m_main_token_stream.Size() > 0 && m_main_token_stream[0].m_token_name == Token::Name::IMPORT)
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
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Invalid import block", m_main_token_stream[0].m_line, m_main_file_name));
			}

			int current_index = 1; // Start after "import"
			if (m_main_token_stream[current_index].m_token_name != Token::Name::LEFT_BRACE)
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected '{' after 'import'", m_main_token_stream[current_index].m_line, m_main_file_name));
			}
			current_index += 1;

			while (current_index < m_main_token_stream.Size() && m_main_token_stream[current_index].m_token_name != Token::Name::RIGHT_BRACE)
			{
				if (m_main_token_stream[current_index].m_token_name != Token::Name::TEXT_LITERAL)
				{
					return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected text literal for import path", m_main_token_stream[current_index].m_line, m_main_file_name));
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
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected '}' after import paths", m_main_token_stream[current_index].m_line, m_main_file_name));
			}

			current_index += 1u; // Move past the '}'

			// Erase all import block tokens (from beginning up to current_index)
			m_main_token_stream.Erase(m_main_token_stream.begin() + current_index);
		}

		// Parse 'use' statements if they exist (after import block)
		std::vector<UseImport> use_imports;

		// Skip whitespace before 'use' statements
		whitespace_count = 0;
		while (m_main_token_stream.Size() > whitespace_count && m_main_token_stream[whitespace_count].m_token_name == Token::Name::WHITESPACE)
		{
			whitespace_count += 1;
		}
		if (whitespace_count > 0)
		{
			m_main_token_stream.Erase(m_main_token_stream.begin() + whitespace_count);
		}

		int use_tokens_consumed = 0;
		while (m_main_token_stream.Size() > use_tokens_consumed && m_main_token_stream[use_tokens_consumed].m_token_name == Token::Name::USE)
		{
			/*
				Syntax: use ModuleName.{Symbol1, Symbol2}
				or:     use ModuleName.Symbol

				Minimum tokens: use Module . { Symbol }
			*/
			int current_use_index = use_tokens_consumed + 1; // Skip 'use'
			SkipWhiteSpace(m_main_token_stream, current_use_index);

			if (current_use_index >= m_main_token_stream.Size() || m_main_token_stream[current_use_index].m_token_name != Token::Name::IDENTIFIER_LITERAL)
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected module name after 'use'", m_main_token_stream[use_tokens_consumed].m_line, m_main_file_name));
			}

			std::string use_module_name = m_main_token_stream[current_use_index].m_lexeme;
			current_use_index += 1;
			SkipWhiteSpace(m_main_token_stream, current_use_index);

			if (current_use_index >= m_main_token_stream.Size() || m_main_token_stream[current_use_index].m_token_name != Token::Name::DOT)
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected '.' after module name in 'use' statement", m_main_token_stream[current_use_index].m_line, m_main_file_name));
			}
			current_use_index += 1; // Skip '.'
			SkipWhiteSpace(m_main_token_stream, current_use_index);

			// Check if it's a block import {Symbol1, Symbol2} or single symbol import
			if (current_use_index < m_main_token_stream.Size() && m_main_token_stream[current_use_index].m_token_name == Token::Name::LEFT_BRACE)
			{
				// Block import: use Module.{Symbol1, Symbol2}
				current_use_index += 1; // Skip '{'
				SkipWhiteSpace(m_main_token_stream, current_use_index);

				while (current_use_index < m_main_token_stream.Size() && m_main_token_stream[current_use_index].m_token_name != Token::Name::RIGHT_BRACE)
				{
					if (m_main_token_stream[current_use_index].m_token_name != Token::Name::IDENTIFIER_LITERAL)
					{
						return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected symbol name in 'use' statement", m_main_token_stream[current_use_index].m_line, m_main_file_name));
					}

					use_imports.emplace_back(use_module_name, m_main_token_stream[current_use_index].m_lexeme);
					current_use_index += 1;
					SkipWhiteSpace(m_main_token_stream, current_use_index);

					if (current_use_index < m_main_token_stream.Size() && m_main_token_stream[current_use_index].m_token_name == Token::Name::COMMA)
					{
						current_use_index += 1;
						SkipWhiteSpace(m_main_token_stream, current_use_index);
					}
				}

				if (current_use_index >= m_main_token_stream.Size() || m_main_token_stream[current_use_index].m_token_name != Token::Name::RIGHT_BRACE)
				{
					return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected '}' after symbol list in 'use' statement", m_main_token_stream[current_use_index].m_line, m_main_file_name));
				}
				current_use_index += 1; // Skip '}'
			}
			else if (current_use_index < m_main_token_stream.Size() && m_main_token_stream[current_use_index].m_token_name == Token::Name::IDENTIFIER_LITERAL)
			{
				// Single symbol import: use Module.Symbol
				use_imports.emplace_back(use_module_name, m_main_token_stream[current_use_index].m_lexeme);
				current_use_index += 1;
			}
			else
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Expected symbol name or '{' after '.' in 'use' statement", m_main_token_stream[current_use_index].m_line, m_main_file_name));
			}

			SkipWhiteSpace(m_main_token_stream, current_use_index);
			use_tokens_consumed = current_use_index;
		}

		// Erase all 'use' statement tokens
		if (use_tokens_consumed > 0)
		{
			m_main_token_stream.Erase(m_main_token_stream.begin() + use_tokens_consumed);
		}

		BuildGraph::BuildNode& main_node = build_graph.m_nodes[m_main_file_name];
		main_node.m_tokens = m_main_token_stream;
		main_node.m_file_name = m_main_file_name;
		main_node.m_use_imports = std::move(use_imports);

		for (const auto& [import_path, line] : import_paths)
		{
			std::filesystem::path resolved_path(import_path);
			if (!resolved_path.is_absolute())
			{
				resolved_path = std::filesystem::path(m_main_file_name).parent_path() / resolved_path;
			}

			std::string include_absolute_path_str = std::filesystem::weakly_canonical(resolved_path).string();

			m_dependency_graph[m_main_file_name].emplace_back(include_absolute_path_str);

			if (build_graph.m_nodes.contains(include_absolute_path_str))
			{
				continue;
			}

			std::ifstream include_file(include_absolute_path_str);
			if (!include_file.is_open())
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Could not open import file: "s + include_absolute_path_str, line, m_main_file_name));
			}

			if (HasCircularDependency())
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Circular dependency detected: "s + include_absolute_path_str, line, m_main_file_name));
			}

			std::ostringstream include_file_stream;
			include_file_stream << include_file.rdbuf();

			MidoriResult::LexerResult lex_result = Lexer(include_file_stream.str(), include_absolute_path_str).Lex();
			if (!lex_result.has_value())
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext(lex_result.error(), line, m_main_file_name));
			}

			TokenStream imported_token_stream = std::move(lex_result.value());

			ModuleManager module_manager(std::move(imported_token_stream), std::move(include_absolute_path_str));
			MidoriResult::ModuleManagerResult nested_build_graph_result = module_manager.GenerateBuildGraph();
			if (!nested_build_graph_result.has_value())
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext(nested_build_graph_result.error(), line, m_main_file_name));
			}

			BuildGraph& nested_build_graph = nested_build_graph_result.value();
			for (const auto& [file_name, node] : nested_build_graph.m_nodes)
			{
				if (!build_graph.m_nodes.contains(file_name))
				{
					build_graph.m_nodes[file_name] = node;
				}
			}

			// Merge module declarations
			for (const auto& [nested_file_path, nested_module_decl] : module_manager.m_module_declarations)
			{
				if (!m_module_declarations.contains(nested_file_path))
				{
					m_module_declarations[nested_file_path] = nested_module_decl;
				}
			}

			for (const auto& [src, dependencies] : module_manager.m_dependency_graph)
			{
				for (const std::string& dependency : dependencies)
				{
					if (!std::ranges::contains(m_dependency_graph[src], dependency))
					{
						m_dependency_graph[src].emplace_back(dependency);
					}
				}
			}
		}
	}

	BuildDependencyGraph(build_graph);

	CalculateInDegrees(build_graph);

	if (HasCircularDependency())
	{
		return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Circular dependency detected in final build graph", 0, m_main_file_name));
	}

	build_graph.m_module_declarations = m_module_declarations;

	// Populate use imports map from build nodes
	for (const auto& [file_name, node] : build_graph.m_nodes)
	{
		if (!node.m_use_imports.empty())
		{
			build_graph.m_use_imports[file_name] = node.m_use_imports;
		}
	}

	return build_graph;
}

bool ModuleManager::HasCircularDependency() const
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

void ModuleManager::BuildDependencyGraph(BuildGraph& build_graph)
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

void ModuleManager::CalculateInDegrees(BuildGraph& build_graph)
{
	for (auto& [file, node] : build_graph.m_nodes)
	{
		node.m_in_degree = 0;
	}

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

std::tuple<std::string, bool, std::vector<ModuleExport>, int> ModuleManager::ParseModuleDeclaration(const TokenStream& tokens, const std::string& file_path)
{
	std::string module_name;
	bool has_module_decl = false;
	std::vector<ModuleExport> all_exports;
	int current_index = 0;

	// Check for "module ModuleName" declaration
	if (tokens.Size() > 0 && tokens[0].m_token_name == Token::Name::MODULE)
	{
		has_module_decl = true;
		current_index = 1;
		SkipWhiteSpace(tokens, current_index);

		// Expect module name (could be dotted like Math.Vector)
		if (current_index >= tokens.Size() || tokens[current_index].m_token_name != Token::Name::IDENTIFIER_LITERAL)
		{
			// Error will be handled by parser later
			return { "", false, {}, 0 };
		}

		module_name = tokens[current_index].m_lexeme;
		current_index += 1;
		SkipWhiteSpace(tokens, current_index);

		// Handle dotted module names (e.g., Math.Vector)
		while (current_index < tokens.Size() && tokens[current_index].m_token_name == Token::Name::DOT)
		{
			current_index += 1; // Skip dot
			SkipWhiteSpace(tokens, current_index);
			if (current_index >= tokens.Size() || tokens[current_index].m_token_name != Token::Name::IDENTIFIER_LITERAL)
			{
				// Error will be handled by parser later
				break;
			}
			module_name += "."s;
			module_name += tokens[current_index].m_lexeme;
			current_index += 1;
			SkipWhiteSpace(tokens, current_index);
		}

		// Parse export blocks if present
		while (current_index < tokens.Size() && (tokens[current_index].m_token_name == Token::Name::PUBLIC || tokens[current_index].m_token_name == Token::Name::PRIVATE))
		{
			VisibilityLevel visibility = VisibilityLevel::Public;

			// Require explicit visibility modifier (syntax: "public export { ... }" or "private export { ... }")
			if (tokens[current_index].m_token_name == Token::Name::PUBLIC)
			{
				visibility = VisibilityLevel::Public;
				current_index += 1;
				SkipWhiteSpace(tokens, current_index);

				if (current_index >= tokens.Size() || tokens[current_index].m_token_name != Token::Name::EXPORT)
				{
					// Error will be handled by parser later
					break;
				}
			}
			else if (tokens[current_index].m_token_name == Token::Name::PRIVATE)
			{
				visibility = VisibilityLevel::Private;
				current_index += 1;
				SkipWhiteSpace(tokens, current_index);

				if (current_index >= tokens.Size() || tokens[current_index].m_token_name != Token::Name::EXPORT)
				{
					// Error will be handled by parser later
					break;
				}
			}

			// Consume "export" keyword
			current_index += 1;
			SkipWhiteSpace(tokens, current_index);

			if (current_index >= tokens.Size() || tokens[current_index].m_token_name != Token::Name::LEFT_BRACE)
			{
				// Error will be handled by parser later
				break;
			}
			current_index += 1;
			SkipWhiteSpace(tokens, current_index);

			// Parse export list
			while (current_index < tokens.Size() && tokens[current_index].m_token_name != Token::Name::RIGHT_BRACE)
			{
				SkipWhiteSpace(tokens, current_index);
				if (current_index >= tokens.Size())
				{
					break;
				}

				if (tokens[current_index].m_token_name == Token::Name::RIGHT_BRACE)
				{
					break;
				}

				if (tokens[current_index].m_token_name == Token::Name::IDENTIFIER_LITERAL)
				{
					std::string symbol_name = tokens[current_index].m_lexeme;
					all_exports.emplace_back(symbol_name, visibility);
					current_index += 1;
					SkipWhiteSpace(tokens, current_index);

					// Skip optional comma
					if (current_index < tokens.Size() && tokens[current_index].m_token_name == Token::Name::COMMA)
					{
						current_index += 1;
						SkipWhiteSpace(tokens, current_index);
					}
				}
				else
				{
					// Error - expected identifier
					break;
				}
			}

			// Expect '}'
			if (current_index < tokens.Size() && tokens[current_index].m_token_name == Token::Name::RIGHT_BRACE)
			{
				current_index += 1;
				SkipWhiteSpace(tokens, current_index);
			}
		}
	}
	else
	{
		// No module declaration
		module_name = std::filesystem::path(file_path).stem().string();
	}

	return { module_name, has_module_decl, all_exports, current_index };
}

void ModuleManager::SkipWhiteSpace(const TokenStream& tokens, int& current_index)
{
	while (current_index < tokens.Size() && tokens[current_index].m_token_name == Token::Name::WHITESPACE)
	{
		current_index += 1;
	}
};

void ModuleManager::InitializeSearchPaths()
{
	// Add current directory
	m_search_paths.push_back(std::filesystem::current_path());

	// Add directory of main file
	std::filesystem::path main_file_dir = std::filesystem::path(m_main_file_name).parent_path();
	if (!main_file_dir.empty() && std::filesystem::exists(main_file_dir))
	{
		m_search_paths.push_back(std::filesystem::weakly_canonical(main_file_dir));
	}

	// Add MIDORI_PATH environment variable paths
	#ifdef _WIN32
	char* midori_path = nullptr;
	size_t len = 0u;
	if (_dupenv_s(&midori_path, &len, "MIDORI_PATH") == 0 && midori_path != nullptr)
	{
		std::string path_str(midori_path);
		std::free(midori_path);
	#else
	const char* midori_path = std::getenv("MIDORI_PATH");
	if (midori_path != nullptr)
	{
		std::string path_str(midori_path);
	#endif
		// Split by platform-specific path separator
		#ifdef _WIN32
		const char separator = ';';
		#else
		const char separator = ':';
		#endif

		size_t start = 0u;
		size_t end = path_str.find(separator);
		while (end != std::string::npos)
		{
			std::string path = path_str.substr(start, end - start);
			if (!path.empty() && std::filesystem::exists(path))
			{
				m_search_paths.push_back(std::filesystem::weakly_canonical(path));
			}
			start = end + 1u;
			end = path_str.find(separator, start);
		}
		// Add last path
		std::string path = path_str.substr(start);
		if (!path.empty() && std::filesystem::exists(path))
		{
			m_search_paths.push_back(std::filesystem::weakly_canonical(path));
		}
	}

	// TODO: Add standard library path (MidoriPrelude/)
	// This should be configured at build time or relative to executable
}

std::vector<std::filesystem::path> ModuleManager::GetPossibleModulePaths(const std::string& module_name) const
{
	std::vector<std::filesystem::path> paths;

	// Convert "Math.Vector" to "Math/Vector.mdr"
	std::string path_str = module_name;
	std::ranges::replace(path_str, '.', static_cast<char>(std::filesystem::path::preferred_separator));
	paths.push_back(path_str + ".mdr"s);

	// Also try as-is with .mdr extension
	paths.push_back(module_name + ".mdr"s);

	return paths;
}