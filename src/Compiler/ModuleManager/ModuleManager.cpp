#include "ModuleManager.h"
#include "Common/Error/Error.h"
#include "Common/Printer/Printer.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/Token/Token.h"
#include "Compiler/ImportResolver/ImportResolver.h"
#include "Compiler/PackageManager/PackageManifest.h"
#include "Library/DynamicFFIRegistry/DynamicFFIRegistry.h"

#include <filesystem>
#include <format>
#include <fstream>
#include <queue>
#include <sstream>
#include <algorithm>

using namespace std::string_literals;

ModuleManager::ModuleManager(TokenStream&& main_file_tokens, std::string_view main_file_name)
	: m_main_token_stream(std::move(main_file_tokens)),
	m_main_file_name(main_file_name)
{
}

MidoriResult::ModuleManagerResult ModuleManager::GenerateBuildGraph()
{
	BuildGraph build_graph;
	return GenerateBuildGraphImpl(build_graph);
}

MidoriResult::ModuleManagerResult ModuleManager::GenerateBuildGraphImpl(BuildGraph& build_graph)
{

	if (m_main_token_stream.Size() != 0)
	{
		std::vector<StatementSpan> spans = ScanModuleStatements(m_main_token_stream);

		std::tuple<std::string, std::vector<ModuleExport>> module_result = ExtractModuleDeclaration(m_main_token_stream, spans);
		std::string module_name = std::get<0>(module_result);
		std::vector<ModuleExport> exports = std::get<1>(module_result);

		if (module_name.empty())
		{
			return std::unexpected
			(
				MidoriError::GenerateModuleErrorWithContext
				(
					"Module declaration required. All .mdr files must begin with an explicit 'module ModuleName' declaration.",
					1,
					m_main_file_name
				)
			);
		}

		if (build_graph.m_module_name_to_file.contains(module_name))
		{
			const std::string& existing_file = build_graph.m_module_name_to_file.at(module_name);
			if (existing_file != m_main_file_name)
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext(
					std::format
					(
						"Duplicate module declaration: '{}' is declared in multiple files:\n  First:  {}\n  Second: {}",
						module_name,
						existing_file,
						m_main_file_name
					),
					1,
					m_main_file_name
				));
			}
		}
		else
		{
			build_graph.m_module_name_to_file[module_name] = m_main_file_name;
		}

		bool has_module_decl = std::ranges::any_of(spans, [](const StatementSpan& span) { return span.m_type == StatementType::MODULE; });

		ModuleDeclaration module_decl(module_name, m_main_file_name);
		module_decl.m_has_module_declaration = has_module_decl;
		module_decl.m_exports = std::move(exports);
		m_module_declarations[m_main_file_name] = std::move(module_decl);

		std::vector<std::pair<std::string, int>> import_paths = ExtractImports(m_main_token_stream, spans);
		std::vector<UseImport> use_imports = ExtractUseStatements(m_main_token_stream, spans);

		std::ranges::sort(spans, [](const StatementSpan& a, const StatementSpan& b) { return a.m_start > b.m_start; });

		for (const StatementSpan& span : spans)
		{
			for (int i = 0; i < span.m_end - span.m_start; i += 1)
			{
				m_main_token_stream.Erase(m_main_token_stream.begin() + span.m_start);
			}
		}

		BuildGraph::BuildNode& main_node = build_graph.m_nodes[m_main_file_name];
		main_node.m_tokens = m_main_token_stream;
		main_node.m_file_name = m_main_file_name;
		main_node.m_use_imports = std::move(use_imports);

		ImportResolver resolver(m_main_file_name);

		for (const auto& [import_specifier, line] : import_paths)
		{
			std::optional<ImportResolver::ResolvedImport> resolved_opt = resolver.Resolve(import_specifier);
			if (!resolved_opt.has_value())
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Could not resolve import: "s + import_specifier, line, m_main_file_name));
			}

			std::string include_absolute_path_str = resolved_opt->m_absolute_path;

			m_dependency_graph[m_main_file_name].emplace_back(include_absolute_path_str);

			std::filesystem::path import_path(include_absolute_path_str);
			std::filesystem::path package_dir = import_path.parent_path();
			std::filesystem::path package_manifest_path = package_dir / "package.midori";

			if (std::filesystem::exists(package_manifest_path))
			{
				std::optional<PackageManifest> manifest_opt = PackageManifest::Load(package_dir);
				if (manifest_opt.has_value())
				{
					const PackageManifest& manifest = manifest_opt.value();
					const PackageFFI& ffi = manifest.GetFFI();

					if (ffi.m_enabled)
					{
						std::filesystem::path library_path = manifest.GetFFILibraryPath();

						if (std::filesystem::exists(library_path))
						{
							DynamicFFIRegistry& registry = DynamicFFIRegistry::GetInstance();
							if (!registry.IsLibraryLoaded(manifest.GetInfo().m_name))
							{
								registry.LoadLibraryWithFunctions(library_path, manifest.GetInfo().m_name, ffi.m_functions);
							}
						}
					}
				}
			}

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
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext(lex_result.error().Rendered(), line, m_main_file_name));
			}

			TokenStream imported_token_stream = std::move(lex_result.value());

			ModuleManager module_manager(std::move(imported_token_stream), std::move(include_absolute_path_str));
			MidoriResult::ModuleManagerResult nested_build_graph_result = module_manager.GenerateBuildGraphImpl(build_graph);
			if (!nested_build_graph_result.has_value())
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext(nested_build_graph_result.error().Rendered(), line, m_main_file_name));
			}

			BuildGraph& nested_build_graph = nested_build_graph_result.value();
			for (const auto& [file_name, node] : nested_build_graph.m_nodes)
			{
				if (!build_graph.m_nodes.contains(file_name))
				{
					build_graph.m_nodes[file_name] = node;
				}
			}

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
	std::unordered_set<std::string> recursion_stack;

	for (const auto& [node, _] : m_dependency_graph)
	{
		if (CheckCycle(node, visited, recursion_stack))
		{
			return true;
		}
	}

	return false;
}

bool ModuleManager::CheckCycle(const std::string& node, std::unordered_set<std::string>& visited, std::unordered_set<std::string>& recursion_stack) const
{
	if (recursion_stack.contains(node))
	{
		return true;
	}

	if (visited.contains(node))
	{
		return false;
	}

	visited.emplace(node);
	recursion_stack.emplace(node);

	if (m_dependency_graph.contains(node))
	{
		for (const std::string& dependency : m_dependency_graph.at(node))
		{
			if (CheckCycle(dependency, visited, recursion_stack))
			{
				return true;
			}
		}
	}

	recursion_stack.erase(node);
	return false;
}

void ModuleManager::BuildDependencyGraph(BuildGraph& build_graph)
{
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

void ModuleManager::SkipWhiteSpace(const TokenStream& tokens, int& current_index)
{
	while (current_index < tokens.Size() && tokens[current_index].m_token_name == Token::Name::WHITESPACE)
	{
		current_index += 1;
	}
};

std::vector<ModuleManager::StatementSpan> ModuleManager::ScanModuleStatements(const TokenStream& tokens)
{
	std::vector<StatementSpan> spans;
	int brace_depth = 0;

	for (int i = 0; i < tokens.Size(); i += 1)
	{
		const Token& token = tokens[i];

		if (token.m_token_name == Token::Name::LEFT_BRACE)
		{
			brace_depth += 1;
		}
		else if (token.m_token_name == Token::Name::RIGHT_BRACE)
		{
			brace_depth -= 1;
		}
		else if (brace_depth == 0)
		{
			StatementType stmt_type = StatementType::MODULE;
			bool is_module_statement = false;

			if (token.m_token_name == Token::Name::MODULE)
			{
				stmt_type = StatementType::MODULE;
				is_module_statement = true;
			}
			else if (token.m_token_name == Token::Name::IMPORT)
			{
				stmt_type = StatementType::IMPORT;
				is_module_statement = true;
			}
			else if (token.m_token_name == Token::Name::USE)
			{
				stmt_type = StatementType::USE;
				is_module_statement = true;
			}
			else if (token.m_token_name == Token::Name::PUBLIC || token.m_token_name == Token::Name::PRIVATE)
			{
				int lookahead = i + 1;
				SkipWhiteSpace(tokens, lookahead);
				if (lookahead < tokens.Size() && tokens[lookahead].m_token_name == Token::Name::EXPORT)
				{
					stmt_type = StatementType::EXPORT;
					is_module_statement = true;
				}
			}

			if (is_module_statement)
			{
				int end = ComputeStatementEnd(tokens, i, stmt_type);
				spans.push_back(StatementSpan{ stmt_type, i, end, token.m_line });
				i = end - 1;
			}
		}
	}

	return spans;
}

int ModuleManager::ComputeStatementEnd(const TokenStream& tokens, int start, StatementType type)
{
	int current = start + 1;
	SkipWhiteSpace(tokens, current);

	if (type == StatementType::MODULE)
	{
		while (current < tokens.Size() && (tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL || tokens[current].m_token_name == Token::Name::SINGLE_DOT))
		{
			current += 1;
			SkipWhiteSpace(tokens, current);
		}
		return current;
	}
	else if (type == StatementType::EXPORT)
	{
		SkipWhiteSpace(tokens, current);

		if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::EXPORT)
		{
			current += 1;
			SkipWhiteSpace(tokens, current);
		}

		if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::LEFT_BRACE)
		{
			current += 1;
			int depth = 1;
			while (current < tokens.Size() && depth > 0)
			{
				if (tokens[current].m_token_name == Token::Name::LEFT_BRACE)
				{
					depth += 1;
				}
				else if (tokens[current].m_token_name == Token::Name::RIGHT_BRACE)
				{
					depth -= 1;
				}
				current += 1;
			}
		}
		return current;
	}
	else if (type == StatementType::IMPORT)
	{
		if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::LEFT_BRACE)
		{
			current += 1;
			int depth = 1;
			while (current < tokens.Size() && depth > 0)
			{
				if (tokens[current].m_token_name == Token::Name::LEFT_BRACE)
				{
					depth += 1;
				}
				else if (tokens[current].m_token_name == Token::Name::RIGHT_BRACE)
				{
					depth -= 1;
				}
				current += 1;
			}
		}
		return current;
	}
	else if (type == StatementType::USE)
	{
		while (current < tokens.Size() && tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
		{
			current += 1;
			SkipWhiteSpace(tokens, current);
		}

		if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::SINGLE_DOT)
		{
			current += 1;
			SkipWhiteSpace(tokens, current);
		}

		if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::LEFT_BRACE)
		{
			current += 1;
			int depth = 1;
			while (current < tokens.Size() && depth > 0)
			{
				if (tokens[current].m_token_name == Token::Name::LEFT_BRACE)
				{
					depth += 1;
				}
				else if (tokens[current].m_token_name == Token::Name::RIGHT_BRACE)
				{
					depth -= 1;
				}
				current += 1;
			}
		}
		else
		{
			while (current < tokens.Size() && tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
			{
				current += 1;
				SkipWhiteSpace(tokens, current);
			}
		}

		if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::SINGLE_SEMICOLON)
		{
			current += 1;
		}

		return current;
	}

	return current;
}

bool ModuleManager::IsKeyword(Token::Name token_name)
{
	return token_name != Token::Name::IDENTIFIER_LITERAL &&
	       token_name != Token::Name::TEXT_LITERAL &&
	       token_name != Token::Name::INTEGER_LITERAL &&
	       token_name != Token::Name::FLOAT_LITERAL &&
	       token_name != Token::Name::WHITESPACE &&
	       token_name != Token::Name::END_OF_FILE &&
	       static_cast<int>(token_name) >= static_cast<int>(Token::Name::ELSE);
}

std::tuple<std::string, std::vector<ModuleExport>> ModuleManager::ExtractModuleDeclaration(const TokenStream& tokens, const std::vector<StatementSpan>& spans)
{
	std::string module_name;
	std::vector<ModuleExport> all_exports;

	for (const StatementSpan& span : spans)
	{
		if (span.m_type == StatementType::MODULE)
		{
			int current = span.m_start + 1;
			SkipWhiteSpace(tokens, current);

			if (current < tokens.Size() && IsKeyword(tokens[current].m_token_name))
			{
				CompilerError error_message = MidoriError::GenerateModuleErrorWithContext("'" + tokens[current].m_lexeme + "' is a reserved keyword and cannot be used as a module name.", tokens[current].m_line, m_main_file_name);
				Printer::Print<Printer::Color::RED>(std::string(error_message.Rendered()) + "\n");
				std::exit(EXIT_FAILURE);
			}

			while (current < span.m_end && (tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL || tokens[current].m_token_name == Token::Name::SINGLE_DOT))
			{
				if (tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
				{
					module_name += tokens[current].m_lexeme;
				}
				else if (tokens[current].m_token_name == Token::Name::SINGLE_DOT)
				{
					module_name += '.';
				}
				current += 1;
			}
		}
		else if (span.m_type == StatementType::EXPORT)
		{
			int current = span.m_start;
			VisibilityLevel visibility = VisibilityLevel::Public;

			if (tokens[current].m_token_name == Token::Name::PUBLIC)
			{
				visibility = VisibilityLevel::Public;
			}
			else if (tokens[current].m_token_name == Token::Name::PRIVATE)
			{
				visibility = VisibilityLevel::Private;
			}

			current += 1;
			SkipWhiteSpace(tokens, current);

			if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::EXPORT)
			{
				current += 1;
				SkipWhiteSpace(tokens, current);
			}

			if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::LEFT_BRACE)
			{
				current += 1;
				SkipWhiteSpace(tokens, current);

				while (current < span.m_end && tokens[current].m_token_name != Token::Name::RIGHT_BRACE)
				{
					if (tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
					{
						all_exports.emplace_back(tokens[current].m_lexeme, visibility);
						current += 1;
						SkipWhiteSpace(tokens, current);

						if (current < span.m_end && tokens[current].m_token_name == Token::Name::COMMA)
						{
							current += 1;
							SkipWhiteSpace(tokens, current);
						}
					}
					else
					{
						current += 1;
					}
				}
			}
		}
	}

	return { module_name, all_exports };
}

std::vector<std::pair<std::string, int>> ModuleManager::ExtractImports(const TokenStream& tokens, const std::vector<StatementSpan>& spans)
{
	std::vector<std::pair<std::string, int>> import_paths;

	for (const StatementSpan& span : spans)
	{
		if (span.m_type == StatementType::IMPORT)
		{
			int current = span.m_start + 1;
			SkipWhiteSpace(tokens, current);

			if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::LEFT_BRACE)
			{
				current += 1;
				SkipWhiteSpace(tokens, current);

				while (current < span.m_end && tokens[current].m_token_name != Token::Name::RIGHT_BRACE)
				{
					std::string import_specifier;
					int import_line = tokens[current].m_line;

					if (tokens[current].m_token_name == Token::Name::TEXT_LITERAL)
					{
						import_specifier = tokens[current].m_lexeme;
						current += 1;
					}
					else if (tokens[current].m_token_name == Token::Name::LEFT_ANGLE)
					{
						current += 1;
						std::string module_name;

						while (current < tokens.Size() && tokens[current].m_token_name != Token::Name::RIGHT_ANGLE)
						{
							if (tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
							{
								module_name += tokens[current].m_lexeme;
							}
							else if (tokens[current].m_token_name == Token::Name::SINGLE_DOT)
							{
								module_name += '.';
							}
							current += 1;
						}

						if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::RIGHT_ANGLE)
						{
							current += 1;
						}

						import_specifier = "<"s + module_name + ">"s;
					}
					else
					{
						current += 1;
					}

					if (!import_specifier.empty())
					{
						import_paths.emplace_back(import_specifier, import_line);
					}

					SkipWhiteSpace(tokens, current);
					if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::COMMA)
					{
						current += 1;
						SkipWhiteSpace(tokens, current);
					}
				}
			}
		}
	}

	return import_paths;
}

std::vector<UseImport> ModuleManager::ExtractUseStatements(const TokenStream& tokens, const std::vector<StatementSpan>& spans)
{
	std::vector<UseImport> use_imports;

	for (const StatementSpan& span : spans)
	{
		if (span.m_type == StatementType::USE)
		{
			int current = span.m_start + 1;
			SkipWhiteSpace(tokens, current);

			if (current >= tokens.Size() || tokens[current].m_token_name != Token::Name::IDENTIFIER_LITERAL)
			{
				continue;
			}

			std::string module_name = tokens[current].m_lexeme;
			current += 1;
			SkipWhiteSpace(tokens, current);

			if (current >= tokens.Size() || tokens[current].m_token_name != Token::Name::SINGLE_DOT)
			{
				continue;
			}
			current += 1;
			SkipWhiteSpace(tokens, current);

			if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::LEFT_BRACE)
			{
				current += 1;
				SkipWhiteSpace(tokens, current);

				while (current < span.m_end && tokens[current].m_token_name != Token::Name::RIGHT_BRACE)
				{
					if (tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
					{
						use_imports.emplace_back(module_name, tokens[current].m_lexeme);
						current += 1;
						SkipWhiteSpace(tokens, current);

						if (current < span.m_end && tokens[current].m_token_name == Token::Name::COMMA)
						{
							current += 1;
							SkipWhiteSpace(tokens, current);
						}
					}
					else
					{
						current += 1;
					}
				}
			}
			else if (current < tokens.Size() && tokens[current].m_token_name == Token::Name::IDENTIFIER_LITERAL)
			{
				use_imports.emplace_back(module_name, tokens[current].m_lexeme);
			}
		}
	}

	return use_imports;
}
