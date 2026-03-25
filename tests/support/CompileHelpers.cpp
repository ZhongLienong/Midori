#include "support/CompileHelpers.h"

#include "Compiler/BuildGraph/BuildGraph.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/ModuleManager/ModuleManager.h"
#include "Compiler/Parser/Parser.h"
#include "Compiler/TypeChecker/TypeChecker.h"
#include "Utility/Driver/MidoriDriver.h"

#include <unordered_map>
#include <utility>

namespace
{
	struct PreparedModule
	{
		MidoriTest::SourceFixture m_source;
		TokenStream m_tokens;
		std::optional<ModuleDeclaration> m_module_declaration;
		std::vector<UseImport> m_use_imports;

		PreparedModule(MidoriTest::SourceFixture source, TokenStream&& tokens, std::optional<ModuleDeclaration>&& module_declaration, std::vector<UseImport>&& use_imports)
			: m_source(std::move(source)),
			m_tokens(std::move(tokens)),
			m_module_declaration(std::move(module_declaration)),
			m_use_imports(std::move(use_imports))
		{
		}
	};

	std::expected<PreparedModule, CompilerError> PrepareSingleModule(MidoriTest::SourceFixture&& source)
	{
		MidoriResult::LexerResult lex_result = Lexer(std::string(source.SourceCode()), source.FileName()).Lex();
		if (!lex_result.has_value())
		{
			return std::unexpected(std::move(lex_result.error()));
		}

		MidoriResult::ModuleManagerResult build_graph_result = ModuleManager(std::move(lex_result.value()), source.FileName(), source.SourceLines()).GenerateBuildGraph();
		if (!build_graph_result.has_value())
		{
			return std::unexpected(std::move(build_graph_result.error()));
		}

		BuildGraph build_graph = std::move(build_graph_result.value());
		std::unordered_map<std::string, BuildGraph::BuildNode>::iterator node_it = build_graph.m_nodes.find(source.FileName());
		if (node_it == build_graph.m_nodes.end())
		{
			return std::unexpected(CompilerError::Simple(CompilerStage::Module, "Failed to locate the prepared main module."));
		}

		std::optional<ModuleDeclaration> module_declaration = std::nullopt;
		std::unordered_map<std::string, ModuleDeclaration>::const_iterator module_it = build_graph.m_module_declarations.find(source.FileName());
		if (module_it != build_graph.m_module_declarations.end())
		{
			module_declaration = module_it->second;
		}

		TokenStream tokens = std::move(node_it->second.m_tokens);
		std::vector<UseImport> use_imports = std::move(node_it->second.m_use_imports);
		return PreparedModule(std::move(source), std::move(tokens), std::move(module_declaration), std::move(use_imports));
	}
}

namespace MidoriTest
{
	LexedSnippet::LexedSnippet(SourceFixture source, TokenStream&& tokens)
		: m_source(std::move(source)),
		m_tokens(std::move(tokens))
	{
	}

	ParsedSnippet::ParsedSnippet(SourceFixture source, MidoriProgramTree&& program, std::vector<CompilerWarning>&& warnings, std::optional<ModuleDeclaration>&& module_declaration, std::vector<UseImport>&& use_imports)
		: m_source(std::move(source)),
		m_program(std::move(program)),
		m_warnings(std::move(warnings)),
		m_module_declaration(std::move(module_declaration)),
		m_use_imports(std::move(use_imports))
	{
	}

	TypedSnippet::TypedSnippet(SourceFixture source, MidoriProgramTree&& program, std::vector<CompilerWarning>&& warnings)
		: m_source(std::move(source)),
		m_program(std::move(program)),
		m_warnings(std::move(warnings))
	{
	}

	ExecutedSnippet::ExecutedSnippet(SourceFixture source, int exit_code)
		: m_source(std::move(source)),
		m_exit_code(exit_code)
	{
	}

	std::expected<LexedSnippet, CompilerError> LexSnippet(std::string source_code, std::string file_name)
	{
		SourceFixture source(std::move(source_code), std::move(file_name));
		MidoriResult::LexerResult lex_result = Lexer(std::string(source.SourceCode()), source.FileName()).Lex();
		if (!lex_result.has_value())
		{
			return std::unexpected(std::move(lex_result.error()));
		}

		return LexedSnippet(std::move(source), std::move(lex_result.value()));
	}

	std::expected<ParsedSnippet, CompilerError> ParseSnippet(std::string source_code, std::string file_name)
	{
		std::expected<PreparedModule, CompilerError> prepared_result = PrepareSingleModule(SourceFixture(std::move(source_code), std::move(file_name)));
		if (!prepared_result.has_value())
		{
			return std::unexpected(std::move(prepared_result.error()));
		}

		PreparedModule prepared = std::move(prepared_result.value());
		const ModuleDeclaration* module_declaration = prepared.m_module_declaration.has_value()
			? &prepared.m_module_declaration.value()
			: nullptr;
		Parser parser(std::move(prepared.m_tokens), prepared.m_source.FileName(), prepared.m_source.SourceLines(), {}, {}, prepared.m_use_imports, module_declaration);
		MidoriResult::ParserResult parse_result = parser.Parse();
		if (!parse_result.has_value())
		{
			return std::unexpected(std::move(parse_result.error()));
		}

		std::vector<CompilerWarning> warnings = parser.GetWarnings();
		return ParsedSnippet(std::move(prepared.m_source), std::move(parse_result.value()), std::move(warnings), std::move(prepared.m_module_declaration), std::move(prepared.m_use_imports));
	}

	std::expected<TypedSnippet, CompilerError> TypeCheckSnippet(std::string source_code, std::string file_name)
	{
		std::expected<ParsedSnippet, CompilerError> parsed_result = ParseSnippet(std::move(source_code), std::move(file_name));
		if (!parsed_result.has_value())
		{
			return std::unexpected(std::move(parsed_result.error()));
		}

		ParsedSnippet parsed = std::move(parsed_result.value());
		MidoriResult::TypeCheckerResult typecheck_result = TypeChecker(std::move(parsed.m_program), parsed.m_source.FileName(), parsed.m_source.SourceLines()).TypeCheck();
		if (!typecheck_result.has_value())
		{
			return std::unexpected(std::move(typecheck_result.error()));
		}

		return TypedSnippet(std::move(parsed.m_source), std::move(typecheck_result.value()), std::move(parsed.m_warnings));
	}

	MidoriResult::CompilerResult CompileSnippet(std::string source_code, std::string file_name)
	{
		return MidoriDriver::CompileSource(std::move(source_code), std::move(file_name));
	}

	std::expected<ExecutedSnippet, CompilerError> ExecuteSnippet(std::string source_code, std::string file_name)
	{
		SourceFixture source(std::move(source_code), std::move(file_name));
		MidoriResult::CompilerResult compile_result = MidoriDriver::CompileSource(std::string(source.SourceCode()), source.FileName());
		if (!compile_result.has_value())
		{
			return std::unexpected(std::move(compile_result.error()));
		}

		MidoriDriver::RunResult run_result = MidoriDriver::RunExecutable(std::move(compile_result.value()));
		if (!run_result.has_value())
		{
			return std::unexpected(std::move(run_result.error()));
		}

		return ExecutedSnippet(std::move(source), run_result.value());
	}

	std::vector<Token::Name> CollectTokenNames(const TokenStream& tokens, bool include_whitespace, bool include_end_of_file)
	{
		std::vector<Token::Name> token_names;
		for (int index = 0; index < tokens.Size(); index += 1)
		{
			const Token& token = tokens[index];
			if (!include_whitespace && token.m_token_name == Token::Name::WHITESPACE)
			{
				continue;
			}

			if (!include_end_of_file && token.m_token_name == Token::Name::END_OF_FILE)
			{
				continue;
			}

			token_names.emplace_back(token.m_token_name);
		}

		return token_names;
	}
}
