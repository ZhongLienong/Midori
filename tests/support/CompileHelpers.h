#pragma once

#include "Common/Error/Error.h"
#include "Compiler/Module/Module.h"
#include "Compiler/Result/Result.h"
#include "Compiler/Token/Token.h"
#include "support/OutputCapture.h"
#include "support/SourceFixture.h"

#include <expected>
#include <optional>
#include <string>
#include <vector>

namespace MidoriTest
{
	struct LexedSnippet
	{
		SourceFixture m_source;
		TokenStream m_tokens;

		LexedSnippet(SourceFixture source, TokenStream&& tokens);
	};

	struct ParsedSnippet
	{
		SourceFixture m_source;
		MidoriProgramTree m_program;
		std::vector<CompilerWarning> m_warnings;
		std::optional<ModuleDeclaration> m_module_declaration;
		std::vector<UseImport> m_use_imports;

		ParsedSnippet(SourceFixture source, MidoriProgramTree&& program, std::vector<CompilerWarning>&& warnings, std::optional<ModuleDeclaration>&& module_declaration, std::vector<UseImport>&& use_imports);
	};

	struct TypedSnippet
	{
		SourceFixture m_source;
		MidoriProgramTree m_program;
		std::vector<CompilerWarning> m_warnings;

		TypedSnippet(SourceFixture source, MidoriProgramTree&& program, std::vector<CompilerWarning>&& warnings);
	};

	struct AnalyzedSnippet
	{
		SourceFixture m_source;
		MidoriProgramTree m_program;
		std::vector<CompilerWarning> m_warnings;
		std::vector<CompilerError> m_errors;

		AnalyzedSnippet(SourceFixture source, MidoriProgramTree&& program, std::vector<CompilerWarning>&& warnings, std::vector<CompilerError>&& errors);
	};

	struct ExecutedSnippet
	{
		SourceFixture m_source;
		int m_exit_code = 0;
		CapturedOutput m_output;

		ExecutedSnippet(SourceFixture source, int exit_code, CapturedOutput output);
	};

	[[nodiscard]] std::expected<LexedSnippet, CompilerError> LexSnippet(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] std::expected<ParsedSnippet, CompilerError> ParseSnippet(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] std::expected<TypedSnippet, MidoriResult::CompilerDiagnostics> TypeCheckSnippetWithDiagnostics(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] std::expected<TypedSnippet, CompilerError> TypeCheckSnippet(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] std::expected<AnalyzedSnippet, CompilerError> AnalyzeSnippet(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] MidoriResult::CompilerResult CompileSnippet(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] std::expected<ExecutedSnippet, CompilerError> ExecuteSnippet(std::string source_code, std::string file_name = "Test.mdr");

	[[nodiscard]] std::vector<Token::Name> CollectTokenNames(const TokenStream& tokens, bool include_whitespace = false, bool include_end_of_file = false);
}
