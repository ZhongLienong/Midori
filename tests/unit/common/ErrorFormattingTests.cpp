#include <catch2/catch_test_macros.hpp>

#include "Common/Error/Error.h"
#include "Compiler/Token/Token.h"

#include <string>
#include <string_view>
#include <vector>

namespace
{
	std::string StripAnsiCodes(std::string_view text)
	{
		std::string stripped;
		stripped.reserve(text.size());

		bool in_escape = false;
		for (const char ch : text)
		{
			if (ch == '\033')
			{
				in_escape = true;
				continue;
			}

			if (in_escape)
			{
				if (ch == 'm')
				{
					in_escape = false;
				}
				continue;
			}

			stripped.push_back(ch);
		}

		return stripped;
	}
}

TEST_CASE("CompilerError WithContext renders source, caret, and suggestion", "[error][format]")
{
	const CompilerError error = CompilerError::WithContext(
		CompilerStage::Parser,
		"Expected expression",
		2,
		"Format.mdr",
		4,
		3u,
		"Try adding a literal",
		"def value = ;");

	const std::string expected_render =
		"Parser Error at Format.mdr:2\n"
		"  |\n"
		"2 | def value = ;\n"
		"  |     ^^^ Expected expression\n"
		"  |\n"
		"  | Try adding a literal\n";

	REQUIRE(StripAnsiCodes(error.Rendered()) == expected_render);
}

TEST_CASE("CompilerWarning WithToken highlights the matching token span", "[warning][format]")
{
	const Token token(std::string("shadowed"), Token::Name::IDENTIFIER_LITERAL, 1, "Warning.mdr");
	const std::vector<std::string> source_lines
	{
		"def shadowed = value"
	};

	const CompilerWarning warning = CompilerWarning::WithToken(
		CompilerStage::StaticAnalyzer,
		"Unused local",
		token,
		"Warning.mdr",
		source_lines,
		"Prefix with '_' if intentional");

	const std::string expected_render =
		"Static Analyzer Warning at Warning.mdr:1\n"
		"  |\n"
		"1 | def shadowed = value\n"
		"  |     ^^^^^^^^ Unused local\n"
		"  |\n"
		"  | Prefix with '_' if intentional\n";

	REQUIRE(StripAnsiCodes(warning.Rendered()) == expected_render);
}

TEST_CASE("Simple compiler diagnostics render as plain messages", "[error][warning][format]")
{
	const CompilerError error = CompilerError::Simple(CompilerStage::Module, "Missing import");
	const CompilerWarning warning = CompilerWarning::Simple(CompilerStage::Optimizer, "Dead store removed");

	REQUIRE(error.Rendered() == "Missing import");
	REQUIRE(warning.Rendered() == "Dead store removed");
}
