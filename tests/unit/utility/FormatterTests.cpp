#include <catch2/catch_test_macros.hpp>

#include "Utility/Formatter/Formatter.h"

#include <expected>
#include <string>

TEST_CASE("Formatter preserves comments while normalizing spacing", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"// heading\n"
		"defun main():Int=>{\n"
		"def value=/*inline*/1; // trailing\n"
		"value\n"
		"};\n";

	const std::expected<std::string, CompilerError> format_result =
		MidoriFormatter::FormatSource(source_code, "CommentedFormat.mdr");
	if (!format_result.has_value())
	{
		FAIL(std::string(format_result.error().Rendered()));
	}

	const std::string expected =
		"module Main\n"
		"// heading\n"
		"defun main(): Int => {\n"
		"    def value = /*inline*/ 1;  // trailing\n"
		"    value\n"
		"};\n";

	CHECK(format_result.value() == expected);
}

TEST_CASE("Formatter is idempotent for comment-bearing files", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"/* banner */ defun main():Int=>0;\n";

	const std::expected<std::string, CompilerError> first_pass =
		MidoriFormatter::FormatSource(source_code, "FormatterIdempotent.mdr");
	if (!first_pass.has_value())
	{
		FAIL(std::string(first_pass.error().Rendered()));
	}

	const std::expected<std::string, CompilerError> second_pass =
		MidoriFormatter::FormatSource(first_pass.value(), "FormatterIdempotent.mdr");
	if (!second_pass.has_value())
	{
		FAIL(std::string(second_pass.error().Rendered()));
	}

	CHECK(second_pass.value() == first_pass.value());
}
