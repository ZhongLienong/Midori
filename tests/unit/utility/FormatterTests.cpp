#include <catch2/catch_test_macros.hpp>

#include "Utility/Formatter/Formatter.h"

#include <expected>
#include <string>
#include <string_view>

namespace
{
	[[nodiscard]] std::string FormatOrFail(std::string_view source_code, std::string_view file_name)
	{
		const std::expected<std::string, CompilerError> format_result =
			MidoriFormatter::FormatSource(source_code, file_name);
		if (!format_result.has_value())
		{
			FAIL(std::string(format_result.error().Rendered()));
		}
		return format_result.value();
	}

	void RequireIdempotent(std::string_view source_code, std::string_view file_name)
	{
		const std::string first_pass = FormatOrFail(source_code, file_name);
		const std::string second_pass = FormatOrFail(first_pass, file_name);
		CHECK(first_pass == second_pass);
	}
}

TEST_CASE("Formatter preserves comments while normalizing spacing", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"// heading\n"
		"defun main():Int=>{\n"
		"def value=/*inline*/1; // trailing\n"
		"value\n"
		"};\n";

	const std::string formatted = FormatOrFail(source_code, "CommentedFormat.mdr");

	const std::string expected =
		"module Main\n"
		"// heading\n"
		"defun main(): Int => {\n"
		"    def value = /*inline*/ 1;  // trailing\n"
		"    value\n"
		"};\n";

	CHECK(formatted == expected);
}

TEST_CASE("Formatter is idempotent for comment-bearing files", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"/* banner */ defun main():Int=>0;\n";

	RequireIdempotent(source_code, "FormatterIdempotent.mdr");
}

TEST_CASE("Formatter handles an empty input", "[formatter][edge]")
{
	const std::string formatted = FormatOrFail("", "Empty.mdr");
	CHECK(formatted.empty());
	RequireIdempotent("", "Empty.mdr");
}

TEST_CASE("Formatter handles a whitespace-only input", "[formatter][edge]")
{
	const std::string formatted = FormatOrFail("\n\n\n   \t\n", "Whitespace.mdr");
	CHECK(formatted.empty());
	RequireIdempotent("\n\n\n   \t\n", "Whitespace.mdr");
}

TEST_CASE("Formatter handles a comment-only file", "[formatter][edge][comments]")
{
	const std::string source_code =
		"// first\n"
		"// second\n"
		"/* block */\n";

	const std::string formatted = FormatOrFail(source_code, "CommentsOnly.mdr");

	CHECK(formatted ==
		"// first\n"
		"// second\n"
		"/* block */\n");
	RequireIdempotent(source_code, "CommentsOnly.mdr");
}

TEST_CASE("Formatter is idempotent for deeply nested expressions", "[formatter][edge]")
{
	const std::string source_code =
		"module DeepNest\n"
		"defun deep(): Int => ((((((((((1 + 2))))))))));\n";

	const std::string formatted = FormatOrFail(source_code, "DeepNest.mdr");
	CHECK(formatted.find("(((((((((1 + 2))))))))") != std::string::npos);
	RequireIdempotent(source_code, "DeepNest.mdr");
}

TEST_CASE("Formatter preserves long expression lines without wrapping", "[formatter][edge]")
{
	const std::string long_addition =
		"def total = 1111111111 + 2222222222 + 3333333333 + 4444444444 + 5555555555 + 6666666666;\n";
	const std::string source_code = std::string("module Long\n") + long_addition;

	const std::string formatted = FormatOrFail(source_code, "Long.mdr");
	CHECK(formatted.find("1111111111 + 2222222222 + 3333333333") != std::string::npos);
	RequireIdempotent(source_code, "Long.mdr");
}

TEST_CASE("Formatter normalizes struct field spacing and is idempotent", "[formatter][edge]")
{
	const std::string source_code =
		"module Shapes\n"
		"struct Point{x:Int,y:Int};\n";

	const std::string formatted = FormatOrFail(source_code, "Shapes.mdr");
	CHECK(formatted.find("struct Point {") != std::string::npos);
	CHECK(formatted.find("x: Int,") != std::string::npos);
	RequireIdempotent(source_code, "Shapes.mdr");
}

TEST_CASE("Formatter is idempotent for match arms", "[formatter][edge]")
{
	const std::string source_code =
		"module MatchArms\n"
		"def value = match 0 with case 0 => 0 case 1 => 1 default => 2;\n";

	const std::string first_pass = FormatOrFail(source_code, "MatchArms.mdr");
	CHECK(first_pass.find("case 0 => 0") != std::string::npos);
	CHECK(first_pass.find("default => 2") != std::string::npos);
	RequireIdempotent(source_code, "MatchArms.mdr");
}

TEST_CASE("Formatter keeps inline comments inline between tokens", "[formatter][comments]")
{
	const std::string source_code =
		"module Inline\n"
		"def x = 1 /* keep */ + 2;\n";

	const std::string formatted = FormatOrFail(source_code, "Inline.mdr");
	CHECK(formatted.find("1 /* keep */ + 2") != std::string::npos);
	RequireIdempotent(source_code, "Inline.mdr");
}

TEST_CASE("Formatter preserves trailing line comments after writing once", "[formatter][comments]")
{
	const std::string source_code =
		"module Trail\n"
		"def x = 1; // tail\n"
		"def y = 2;\n";

	const std::string first_pass = FormatOrFail(source_code, "Trail.mdr");
	CHECK(first_pass.find("// tail") != std::string::npos);
	RequireIdempotent(source_code, "Trail.mdr");
}

TEST_CASE("Formatter normalizes string escape sequences", "[formatter][edge]")
{
	const std::string source_code =
		"module Strings\n"
		"def s = \"a\nb\tc\";\n";

	const std::string formatted = FormatOrFail(source_code, "Strings.mdr");
	CHECK(formatted.find("\"a\\nb\\tc\"") != std::string::npos);
	RequireIdempotent(source_code, "Strings.mdr");
}

TEST_CASE("Formatter round-trips newtype declarations and leading-bar sums", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"type Meters=Int;\n"
		"type Solo=|Only(Int);\n";

	const std::string formatted = FormatOrFail(source_code, "NewtypeFormat.mdr");

	const std::string expected =
		"module Main\n"
		"type Meters = Int;\n"
		"type Solo = | Only(Int);\n";

	CHECK(formatted == expected);
}

TEST_CASE("Formatter is idempotent for newtype declarations", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"type Meters = Int;\n"
		"type Solo = | Only(Int);\n";

	const std::string once = FormatOrFail(source_code, "NewtypeIdempotent.mdr");
	const std::string twice = FormatOrFail(once, "NewtypeIdempotent.mdr");

	CHECK(once == twice);
}
