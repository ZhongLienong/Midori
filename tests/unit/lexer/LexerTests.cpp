#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include "Compiler/Token/Token.h"
#include "support/CompileHelpers.h"

#include <string>
#include <string_view>
#include <vector>

using Catch::Matchers::ContainsSubstring;

namespace
{
	const Token* FindTokenByLexeme(const TokenStream& tokens, std::string_view lexeme)
	{
		for (int index = 0; index < tokens.Size(); index += 1)
		{
			const Token& token = tokens[index];
			if (token.m_lexeme == lexeme)
			{
				return &token;
			}
		}

		return nullptr;
	}
}

TEST_CASE("Lexer tokenizes literal variants and decodes string escapes", "[lexer]")
{
	const std::string source_code =
		R"(def values = [42, 0x2A, 0b1010, 3.5, "line\n\t\"\\q"];
)";

	std::expected<MidoriTest::LexedSnippet, CompilerError> lex_result = MidoriTest::LexSnippet(source_code, "LiteralForms.mdr");
	if (!lex_result.has_value())
	{
		FAIL(std::string(lex_result.error().Rendered()));
	}

	const TokenStream& tokens = lex_result->m_tokens;
	const std::vector<Token::Name> expected_names
	{
		Token::Name::DEF,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::SINGLE_EQUAL,
		Token::Name::LEFT_BRACKET,
		Token::Name::INTEGER_LITERAL,
		Token::Name::COMMA,
		Token::Name::INTEGER_LITERAL,
		Token::Name::COMMA,
		Token::Name::INTEGER_LITERAL,
		Token::Name::COMMA,
		Token::Name::FLOAT_LITERAL,
		Token::Name::COMMA,
		Token::Name::TEXT_LITERAL,
		Token::Name::RIGHT_BRACKET,
		Token::Name::SINGLE_SEMICOLON
	};

	REQUIRE(MidoriTest::CollectTokenNames(tokens) == expected_names);
	CHECK(tokens[4].m_lexeme == "42");
	CHECK(tokens[6].m_lexeme == "0x2A");
	CHECK(tokens[8].m_lexeme == "0b1010");
	CHECK(tokens[10].m_lexeme == "3.5");
	CHECK(tokens[12].m_lexeme == "line\n\t\"\\q");
}

TEST_CASE("Lexer skips comments and preserves token line numbers", "[lexer]")
{
	const std::string source_code =
		R"(module Commented
// this line comment should disappear
def value = 1;
/* block comment
still in the comment */
def other = 2;
)";

	std::expected<MidoriTest::LexedSnippet, CompilerError> lex_result = MidoriTest::LexSnippet(source_code, "Comments.mdr");
	if (!lex_result.has_value())
	{
		FAIL(std::string(lex_result.error().Rendered()));
	}

	const TokenStream& tokens = lex_result->m_tokens;
	const std::vector<Token::Name> expected_names
	{
		Token::Name::MODULE,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::DEF,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::SINGLE_EQUAL,
		Token::Name::INTEGER_LITERAL,
		Token::Name::SINGLE_SEMICOLON,
		Token::Name::DEF,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::SINGLE_EQUAL,
		Token::Name::INTEGER_LITERAL,
		Token::Name::SINGLE_SEMICOLON
	};

	REQUIRE(MidoriTest::CollectTokenNames(tokens) == expected_names);

	const Token* other_token = FindTokenByLexeme(tokens, "other");
	REQUIRE(other_token != nullptr);
	CHECK(other_token->m_line == 6);
}

TEST_CASE("Lexer records exact token columns and source spans", "[lexer]")
{
	const std::string source_code =
		R"(def alpha = alpha + alpha;
)";

	std::expected<MidoriTest::LexedSnippet, CompilerError> lex_result = MidoriTest::LexSnippet(source_code, "TokenSpans.mdr");
	if (!lex_result.has_value())
	{
		FAIL(std::string(lex_result.error().Rendered()));
	}

	const TokenStream& tokens = lex_result->m_tokens;
	REQUIRE(tokens.Size() == 8);

	CHECK(tokens[1].m_column == 4);
	CHECK(tokens[1].m_source_length == 5u);
	CHECK(tokens[3].m_column == 12);
	CHECK(tokens[3].m_source_length == 5u);
	CHECK(tokens[5].m_column == 20);
	CHECK(tokens[5].m_source_length == 5u);
	CHECK(tokens[7].m_token_name == Token::Name::END_OF_FILE);
	CHECK(tokens[7].m_column == 26);
	CHECK(tokens[7].m_source_length == 0u);
}

TEST_CASE("Lexer recognizes compound operator tokens used by later stages", "[lexer]")
{
	const std::string source_code =
		R"(value =++ next;
total += 1;
mask <~= 2;
flags ~>= 3;
stream |> sink;
)";

	std::expected<MidoriTest::LexedSnippet, CompilerError> lex_result = MidoriTest::LexSnippet(source_code, "Operators.mdr");
	if (!lex_result.has_value())
	{
		FAIL(std::string(lex_result.error().Rendered()));
	}

	const std::vector<Token::Name> expected_names
	{
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::EQUAL_PLUS_PLUS,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::SINGLE_SEMICOLON,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::PLUS_EQUAL,
		Token::Name::INTEGER_LITERAL,
		Token::Name::SINGLE_SEMICOLON,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::LEFT_SHIFT_EQUAL,
		Token::Name::INTEGER_LITERAL,
		Token::Name::SINGLE_SEMICOLON,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::RIGHT_SHIFT_EQUAL,
		Token::Name::INTEGER_LITERAL,
		Token::Name::SINGLE_SEMICOLON,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::BAR_BRACKET,
		Token::Name::IDENTIFIER_LITERAL,
		Token::Name::SINGLE_SEMICOLON
	};

	REQUIRE(MidoriTest::CollectTokenNames(lex_result->m_tokens) == expected_names);
}

TEST_CASE("Lexer reports the dedicated =+ typo diagnostic", "[lexer]")
{
	const std::string source_code =
		R"(def value =+ 1;
)";

	std::expected<MidoriTest::LexedSnippet, CompilerError> lex_result = MidoriTest::LexSnippet(source_code, "EqualPlusTypo.mdr");

	REQUIRE_FALSE(lex_result.has_value());

	const CompilerError& error = lex_result.error();
	REQUIRE(error.m_stage == CompilerStage::Lexer);
	REQUIRE(error.m_location.has_value());
	CHECK(error.m_location->m_file_name == "EqualPlusTypo.mdr");
	CHECK(error.m_location->m_line == 1);

	const std::string rendered_error = std::string(error.Rendered());
	CHECK_THAT(rendered_error, ContainsSubstring("Lexer Error"));
	CHECK_THAT(rendered_error, ContainsSubstring("Unexpected character '=+' (did you mean '=++'?)"));
	CHECK_THAT(rendered_error, ContainsSubstring("EqualPlusTypo.mdr:1"));
	CHECK_THAT(rendered_error, ContainsSubstring("def value =+ 1;"));
}
