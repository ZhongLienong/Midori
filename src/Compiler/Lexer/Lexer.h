#pragma once

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"

#include <string>
#include <unordered_map>
#include <vector>

class Lexer
{
private:
	std::string m_source_code;
	std::string m_file_name;
	std::vector<std::string> m_source_lines;
	int m_line = 1;
	size_t m_begin = 0u;
	size_t m_current = 0u;
	size_t m_line_start = 0u;
	static const std::unordered_map<std::string, Token::Name> s_keywords;

public:

	Lexer(std::string&& source_code, std::string_view file_name) noexcept;

	MidoriResult::LexerResult Lex();

private:

	bool IsAtEnd(int offset) const;

	bool IsDigit(char c) const;

	bool IsAlpha(char c) const;

	bool IsAlphaNumeric(char c) const;

	char Advance();

	char LookAhead(int offset) const;

	bool MatchNext(char expected);

	Token MakeToken(Token::Name type) const;

	Token MakeToken(Token::Name type, std::string&& lexeme) const;

	MidoriResult::TokenResult LexOneToken();

	MidoriResult::TokenResult SkipWhitespaceAndComments();

	MidoriResult::TokenResult MatchString();

	Token MatchNumber();

	Token MatchIdentifierOrReserved();
};