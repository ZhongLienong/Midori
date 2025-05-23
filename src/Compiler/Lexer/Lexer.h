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
	int m_line = 1;
	size_t m_begin = 0u;
	size_t m_current = 0u;
	bool m_is_main_program = true;
	static const std::unordered_map<std::string, Token::Name> s_keywords;

public:

	Lexer(std::string&& source_code, bool is_main_program = true) noexcept : m_source_code(std::move(source_code)), m_is_main_program(is_main_program) {}

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