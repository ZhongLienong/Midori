#pragma once

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"

#include <string>
#include <unordered_map>
#include <vector>

class Lexer
{
private:
	const std::string m_source_code;
	const std::string m_file_name;
	const std::vector<std::string> m_source_lines;
	static const std::unordered_map<std::string, Token::Name> s_keywords;
	int m_current = 0;
	int m_begin = 0;
	int m_line_start = 0;
	int m_line = 1;

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

	MidoriResult::TokenResult MakeTokenResult(Token::Name type) const;

	MidoriResult::TokenResult MakeTokenResult(Token::Name type, std::string&& lexeme) const;

	MidoriResult::TokenResult LexOneToken();

	MidoriResult::TokenResult SkipWhitespaceAndComments();

	MidoriResult::TokenResult MatchString();

	MidoriResult::TokenResult MatchNumber();

	MidoriResult::TokenResult MatchIdentifierOrReserved();

	template<typename Predicate>
	int ConsumeWhile(Predicate&& pred);

	MidoriResult::TokenResult MatchStringRecursive(std::string&& acc);

	MidoriResult::TokenResult SkipLineComment();

	MidoriResult::TokenResult SkipBlockComment();

	int ConsumeDigits();

	int ConsumeAlphaNumeric();

	MidoriResult::LexerResult LexRecursive(TokenStream&& tokens, std::string&& errors);

	std::vector<std::string> SplitIntoLines(const std::string& source);
};