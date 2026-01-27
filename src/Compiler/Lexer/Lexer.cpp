#include "Lexer.h"
#include <sstream>

using namespace std::string_literals;

const std::unordered_map<std::string, Token::Name> Lexer::s_keywords =
{
	// types
	{"Float"s, Token::Name::FLOAT},
	{"Int"s, Token::Name::INTEGER},
	{"Byte"s, Token::Name::BYTE},
	{"Word"s, Token::Name::WORD},
	{"Text"s, Token::Name::TEXT},
	{"Bool"s, Token::Name::BOOL},
	{"Unit"s, Token::Name::UNIT},
	{"Array"s, Token::Name::ARRAY},
	{"Future"s, Token::Name::FUTURE},
	{"Never"s, Token::Name::NEVER},

	// reserved keywords
	{"else"s, Token::Name::ELSE},
	{"false"s, Token::Name::FALSE},
	{"loop"s, Token::Name::LOOP},
	{"for"s, Token::Name::FOR},
	{"in"s, Token::Name::IN},
	{"if"s, Token::Name::IF},
	{"return"s, Token::Name::RETURN},
	{"true"s, Token::Name::TRUE},
	{"def"s, Token::Name::DEF},
	{"defun"s, Token::Name::DEFUN},
	{"fn"s, Token::Name::FUNCTION},
	{"as"s, Token::Name::AS},
	{"break"s, Token::Name::BREAK},
	{"continue"s, Token::Name::CONTINUE},
	{"import"s, Token::Name::IMPORT},
	{"struct"s, Token::Name::STRUCT},
	{"union"s, Token::Name::UNION},
	{"new"s, Token::Name::NEW},
	{"foreign"s, Token::Name::FOREIGN},
	{"case"s, Token::Name::CASE},
	{"default"s, Token::Name::DEFAULT},
	{"then"s, Token::Name::THEN},
	{"with"s, Token::Name::WITH},
	{"match"s, Token::Name::MATCH},
	{"module"s, Token::Name::MODULE},
	{"export"s, Token::Name::EXPORT},
	{"public"s, Token::Name::PUBLIC},
	{"private"s, Token::Name::PRIVATE},
	{"use"s, Token::Name::USE},
	{"class"s, Token::Name::CLASS},
	{"instance"s, Token::Name::INSTANCE},
	{"where"s, Token::Name::WHERE},
	{"type"s, Token::Name::TYPE},
	{"async"s, Token::Name::ASYNC},
	{"await"s, Token::Name::AWAIT}
};

template<typename Predicate>
int Lexer::ConsumeWhile(Predicate&& pred)
{
	int count = 0;
	while (!IsAtEnd(0) && pred(LookAhead(0)))
	{
		Advance();
		count += 1;
	}
	return count;
}

bool Lexer::IsAtEnd(int offset) const
{
	return static_cast<size_t>(m_current + offset) >= m_source_code.size();
}

bool Lexer::IsDigit(char c) const
{
	return c >= '0' && c <= '9';
}

bool Lexer::IsAlpha(char c) const
{
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

bool Lexer::IsAlphaNumeric(char c) const
{
	return IsDigit(c) || IsAlpha(c);
}

char Lexer::Advance()
{
	return m_source_code[m_current++];
}

char Lexer::LookAhead(int offset) const
{
	return IsAtEnd(offset) ? '\0' : m_source_code[static_cast<size_t>(m_current + offset)];
}

bool Lexer::MatchNext(char expected)
{
	if (!IsAtEnd(0) && m_source_code[m_current] == expected)
	{
		m_current += 1;
		return true;
	}
	return false;
}

Token Lexer::MakeToken(Token::Name type) const
{
	return Token(m_source_code.substr(m_begin, m_current - m_begin), type, m_line, m_file_name);
}

Token Lexer::MakeToken(Token::Name type, std::string&& lexeme) const
{
	return Token(std::move(lexeme), type, m_line, m_file_name);
}

MidoriResult::TokenResult Lexer::MakeTokenResult(Token::Name type) const
{
	return MakeToken(type);
}

MidoriResult::TokenResult Lexer::MakeTokenResult(Token::Name type, std::string&& lexeme) const
{
	return MakeToken(type, std::move(lexeme));
}

int Lexer::ConsumeDigits()
{
	return ConsumeWhile([this](char c) { return IsDigit(c); });
}

int Lexer::ConsumeAlphaNumeric()
{
	return ConsumeWhile([this](char c) { return IsAlphaNumeric(c); });
}

MidoriResult::TokenResult Lexer::SkipLineComment()
{
	ConsumeWhile([](char c) { return c != '\n'; });
	return Token(" "s, Token::Name::WHITESPACE, m_line, m_file_name);
}

MidoriResult::TokenResult Lexer::SkipBlockComment()
{
	while (true)
	{
		if (LookAhead(0) == '*' && LookAhead(1) == '/')
		{
			Advance(); // '*'
			Advance(); // '/'
			return Token(" "s, Token::Name::WHITESPACE, m_line, m_file_name);
		}

		if (IsAtEnd(0))
		{
			int column = m_current - m_line_start;
			return std::unexpected<std::string>(MidoriError::GenerateLexerErrorWithContext("Unterminated block comment", m_line, column, m_file_name, m_source_lines));
		}

		if (LookAhead(0) == '\n')
		{
			m_line += 1;
			m_line_start = m_current + 1;
		}

		Advance();
	}
}

MidoriResult::TokenResult Lexer::SkipWhitespaceAndComments()
{
	while (true)
	{
		if (IsAtEnd(0))
		{
			return Token(" "s, Token::Name::WHITESPACE, m_line, m_file_name);
		}

		char c = LookAhead(0);

		switch (c)
		{
		case ' ':
		case '\r':
		case '\t':
			Advance();
			continue;
		case '\n':
			m_line += 1;
			Advance();
			m_line_start = m_current;
			continue;
		case '/':
			if (LookAhead(1) == '/')
			{
				Advance(); // '/'
				Advance(); // '/'
				(void)SkipLineComment();
				continue;
			}
			else if (LookAhead(1) == '*')
			{
				Advance(); // '/'
				Advance(); // '*'
				MidoriResult::TokenResult result = SkipBlockComment();
				if (!result.has_value())
				{
					return result;
				}
				continue;
			}
			return Token(" "s, Token::Name::WHITESPACE, m_line, m_file_name);
		default:
			return Token(" "s, Token::Name::WHITESPACE, m_line, m_file_name);
		}
	}
}

MidoriResult::TokenResult Lexer::MatchStringRecursive(std::string&& acc)
{
	while (true)
	{
		if (IsAtEnd(0))
		{
			int column = m_begin - m_line_start;
			return std::unexpected<std::string>(MidoriError::GenerateLexerErrorWithContext("Unterminated string", m_line, column, m_file_name, m_source_lines));
		}

		if (LookAhead(0) == '"')
		{
			Advance();
			return MakeToken(Token::Name::TEXT_LITERAL, std::move(acc));
		}

		if (LookAhead(0) == '\n')
		{
			m_line += 1;
		}

		if (LookAhead(0) == '\\' && !IsAtEnd(1))
		{
			char escape_char = LookAhead(1);
			char escaped_value;
			bool add_backslash = false;

			switch (escape_char)
			{
			case 't':
				escaped_value = '\t';
				break;
			case 'n':
				escaped_value = '\n';
				break;
			case 'b':
				escaped_value = '\b';
				break;
			case 'f':
				escaped_value = '\f';
				break;
			case '"':
				escaped_value = '"';
				break;
			case '\\':
				escaped_value = '\\';
				break;
			default:
				add_backslash = true;
				escaped_value = escape_char;
				break;
			}

			if (add_backslash)
			{
				acc += '\\';
			}
			acc += escaped_value;

			Advance(); // Skip the backslash
			Advance(); // Skip the escape character
			continue;
		}

		acc += Advance();
	}
}

MidoriResult::TokenResult Lexer::MatchString()
{
	return MatchStringRecursive(std::string{});
}

MidoriResult::TokenResult Lexer::MatchNumber()
{
	if (LookAhead(-1) == '0' && (LookAhead(0) == 'x' || LookAhead(0) == 'X'))
	{
		Advance();
		ConsumeWhile([](char c) { return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); });
		return MakeTokenResult(Token::Name::INTEGER_LITERAL);
	}
	else if (LookAhead(-1) == '0' && (LookAhead(0) == 'b' || LookAhead(0) == 'B'))
	{
		Advance(); 
		ConsumeWhile([](char c) { return c == '0' || c == '1'; });
		return MakeTokenResult(Token::Name::INTEGER_LITERAL);
	}
	else
	{
		// Normal decimal number
		ConsumeDigits();

		// Look for a decimal part.
		bool is_float = LookAhead(0) == '.' && IsDigit(LookAhead(1));

		return (is_float
			? (Advance(), ConsumeDigits(), MakeTokenResult(Token::Name::FLOAT_LITERAL))
			: MakeTokenResult(Token::Name::INTEGER_LITERAL));
	}
}

MidoriResult::TokenResult Lexer::MatchIdentifierOrReserved()
{
	ConsumeAlphaNumeric();

	std::string identifier = m_source_code.substr(m_begin, m_current - m_begin);

	return s_keywords.contains(identifier)
		? MakeTokenResult(s_keywords.at(identifier))
		: MakeTokenResult(Token::Name::IDENTIFIER_LITERAL);
}

MidoriResult::TokenResult Lexer::LexOneToken()
{
	return SkipWhitespaceAndComments()
		.and_then
		(
			[this](Token&&) ->MidoriResult::TokenResult
			{
				m_begin = m_current;

				char next_char = Advance();
				switch (next_char)
				{
				case '(':
					return MakeTokenResult(Token::Name::LEFT_PAREN);
				case ')':
					return MakeTokenResult(Token::Name::RIGHT_PAREN);
				case '{':
					return MakeTokenResult(Token::Name::LEFT_BRACE);
				case '}':
					return MakeTokenResult(Token::Name::RIGHT_BRACE);
				case '[':
					return MakeTokenResult(Token::Name::LEFT_BRACKET);
				case ']':
					return MakeTokenResult(Token::Name::RIGHT_BRACKET);
				case ',':
					return MakeTokenResult(Token::Name::COMMA);
				case '.':
				{
					if (IsDigit(LookAhead(0)))
					{
						return MatchNumber();
					}
					else if (MatchNext('.'))
					{
						return MakeTokenResult(Token::Name::DOUBLE_DOT);
					}
					else
					{
						return MakeTokenResult(Token::Name::SINGLE_DOT);
					}
				}
				case ';':
					return MakeTokenResult(Token::Name::SINGLE_SEMICOLON);
				case '+':
				{
					if (MatchNext('+'))
					{
						if (MatchNext('='))
						{
							return MakeTokenResult(Token::Name::PLUS_PLUS_EQUAL);
						}
						else
						{
							return MakeTokenResult(Token::Name::DOUBLE_PLUS);
						}
					}
					else
					{
						if (MatchNext('='))
						{
							return MakeTokenResult(Token::Name::PLUS_EQUAL);
						}
						else
						{
							return MakeTokenResult(Token::Name::SINGLE_PLUS);
						}
					}
				}
				case '-':
				{
					if (MatchNext('>'))
					{
						return MakeTokenResult(Token::Name::THIN_ARROW);
					}
					else if (MatchNext('-'))
					{
						return MakeTokenResult(Token::Name::DOUBLE_MINUS);
					}
					else if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::MINUS_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::SINGLE_MINUS);
					}
				}
				case ':':
				{
					if (MatchNext(':'))
					{
						return MakeTokenResult(Token::Name::DOUBLE_COLON);
					}
					else
					{
						return MakeTokenResult(Token::Name::SINGLE_COLON);
					}
				}
				case '%':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::PERCENT_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::PERCENT);
					}
				}
				case '*':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::STAR_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::STAR);
					}
				}
				case '/':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::SLASH_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::SLASH);
					}
				}
				case '|':
				{
					if (MatchNext('|'))
					{
						return MakeTokenResult(Token::Name::DOUBLE_BAR);
					}
					else if (MatchNext('>'))
					{
						return MakeTokenResult(Token::Name::BAR_BRACKET);
					}
					else if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::BAR_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::SINGLE_BAR);
					}
				}
				case '^':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::CARET_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::CARET);
					}
				}
				case '&':
				{
					if (MatchNext('&'))
					{
						return MakeTokenResult(Token::Name::DOUBLE_AMPERSAND);
					}
					else if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::AMPERSAND_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::SINGLE_AMPERSAND);
					}
				}
				case '!':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::BANG_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::BANG);
					}
				}
				case '=':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::DOUBLE_EQUAL);
					}
					else if (MatchNext('>'))
					{
						return MakeTokenResult(Token::Name::FAT_ARROW);
					}
					else if (MatchNext('+'))
					{
						if (MatchNext('+'))
						{
							return MakeTokenResult(Token::Name::EQUAL_PLUS_PLUS);
						}
						else
						{
							return std::unexpected<std::string>(MidoriError::GenerateLexerErrorWithContext("Unexpected character '=+' (did you mean '=++'?)", m_line, m_begin - m_line_start, m_file_name, m_source_lines));
						}
					}
					else
					{
						return MakeTokenResult(Token::Name::SINGLE_EQUAL);
					}
				}
				case '>':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::GREATER_EQUAL);
					}
					else
					{
						return MakeTokenResult(Token::Name::RIGHT_ANGLE);
					}
				}
				case '<':
				{
					if (MatchNext('='))
					{
						return MakeTokenResult(Token::Name::LESS_EQUAL);
					}
					else if (MatchNext('~'))
					{
						if (MatchNext('='))
						{
							return MakeTokenResult(Token::Name::LEFT_SHIFT_EQUAL);
						}
						else
						{
							return MakeTokenResult(Token::Name::LEFT_SHIFT);
						}
					}
					else
					{
						return MakeTokenResult(Token::Name::LEFT_ANGLE);
					}
				}
				case '~':
				{
					if (MatchNext('>'))
					{
						if (MatchNext('='))
						{
							return MakeTokenResult(Token::Name::RIGHT_SHIFT_EQUAL);
						}
						else
						{
							return MakeTokenResult(Token::Name::RIGHT_SHIFT);
						}
					}
					else
					{
						return MakeTokenResult(Token::Name::TILDE);
					}
				}
				case '#':
					return MakeTokenResult(Token::Name::HASH);
				case '"':
					return MatchString();
				default:
				{
					if (IsDigit(next_char))
					{
						return MatchNumber();
					}
					else if (IsAlpha(next_char))
					{
						return MatchIdentifierOrReserved();
					}
					else if (next_char == '\0')
					{
						return MakeTokenResult(Token::Name::END_OF_FILE);
					}
					else
					{
						return std::unexpected<std::string>(MidoriError::GenerateLexerErrorWithContext("Invalid character: "s + next_char, m_line, m_current - m_line_start, m_file_name, m_source_lines));
					}
				}
				}
			}
		);
}

std::vector<std::string> Lexer::SplitIntoLines(const std::string& source)
{
	std::istringstream iss(source);
	std::vector<std::string> lines;
	std::string line;

	for (; std::getline(iss, line); )
	{
		lines.emplace_back(std::move(line));
	}

	return lines;
}

Lexer::Lexer(std::string&& source_code, std::string_view file_name) noexcept
	: m_source_code(std::move(source_code)),
	m_file_name(file_name),
	m_source_lines(SplitIntoLines(m_source_code))
{
}

MidoriResult::LexerResult Lexer::LexRecursive(TokenStream&& tokens, std::string&& errors)
{
	while (!IsAtEnd(0))
	{
		MidoriResult::TokenResult token_result = LexOneToken();

		if (token_result.has_value())
		{
			tokens.AddToken(std::move(token_result.value()));
		}
		else
		{
			errors.append(std::move(token_result.error())).append("\n");
		}
	}

	if (!errors.empty())
	{
		return std::unexpected<std::string>(std::move(errors));
	}

	if (tokens.Size() == 0 || (std::prev(tokens.cend())->m_token_name != Token::Name::END_OF_FILE))
	{
		tokens.AddToken(MakeToken(Token::Name::END_OF_FILE));
	}

	return std::move(tokens);
}

MidoriResult::LexerResult Lexer::Lex()
{
	return LexRecursive(TokenStream{}, std::string{});
}
