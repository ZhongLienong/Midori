#include "Lexer.h"

using namespace std::string_literals;

const std::unordered_map<std::string, Token::Name> Lexer::s_keywords =
{
	// types
	{"Float"s, Token::Name::FLOAT},
	{"Int"s, Token::Name::INTEGER},
	{"Text"s, Token::Name::TEXT},
	{"Bool"s, Token::Name::BOOL},
	{"Unit"s, Token::Name::UNIT},
	{"Array"s, Token::Name::ARRAY},

	// reserved keywords
	{"else"s, Token::Name::ELSE},
	{"false"s, Token::Name::FALSE},
	{"loop"s, Token::Name::LOOP},
	{"if"s, Token::Name::IF},
	{"return"s, Token::Name::RETURN},
	{"true"s, Token::Name::TRUE},
	{"def"s, Token::Name::DEF},
	{"fn"s, Token::Name::FUNCTION},
	{"do"s, Token::Name::DO},
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
	{"switch"s, Token::Name::SWITCH},
	{"namespace"s, Token::Name::NAMESPACE},
};

bool Lexer::IsAtEnd(int offset) const
{
	return m_current + static_cast<size_t>(offset) >= m_source_code.size();
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
	return (IsAtEnd(0u) || m_current + static_cast<size_t>(offset) >= m_source_code.size()) ? '\0' : m_source_code[m_current + static_cast<size_t>(offset)];
}

bool Lexer::MatchNext(char expected)
{
	return ((IsAtEnd(0u) || m_source_code[m_current] != expected) ? false : (++m_current, true));
}

Token Lexer::MakeToken(Token::Name type) const
{
	return Token(m_source_code.substr(m_begin, m_current - m_begin), type, m_line);
}

Token Lexer::MakeToken(Token::Name type, std::string&& lexeme) const
{
	return Token(std::move(lexeme), type, m_line);
}

MidoriResult::TokenResult Lexer::SkipWhitespaceAndComments()
{
	while (!IsAtEnd(0))
	{
		char c = LookAhead(0);

		switch (c)
		{
		case ' ':
		case '\r':
		case '\t':
			Advance();
			break;
		case '\n':
			m_line++;
			Advance();
			break;
		case '/':
			if (LookAhead(1) == '/')
			{
				// A comment goes until the end of the line.
				while (LookAhead(0) != '\n' && !IsAtEnd(0))
				{
					Advance();
				}
				break;
			}
			else if (LookAhead(1) == '*')
			{
				// A block comment goes until the closing "*/".
				Advance();
				Advance();

				while (!(LookAhead(0) == '*' && LookAhead(1) == '/') && !IsAtEnd(0))
				{
					if (LookAhead(0) == '\n')
					{
						m_line++;
					}
					Advance();
				}

				if (IsAtEnd(0))
				{
					return std::unexpected<std::string>(MidoriError::GenerateLexerError("Unterminated block comment.", m_line));
				}

				// The closing "*/".
				Advance();
				Advance();

				break;
			}
			return Token(" "s, Token::Name::WHITESPACE, m_line);
		default:
			return Token(" "s, Token::Name::WHITESPACE, m_line);
		}
	}

	return Token(" "s, Token::Name::WHITESPACE, m_line);
}

MidoriResult::TokenResult Lexer::MatchString()
{
	std::string result;

	while (!IsAtEnd(0) && LookAhead(0) != '"')
	{
		if (LookAhead(0) == '\n')
		{
			m_line += 1;
		}

		if (LookAhead(0) == '\\')
		{
			if (!IsAtEnd(1))
			{
				switch (LookAhead(1))
				{
				case 't':
					result += '\t';
					break;
				case 'n':
					result += '\n';
					break;
				case 'b':
					result += '\b';
					break;
				case 'f':
					result += '\f';
					break;
				case '"':
					result += '"';
					break;
				case '\\':
					result += '\\';
					break;
				default:
					result += Advance();
					continue;
				}

				Advance();
				Advance();
				continue;
			}
		}

		result += Advance();
	}

	if (IsAtEnd(0))
	{
		return std::unexpected<std::string>(MidoriError::GenerateLexerError("Unterminated string.", m_line));
	}
	else
	{
		Advance();
	}

	return MakeToken(Token::Name::TEXT_LITERAL, std::move(result));
}

Token Lexer::MatchNumber()
{
	bool is_float = false;

	while (IsDigit(LookAhead(0)))
	{
		Advance();
	}

	// Look for a decimal part.
	if (LookAhead(0) == '.' && IsDigit(LookAhead(1)))
	{
		is_float = true;

		Advance();

		while (IsDigit(LookAhead(0)))
		{
			Advance();
		}
	}

	return MakeToken(is_float ? Token::Name::FLOAT_LITERAL : Token::Name::INTEGER_LITERAL);
}

Token Lexer::MatchIdentifierOrReserved()
{
	while (IsAlphaNumeric(LookAhead(0)))
	{
		Advance();
	}

	std::string identifier = m_source_code.substr(m_begin, m_current - m_begin);

	if (s_keywords.contains(identifier))
	{
		return MakeToken(s_keywords.at(identifier));
	}
	else
	{
		return MakeToken(Token::Name::IDENTIFIER_LITERAL);
	}
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
					return MakeToken(Token::Name::LEFT_PAREN);
				case ')':
					return MakeToken(Token::Name::RIGHT_PAREN);
				case '{':
					return MakeToken(Token::Name::LEFT_BRACE);
				case '}':
					return MakeToken(Token::Name::RIGHT_BRACE);
				case '[':
					return MakeToken(Token::Name::LEFT_BRACKET);
				case '@':
					return MakeToken(Token::Name::AT);
				case ']':
					if (MatchNext('['))
					{
						return MakeToken(Token::Name::RIGHT_LEFT_BRACKET);
					}
					else
					{
						return MakeToken(Token::Name::RIGHT_BRACKET);
					}
				case ',':
					return MakeToken(Token::Name::COMMA);
				case '.':
					if (IsDigit(LookAhead(0)))
					{
						return MatchNumber();
					}
					else
					{
						return MakeToken(Token::Name::DOT);
					}
				case ';':
					return MakeToken(Token::Name::SINGLE_SEMICOLON);
				case '+':
					if (MatchNext('+'))
					{
						return MakeToken(Token::Name::DOUBLE_PLUS);
					}
					else
					{
						return MakeToken(Token::Name::SINGLE_PLUS);
					}
				case '-':
					if (MatchNext('>'))
					{
						return MakeToken(Token::Name::THIN_ARROW);
					}
					else if (MatchNext('-'))
					{
						return MakeToken(Token::Name::DOUBLE_MINUS);
					}
					else
					{
						return MakeToken(Token::Name::SINGLE_MINUS);
					}
				case '?':
					return MakeToken(Token::Name::QUESTION);
				case ':':
					if (MatchNext(':'))
					{
						return MakeToken(Token::Name::DOUBLE_COLON);
					}
					else
					{
						return MakeToken(Token::Name::SINGLE_COLON);
					}
				case '%':
					return MakeToken(Token::Name::PERCENT);
				case '*':
					return MakeToken(Token::Name::STAR);
				case '/':
					return MakeToken(Token::Name::SLASH);
				case '|':
					if (MatchNext('|'))
					{
						return MakeToken(Token::Name::DOUBLE_BAR);
					}
					else
					{
						return MakeToken(Token::Name::SINGLE_BAR);
					}
				case '^':
					return MakeToken(Token::Name::CARET);
				case '&':
					if (MatchNext('&'))
					{
						return MakeToken(Token::Name::DOUBLE_AMPERSAND);
					}
					else
					{
						return MakeToken(Token::Name::SINGLE_AMPERSAND);
					}
				case '!':
					if (MatchNext('='))
					{
						return MakeToken(Token::Name::BANG_EQUAL);
					}
					else
					{
						return MakeToken(Token::Name::BANG);
					}
				case '=':
					if (MatchNext('='))
					{
						return MakeToken(Token::Name::DOUBLE_EQUAL);
					}
					else
					{
						return MakeToken(Token::Name::SINGLE_EQUAL);
					}
				case '>':
					if (MatchNext('='))
					{
						return MakeToken(Token::Name::GREATER_EQUAL);
					}
					else
					{
						return MakeToken(Token::Name::RIGHT_ANGLE);
					}
				case '<':
					if (MatchNext('='))
					{
						return MakeToken(Token::Name::LESS_EQUAL);
					}
					else if (MatchNext('~'))
					{
						return MakeToken(Token::Name::LEFT_SHIFT);
					}
					else
					{
						return MakeToken(Token::Name::LEFT_ANGLE);
					}
				case '~':
					if (MatchNext('>'))
					{
						return MakeToken(Token::Name::RIGHT_SHIFT);
					}
					else
					{
						return MakeToken(Token::Name::TILDE);
					}
				case ' ':
				case '\r':
				case '\t':
					Advance();
					return LexOneToken();
				case '"':
					return MatchString();
				case '\n':
					m_line += 1;
					return LexOneToken();
				default:
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
						return MakeToken(Token::Name::END_OF_FILE);
					}
					else
					{
						return std::unexpected<std::string>(MidoriError::GenerateLexerError("Invalid character: "s + next_char, m_line));
					}
				}
			}
		);
}

MidoriResult::LexerResult Lexer::Lex()
{
	TokenStream token_stream;
	std::string errors;

	while (!IsAtEnd(0))
	{
		std::expected<Token, std::string> result = LexOneToken();
		if (result.has_value())
		{
			token_stream.AddToken(std::move(result.value()));
		}
		else
		{
			errors.append(result.error());
			errors.push_back('\n');
		}
	}

	if (!errors.empty())
	{
		return std::unexpected<std::string>(errors);
	}

	if ((std::prev(token_stream.cend())->m_token_name != Token::Name::END_OF_FILE) && m_is_main_program)
	{
		token_stream.AddToken(MakeToken(Token::Name::END_OF_FILE));
	}

	return token_stream;
}