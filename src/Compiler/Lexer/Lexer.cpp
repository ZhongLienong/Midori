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
	return static_cast<size_t>(m_cursor.m_current + offset) >= m_source.m_code.size();
}

bool Lexer::IsDigit(char c)
{
	return c >= '0' && c <= '9';
}

bool Lexer::IsAlpha(char c)
{
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

bool Lexer::IsAlphaNumeric(char c)
{
	return IsDigit(c) || IsAlpha(c);
}

char Lexer::Advance()
{
	return m_source.m_code[m_cursor.m_current++];
}

char Lexer::LookAhead(int offset) const
{
	return IsAtEnd(offset) ? '\0' : m_source.m_code[static_cast<size_t>(m_cursor.m_current + offset)];
}

bool Lexer::MatchNext(char expected)
{
	if (!IsAtEnd(0) && m_source.m_code[m_cursor.m_current] == expected)
	{
		m_cursor.m_current += 1;
		return true;
	}
	return false;
}

Lexer& Lexer::BeginToken()
{
	m_cursor.m_begin = m_cursor.m_current;
	return *this;
}

Lexer& Lexer::AdvanceLine()
{
	m_cursor.m_line += 1;
	Advance();
	m_cursor.m_line_start = m_cursor.m_current;
	return *this;
}

int Lexer::CurrentColumn() const
{
	return m_cursor.m_current - m_cursor.m_line_start;
}

int Lexer::BeginColumn() const
{
	return m_cursor.m_begin - m_cursor.m_line_start;
}

Token Lexer::MakeToken(Token::Name type) const
{
	return Token(m_source.m_code.substr(m_cursor.m_begin, m_cursor.m_current - m_cursor.m_begin), type, m_cursor.m_line, m_source.m_file_name);
}

Token Lexer::MakeToken(Token::Name type, std::string&& lexeme) const
{
	return Token(std::move(lexeme), type, m_cursor.m_line, m_source.m_file_name);
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
	return ConsumeWhile(IsDigit);
}

int Lexer::ConsumeAlphaNumeric()
{
	return ConsumeWhile(IsAlphaNumeric);
}

MidoriResult::VoidResult Lexer::SkipLineComment()
{
	ConsumeWhile([](char c) { return c != '\n'; });
	return {};
}

MidoriResult::VoidResult Lexer::SkipBlockComment()
{
	while (true)
	{
		if (LookAhead(0) == '*' && LookAhead(1) == '/')
		{
			Advance(); // '*'
			Advance(); // '/'
			return {};
		}

		if (IsAtEnd(0))
		{
			const int column = CurrentColumn();
			return std::unexpected(MidoriError::GenerateLexerErrorWithContext("Unterminated block comment", m_cursor.m_line, column, m_source.m_file_name, m_source.m_lines));
		}

		if (LookAhead(0) == '\n')
		{
			AdvanceLine();
			continue;
		}

		Advance();
	}
}

MidoriResult::VoidResult Lexer::SkipWhitespaceAndComments()
{
	while (true)
	{
		if (IsAtEnd(0))
		{
			return {};
		}

		const char c = LookAhead(0);

		switch (c)
		{
		case ' ':
		case '\r':
		case '\t':
			Advance();
			continue;
		case '\n':
			AdvanceLine();
			continue;
		case '/':
		{
			if (LookAhead(1) == '/')
			{
				Advance(); // '/'
				Advance(); // '/'
				return SkipLineComment()
					.and_then([this]() -> MidoriResult::VoidResult { return SkipWhitespaceAndComments(); });
			}
			else if (LookAhead(1) == '*')
			{
				Advance(); // '/'
				Advance(); // '*'
				return SkipBlockComment()
					.and_then([this]() -> MidoriResult::VoidResult { return SkipWhitespaceAndComments(); });
			}
			return {};
		}
		default:
			return {};
		}
	}
}

MidoriResult::TokenResult Lexer::MatchStringRecursive(std::string&& acc)
{
	while (true)
	{
		if (IsAtEnd(0))
		{
			const int column = BeginColumn();
			return std::unexpected(MidoriError::GenerateLexerErrorWithContext("Unterminated string", m_cursor.m_line, column, m_source.m_file_name, m_source.m_lines));
		}

		if (LookAhead(0) == '"')
		{
			Advance();
			return MakeToken(Token::Name::TEXT_LITERAL, std::move(acc));
		}

		if (LookAhead(0) == '\n')
		{
			m_cursor.m_line += 1;
		}

		if (LookAhead(0) == '\\' && !IsAtEnd(1))
		{
			const char escape_char = LookAhead(1);
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
	if (IsHexPrefix())
	{
		return MatchPrefixedInteger(IsHexDigit);
	}

	if (IsBinaryPrefix())
	{
		return MatchPrefixedInteger(IsBinaryDigit);
	}

	return MatchDecimalNumber();
}

MidoriResult::TokenResult Lexer::MatchDecimalNumber()
{
	ConsumeDigits();

	const bool is_float = LookAhead(0) == '.' && IsDigit(LookAhead(1));

	if (!is_float)
	{
		return MakeTokenResult(Token::Name::INTEGER_LITERAL);
	}

	Advance();
	ConsumeDigits();
	return MakeTokenResult(Token::Name::FLOAT_LITERAL);
}

MidoriResult::TokenResult Lexer::MatchPrefixedInteger(bool (*predicate)(char))
{
	Advance();
	ConsumeWhile(predicate);
	return MakeTokenResult(Token::Name::INTEGER_LITERAL);
}

bool Lexer::IsHexPrefix() const
{
	return LookAhead(-1) == '0' && (LookAhead(0) == 'x' || LookAhead(0) == 'X');
}

bool Lexer::IsBinaryPrefix() const
{
	return LookAhead(-1) == '0' && (LookAhead(0) == 'b' || LookAhead(0) == 'B');
}

bool Lexer::IsHexDigit(char c)
{
	return IsDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

bool Lexer::IsBinaryDigit(char c)
{
	return c == '0' || c == '1';
}

MidoriResult::TokenResult Lexer::MatchIdentifierOrReserved()
{
	ConsumeAlphaNumeric();

	std::string identifier = m_source.m_code.substr(m_cursor.m_begin, m_cursor.m_current - m_cursor.m_begin);

	return s_keywords.contains(identifier)
		? MakeTokenResult(s_keywords.at(identifier))
		: MakeTokenResult(Token::Name::IDENTIFIER_LITERAL);
}

MidoriResult::TokenResult Lexer::LexTokenAfterWhitespace()
{
	return IsAtEnd(0)
		? BeginToken().MakeTokenResult(Token::Name::END_OF_FILE)
		: BeginToken().LexTokenFrom(Advance());
}

MidoriResult::TokenResult Lexer::LexTokenFrom(char next_char)
{
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
		return MatchDot();
	case ';':
		return MakeTokenResult(Token::Name::SINGLE_SEMICOLON);
	case '+':
		return MatchPlus();
	case '-':
		return MatchMinus();
	case ':':
		return MatchColon();
	case '%':
		return MatchPercent();
	case '*':
		return MatchStar();
	case '/':
		return MatchSlash();
	case '|':
		return MatchPipe();
	case '^':
		return MatchCaret();
	case '&':
		return MatchAmpersand();
	case '!':
		return MatchBang();
	case '=':
		return MatchEqual();
	case '>':
		return MatchGreater();
	case '<':
		return MatchLess();
	case '~':
		return MatchTilde();
	case '#':
		return MakeTokenResult(Token::Name::HASH);
	case '"':
		return MatchString();
	default:
		return MatchLiteralOrIdentifier(next_char);
	}
}

MidoriResult::TokenResult Lexer::MatchDot()
{
	if (IsDigit(LookAhead(0)))
	{
		return MatchNumber();
	}

	return MatchNext('.')
		? MakeTokenResult(Token::Name::DOUBLE_DOT)
		: MakeTokenResult(Token::Name::SINGLE_DOT);
}

MidoriResult::TokenResult Lexer::MatchPlus()
{
	if (MatchNext('+'))
	{
		return MatchNext('=')
			? MakeTokenResult(Token::Name::PLUS_PLUS_EQUAL)
			: MakeTokenResult(Token::Name::DOUBLE_PLUS);
	}

	return MatchNext('=')
		? MakeTokenResult(Token::Name::PLUS_EQUAL)
		: MakeTokenResult(Token::Name::SINGLE_PLUS);
}

MidoriResult::TokenResult Lexer::MatchMinus()
{
	if (MatchNext('>'))
	{
		return MakeTokenResult(Token::Name::THIN_ARROW);
	}

	if (MatchNext('-'))
	{
		return MakeTokenResult(Token::Name::DOUBLE_MINUS);
	}

	return MatchNext('=')
		? MakeTokenResult(Token::Name::MINUS_EQUAL)
		: MakeTokenResult(Token::Name::SINGLE_MINUS);
}

MidoriResult::TokenResult Lexer::MatchColon()
{
	return MatchNext(':')
		? MakeTokenResult(Token::Name::DOUBLE_COLON)
		: MakeTokenResult(Token::Name::SINGLE_COLON);
}

MidoriResult::TokenResult Lexer::MatchPercent()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::PERCENT_EQUAL)
		: MakeTokenResult(Token::Name::PERCENT);
}

MidoriResult::TokenResult Lexer::MatchStar()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::STAR_EQUAL)
		: MakeTokenResult(Token::Name::STAR);
}

MidoriResult::TokenResult Lexer::MatchSlash()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::SLASH_EQUAL)
		: MakeTokenResult(Token::Name::SLASH);
}

MidoriResult::TokenResult Lexer::MatchPipe()
{
	if (MatchNext('|'))
	{
		return MakeTokenResult(Token::Name::DOUBLE_BAR);
	}

	if (MatchNext('>'))
	{
		return MakeTokenResult(Token::Name::BAR_BRACKET);
	}

	return MatchNext('=')
		? MakeTokenResult(Token::Name::BAR_EQUAL)
		: MakeTokenResult(Token::Name::SINGLE_BAR);
}

MidoriResult::TokenResult Lexer::MatchCaret()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::CARET_EQUAL)
		: MakeTokenResult(Token::Name::CARET);
}

MidoriResult::TokenResult Lexer::MatchAmpersand()
{
	if (MatchNext('&'))
	{
		return MakeTokenResult(Token::Name::DOUBLE_AMPERSAND);
	}

	return MatchNext('=')
		? MakeTokenResult(Token::Name::AMPERSAND_EQUAL)
		: MakeTokenResult(Token::Name::SINGLE_AMPERSAND);
}

MidoriResult::TokenResult Lexer::MatchBang()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::BANG_EQUAL)
		: MakeTokenResult(Token::Name::BANG);
}

MidoriResult::TokenResult Lexer::MatchEqual()
{
	if (MatchNext('='))
	{
		return MakeTokenResult(Token::Name::DOUBLE_EQUAL);
	}

	if (MatchNext('>'))
	{
		return MakeTokenResult(Token::Name::FAT_ARROW);
	}

	if (MatchNext('+'))
	{
		return MatchEqualPlusPlus();
	}

	return MakeTokenResult(Token::Name::SINGLE_EQUAL);
}

MidoriResult::TokenResult Lexer::MatchEqualPlusPlus()
{
	return MatchNext('+')
		? MakeTokenResult(Token::Name::EQUAL_PLUS_PLUS)
		: std::unexpected(MidoriError::GenerateLexerErrorWithContext("Unexpected character '=+' (did you mean '=++'?)", m_cursor.m_line, BeginColumn(), m_source.m_file_name, m_source.m_lines));
}

MidoriResult::TokenResult Lexer::MatchGreater()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::GREATER_EQUAL)
		: MakeTokenResult(Token::Name::RIGHT_ANGLE);
}

MidoriResult::TokenResult Lexer::MatchLess()
{
	if (MatchNext('='))
	{
		return MakeTokenResult(Token::Name::LESS_EQUAL);
	}

	return MatchNext('~')
		? MatchLeftShift()
		: MakeTokenResult(Token::Name::LEFT_ANGLE);
}

MidoriResult::TokenResult Lexer::MatchLeftShift()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::LEFT_SHIFT_EQUAL)
		: MakeTokenResult(Token::Name::LEFT_SHIFT);
}

MidoriResult::TokenResult Lexer::MatchTilde()
{
	return MatchNext('>')
		? MatchRightShift()
		: MakeTokenResult(Token::Name::TILDE);
}

MidoriResult::TokenResult Lexer::MatchRightShift()
{
	return MatchNext('=')
		? MakeTokenResult(Token::Name::RIGHT_SHIFT_EQUAL)
		: MakeTokenResult(Token::Name::RIGHT_SHIFT);
}

MidoriResult::TokenResult Lexer::MatchLiteralOrIdentifier(char next_char)
{
	if (IsDigit(next_char))
	{
		return MatchNumber();
	}

	if (IsAlpha(next_char))
	{
		return MatchIdentifierOrReserved();
	}

	if (next_char == '\0')
	{
		return MakeTokenResult(Token::Name::END_OF_FILE);
	}

	return MakeInvalidCharacterError(next_char);
}

MidoriResult::TokenResult Lexer::MakeInvalidCharacterError(char next_char) const
{
	return std::unexpected(MidoriError::GenerateLexerErrorWithContext("Invalid character: "s + next_char, m_cursor.m_line, CurrentColumn(), m_source.m_file_name, m_source.m_lines));
}

MidoriResult::Result<Lexer::LexState> Lexer::RecordTokenOrError(LexState state)
{
	return LexOneToken()
		.transform
		(
			[state = std::move(state)](Token&& token) mutable -> LexState
			{
				state.m_tokens.AddToken(std::move(token));
				return std::move(state);
			}
		)
		.or_else
		(
			[state = std::move(state)](CompilerError&& error) mutable -> MidoriResult::Result<LexState>
			{
				state.m_errors.append(error.Rendered()).append("\n");
				return std::move(state);
			}
		);
}

MidoriResult::TokenResult Lexer::LexOneToken()
{
	return SkipWhitespaceAndComments()
		.and_then([this]() -> MidoriResult::TokenResult { return LexTokenAfterWhitespace(); });
}

std::vector<std::string> Lexer::SplitIntoLines(const std::string& source)
{
	std::istringstream iss(source);
	std::vector<std::string> lines;
	std::string line;

	while (std::getline(iss, line))
	{
		lines.emplace_back(std::move(line));
	}

	return lines;
}

Lexer::Source Lexer::BuildSource(std::string&& source_code, std::string_view file_name)
{
	std::vector<std::string> lines = SplitIntoLines(source_code);
	return Source{ std::move(source_code), std::string(file_name), std::move(lines) };
}

Lexer::Lexer(std::string&& source_code, std::string_view file_name) noexcept
	: m_source(BuildSource(std::move(source_code), file_name))
{
}

MidoriResult::LexerResult Lexer::LexRecursive(LexState state)
{
	while (!IsAtEnd(0))
	{
		MidoriResult::VoidResult step = RecordTokenOrError(std::move(state))
			.and_then
			(
				[&state](LexState&& next_state) -> MidoriResult::VoidResult
				{
					state = std::move(next_state);
					return {};
				}
			);
		if (!step)
		{
			return std::unexpected(step.error());
		}
	}

	if (!state.m_errors.empty())
	{
		return std::unexpected(std::move(state.m_errors));
	}

	if (state.m_tokens.Size() == 0 || (std::prev(state.m_tokens.cend())->m_token_name != Token::Name::END_OF_FILE))
	{
		state.m_tokens.AddToken(MakeToken(Token::Name::END_OF_FILE));
	}

	return std::move(state.m_tokens);
}

MidoriResult::LexerResult Lexer::Lex() &
{
	return LexRecursive(LexState{ TokenStream{}, std::string{} });
}

MidoriResult::LexerResult Lexer::Lex() &&
{
	return LexRecursive(LexState{ TokenStream{}, std::string{} });
}

