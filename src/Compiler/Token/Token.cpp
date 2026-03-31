#include "Token.h"
#include "Common/Printer/Printer.h"

#include <queue>
#include <map>

Token::Token(std::string lexeme, Name token_name, int line, std::string_view file_name) noexcept
	: m_file_name(file_name),
	m_lexeme(std::move(lexeme)),
	m_token_name(token_name),
	m_line(line)
{
}

Token::Token(std::string lexeme, Name token_name, int line, std::string_view file_name, int column, size_t source_length) noexcept
	: m_file_name(file_name),
	m_lexeme(std::move(lexeme)),
	m_token_name(token_name),
	m_line(line),
	m_column(column),
	m_source_length(source_length)
{
}

Token::Token(std::string lexeme, Name token_name, const Token& anchor) noexcept
	: m_file_name(anchor.m_file_name),
	m_lexeme(std::move(lexeme)),
	m_token_name(token_name),
	m_line(anchor.m_line),
	m_column(anchor.m_column),
	m_source_length(anchor.m_source_length)
{
}

TokenStream::iterator TokenStream::begin()
{ 
	return m_tokens.begin(); 
}

TokenStream::iterator TokenStream::end()
{ 
	return m_tokens.end();
}

TokenStream::const_iterator TokenStream::cbegin() const
{ 
	return m_tokens.cbegin(); 
}

TokenStream::const_iterator TokenStream::cend() const
{ 
	return m_tokens.cend(); 
}

TokenStream& TokenStream::AddToken(Token&& token)
{ 
	m_tokens.emplace_back(std::move(token)); 
	return *this;
}

Token& TokenStream::operator[](int index) const 
{ 
	return const_cast<Token&>(m_tokens[static_cast<size_t>(index)]);
}

int TokenStream::Size() const 
{ 
	return static_cast<int>(m_tokens.size()); 
}

void TokenStream::Insert(TokenStream::iterator iter, TokenStream&& tokens)
{
	m_tokens.insert(iter, tokens.begin(), tokens.end());
}

void TokenStream::Erase(TokenStream::iterator iter)
{
	m_tokens.erase(iter);
}

void TokenStream::PopBack() noexcept
{
    m_tokens.pop_back();
}

