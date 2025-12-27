#pragma once

#include <string>
#include <unordered_map>
#include <vector>

// Forward declaration moved to include
#include "Compiler/Module/Module.h"

struct Token
{
	enum class Name
	{
		// symbol
		THIN_ARROW,
		FAT_ARROW,
		LEFT_PAREN,
		RIGHT_PAREN,
		LEFT_BRACE,
		RIGHT_BRACE,
		LEFT_BRACKET,
		RIGHT_BRACKET,
		RIGHT_LEFT_BRACKET,
		COMMA,
		SINGLE_DOT,
		DOUBLE_DOT,
		SINGLE_SEMICOLON,
		SINGLE_PLUS,
		DOUBLE_PLUS,
		SINGLE_MINUS,
		DOUBLE_MINUS,
		LEFT_SHIFT,
		RIGHT_SHIFT,
		PERCENT,
		STAR,
		SLASH,
		SINGLE_BAR,
		DOUBLE_BAR,
		BAR_BRACKET,
		CARET,
		SINGLE_AMPERSAND,
		DOUBLE_AMPERSAND,
		BANG,
		BANG_EQUAL,
		SINGLE_EQUAL,
		DOUBLE_EQUAL,
		RIGHT_ANGLE,
		GREATER_EQUAL,
		LEFT_ANGLE,
		LESS_EQUAL,
		SINGLE_COLON,
		DOUBLE_COLON,
		TILDE,
		HASH,
		PLUS_PLUS_EQUAL,
		EQUAL_PLUS_PLUS,
		PLUS_EQUAL,
		MINUS_EQUAL,
		STAR_EQUAL,
		SLASH_EQUAL,
		PERCENT_EQUAL,
		AMPERSAND_EQUAL,
		BAR_EQUAL,
		CARET_EQUAL,
		LEFT_SHIFT_EQUAL,
		RIGHT_SHIFT_EQUAL,

		// Literal
		IDENTIFIER_LITERAL,
		TEXT_LITERAL,
		FLOAT_LITERAL,
		INTEGER_LITERAL,

		// reserved
		ELSE,
		FALSE,
		FUNCTION,
		LOOP,
		FOR,
		IN,
		IF,
		RETURN,
		TRUE,
		DEF,
		DEFUN,
		BREAK,
		CONTINUE,
		IMPORT,
		STRUCT,
		UNION,
		NEW,
		AS,
		FOREIGN,
		CASE,
		DEFAULT,
		MATCH,
		THEN,
		WITH,
		MODULE,
		EXPORT,
		PUBLIC,
		PRIVATE,
		USE,
		CLASS,
		INSTANCE,
		WHERE,
		TYPE,

		// types
		FLOAT,
		INTEGER,
		BYTE,
		WORD,
		TEXT,
		BOOL,
		UNIT,
		ARRAY,
		NEVER,

		WHITESPACE,
		END_OF_FILE,
	};

	std::string m_lexeme;
	Name m_token_name;
	int m_line;

	Token(std::string&& lexeme, Name token_name, int line) noexcept;
};

class TokenStream
{
private:
	std::vector<Token> m_tokens;

public:
	using iterator = std::vector<Token>::iterator;
	using const_iterator = std::vector<Token>::const_iterator;

	iterator begin();

	iterator end();

	const_iterator cbegin() const;

	const_iterator cend() const;

	TokenStream& AddToken(Token&& token);

	Token& operator[](int index) const;

	int Size() const;

	void Insert(iterator iter, TokenStream&& tokens);

	void Erase(iterator iter);

	void PopBack() noexcept;
};

struct BuildGraph
{
	struct BuildNode
	{
		TokenStream m_tokens;
		std::string m_file_name;
		std::vector<std::string> m_dependencies;
		std::vector<UseImport> m_use_imports;  // Symbols brought into scope via 'use' statements
		int m_in_degree = 0;
		bool m_processed = false;
	};

	std::unordered_map<std::string, BuildNode> m_nodes;
	std::unordered_map<std::string, ModuleDeclaration> m_module_declarations;
	std::unordered_map<std::string, std::vector<UseImport>> m_use_imports;  // Maps file_name -> use imports

	std::vector<std::string> GetStartingPoints() const;

	void MarkProcessed(const std::string& file_name);

	bool IsComplete() const;

	std::vector<std::vector<std::string>> GetCompilationStreams() const;
};