#include "Error.h"
#include "Common/Printer/Printer.h"
#include <sstream>
#include <algorithm>

std::string MidoriError::GenerateRichError(std::string_view error_type, std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion)
{
	std::ostringstream oss;

	// Error header with colors
	oss << Printer::Detail::GetStyleCode(Printer::Style::BOLD);
	oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_RED);
	oss << error_type;
	oss << "\033[0m";
	oss << " at ";
	oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_CYAN);
	oss << file_name << ":" << line;
	oss << "\033[0m\n";

	// Get the source line (line numbers are 1-indexed)
	if (line > 0 && static_cast<size_t>(line) <= source_lines.size())
	{
		std::string_view source_line = source_lines[line - 1];

		// Line number gutter
		std::string line_num_str = std::to_string(line);
		int gutter_width = static_cast<int>(line_num_str.length()) + 1;

		// Empty line with separator
		oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
		for (int i = 0; i < gutter_width; i += 1)
		{
			oss << " ";
		}
		oss << "|\n";

		// Source line
		oss << line_num_str << " ";
		oss << "|";
		oss << "\033[0m";
		oss << " " << source_line << "\n";

		// Error pointer line (if column info provided)
		oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
		for (int i = 0; i < gutter_width; i += 1)
		{
			oss << " ";
		}
		oss << "|";
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_RED);

		if (column.has_value())
		{
			oss << " ";

			// Add spaces before the caret
			for (int i = 0; i < column.value(); i += 1)
			{
				oss << " ";
			}

			// Add carets
			size_t length = caret_length.value_or(1u);
			for (size_t i = 0u; i < length; i += 1)
			{
				oss << "^";
			}
			oss << " " << message;
		}
		else
		{
			oss << " " << message;
		}

		oss << "\033[0m\n";

		// Empty line separator
		oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
		for (int i = 0; i < gutter_width; i += 1)
		{
			oss << " ";
		}
		oss << "|\033[0m\n";
	}
	else
	{
		oss << "  " << message << "\n";
	}

	if (suggestion.has_value())
	{
		oss << Printer::Detail::GetColorCode(Printer::Color::YELLOW);
		oss << "  | ";
		oss << suggestion.value();
		oss << "\033[0m\n";
	}

	return oss.str();
}

std::string MidoriError::GenerateRichError(std::string_view error_type, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Find column position by searching for the token lexeme in the source line
	std::optional<int> column = std::nullopt;
	std::optional<size_t> caret_length = std::nullopt;

	if (token.m_line > 0 && static_cast<size_t>(token.m_line) <= source_lines.size())
	{
		std::string_view source_line = source_lines[static_cast<size_t>(token.m_line - 1)];
		size_t col_pos = source_line.find(token.m_lexeme);
		if (col_pos != std::string::npos)
		{
			column = static_cast<int>(col_pos);
			caret_length = std::max(token.m_lexeme.length(), size_t(1u));
		}
	}

	return GenerateRichError(error_type, message, token.m_line, file_name, source_lines, column, caret_length, suggestion);
}

std::string MidoriError::GenerateCodeGeneratorErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError("Code Generator Error", message, token, file_name, source_lines, suggestion);
}

std::string MidoriError::GenerateCodeGeneratorErrorWithContext(std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Use unified implementation without column info (no caret)
	return GenerateRichError("Code Generator Error", message, line, file_name, source_lines, std::nullopt, std::nullopt, suggestion);
}

std::string MidoriError::GenerateLexerErrorWithContext(std::string_view message, int line, int column, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Use unified implementation with column and single caret
	return GenerateRichError("Lexer Error", message, line, file_name, source_lines, column, 1, suggestion);
}

std::string MidoriError::GenerateImportManagerErrorWithContext(std::string_view message, int line, std::string_view file_name, std::optional<std::string_view> suggestion)
{
	std::ostringstream oss;

	// Error header with colors
	oss << Printer::Detail::GetStyleCode(Printer::Style::BOLD);
	oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_RED);
	oss << "Import Manager Error";
	oss << "\033[0m";
	oss << " at ";
	oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_CYAN);
	oss << file_name << ":" << line;
	oss << "\033[0m\n";

	oss << "  " << message << "\n";

	if (suggestion.has_value())
	{
		oss << Printer::Detail::GetColorCode(Printer::Color::YELLOW);
		oss << "  | ";
		oss << suggestion.value();
		oss << "\033[0m\n";
	}

	return oss.str();
}

std::string MidoriError::GenerateParserErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError("Parser Error", message, token, file_name, source_lines, suggestion);
}

std::string MidoriError::GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError("Type Checker Error", message, token, file_name, source_lines, suggestion);
}

std::string MidoriError::GenerateRuntimeError(std::string_view message, int line)
{
	std::ostringstream oss;

	// Error type header in bold bright red
	oss << Printer::Detail::GetStyleCode(Printer::Style::BOLD);
	oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_RED);
	oss << "Runtime Error";
	oss << "\033[0m"; // Reset

	// Line number in cyan
	if (line > 0)
	{
		oss << " at ";
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_CYAN);
		oss << "line " << line;
		oss << "\033[0m"; // Reset
	}

	oss << "\n";

	// Error message
	oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_WHITE);
	oss << message;
	oss << "\033[0m"; // Reset

	return oss.str();
}
