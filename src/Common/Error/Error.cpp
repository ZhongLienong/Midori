#include "Error.h"
#include "Common/Printer/Printer.h"
#include <algorithm>
#include <sstream>

namespace
{
	std::string_view StageLabel(CompilerStage stage)
	{
		switch (stage)
		{
		case CompilerStage::Lexer:
			return "Lexer Error";
		case CompilerStage::Parser:
			return "Parser Error";
		case CompilerStage::TypeChecker:
			return "Type Checker Error";
		case CompilerStage::CodeGenerator:
			return "Code Generator Error";
		case CompilerStage::Module:
			return "Module Error";
		case CompilerStage::Optimizer:
			return "Optimizer Error";
		case CompilerStage::BytecodeLinker:
			return "Bytecode Linker Error";
		case CompilerStage::Compiler:
			return "Compiler Error";
		case CompilerStage::Runtime:
			return "Runtime Error";
		case CompilerStage::Unknown:
		default:
			return "Error";
		}
	}

	std::string_view StageWarningLabel(CompilerStage stage)
	{
		switch (stage)
		{
		case CompilerStage::Lexer:
			return "Lexer Warning";
		case CompilerStage::Parser:
			return "Parser Warning";
		case CompilerStage::TypeChecker:
			return "Type Checker Warning";
		case CompilerStage::CodeGenerator:
			return "Code Generator Warning";
		case CompilerStage::Module:
			return "Module Warning";
		case CompilerStage::Optimizer:
			return "Optimizer Warning";
		case CompilerStage::BytecodeLinker:
			return "Bytecode Linker Warning";
		case CompilerStage::Compiler:
			return "Compiler Warning";
		case CompilerStage::Runtime:
			return "Runtime Warning";
		case CompilerStage::Unknown:
		default:
			return "Warning";
		}
	}

	std::string RenderCompilerError(const CompilerError& error)
	{
		if (!error.m_location.has_value())
		{
			return error.m_message;
		}

		const CompilerErrorLocation& location = error.m_location.value();
		std::ostringstream oss;

		// Error header with colors
		oss << Printer::Detail::GetStyleCode(Printer::Style::BOLD);
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_RED);
		oss << StageLabel(error.m_stage);
		oss << "\033[0m";
		oss << " at ";
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_CYAN);
		oss << location.m_file_name << ":" << location.m_line;
		oss << "\033[0m\n";

		if (location.m_source_line.has_value() && location.m_line > 0)
		{
			std::string_view source_line = location.m_source_line.value();

			// Line number gutter
			std::string line_num_str = std::to_string(location.m_line);
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

			if (location.m_column.has_value())
			{
				oss << " ";

				// Add spaces before the caret
				for (int i = 0; i < location.m_column.value(); i += 1)
				{
					oss << " ";
				}

				// Add carets
				size_t length = location.m_caret_length.value_or(1u);
				for (size_t i = 0u; i < length; i += 1u)
				{
					oss << "^";
				}
				oss << " " << error.m_message;
			}
			else
			{
				oss << " " << error.m_message;
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
			oss << "  " << error.m_message << "\n";
		}

		if (error.m_suggestion.has_value())
		{
			oss << Printer::Detail::GetColorCode(Printer::Color::YELLOW);
			oss << "  | ";
			oss << error.m_suggestion.value();
			oss << "\033[0m\n";
		}

		return oss.str();
	}

	std::string RenderCompilerWarning(const CompilerWarning& warning)
	{
		if (!warning.m_location.has_value())
		{
			return warning.m_message;
		}

		const CompilerErrorLocation& location = warning.m_location.value();
		std::ostringstream oss;

		oss << Printer::Detail::GetStyleCode(Printer::Style::BOLD);
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_YELLOW);
		oss << StageWarningLabel(warning.m_stage);
		oss << "\033[0m";
		oss << " at ";
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_CYAN);
		oss << location.m_file_name << ":" << location.m_line;
		oss << "\033[0m\n";

		if (location.m_source_line.has_value() && location.m_line > 0)
		{
			std::string_view source_line = location.m_source_line.value();

			std::string line_num_str = std::to_string(location.m_line);
			int gutter_width = static_cast<int>(line_num_str.length()) + 1;

			oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
			for (int i = 0; i < gutter_width; i += 1)
			{
				oss << " ";
			}
			oss << "|\n";

			oss << line_num_str << " ";
			oss << "|";
			oss << "\033[0m";
			oss << " " << source_line << "\n";

			oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
			for (int i = 0; i < gutter_width; i += 1)
			{
				oss << " ";
			}
			oss << "|";
			oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_YELLOW);

			if (location.m_column.has_value())
			{
				oss << " ";
				for (int i = 0; i < location.m_column.value(); i += 1)
				{
					oss << " ";
				}

				size_t length = location.m_caret_length.value_or(1u);
				for (size_t i = 0u; i < length; i += 1u)
				{
					oss << "^";
				}
				oss << " " << warning.m_message;
			}
			else
			{
				oss << " " << warning.m_message;
			}

			oss << "\033[0m\n";

			oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
			for (int i = 0; i < gutter_width; i += 1)
			{
				oss << " ";
			}
			oss << "|\033[0m\n";
		}
		else
		{
			oss << "  " << warning.m_message << "\n";
		}

		if (warning.m_suggestion.has_value())
		{
			oss << Printer::Detail::GetColorCode(Printer::Color::YELLOW);
			oss << "  | ";
			oss << warning.m_suggestion.value();
			oss << "\033[0m\n";
		}

		return oss.str();
	}
}

CompilerError::CompilerError(std::string message)
	: m_stage(CompilerStage::Compiler),
	m_message(std::move(message))
{
	m_rendered = m_message;
}

CompilerError::CompilerError(std::string_view message)
	: CompilerError(std::string(message))
{
}

CompilerError::CompilerError(const char* message)
	: CompilerError(std::string(message))
{
}

CompilerError::CompilerError(CompilerStage stage, std::string message)
	: m_stage(stage),
	m_message(std::move(message))
{
	m_rendered = RenderCompilerError(*this);
}

CompilerError CompilerError::NoMatch()
{
	CompilerError error;
	error.m_stage = CompilerStage::Parser;
	error.m_code = CompilerErrorCode::NoMatch;
	return error;
}

CompilerError CompilerError::Simple(CompilerStage stage, std::string_view message)
{
	CompilerError error(stage, std::string(message));
	return error;
}

CompilerError CompilerError::WithContext(CompilerStage stage, std::string_view message, int line, std::string_view file_name, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, std::optional<std::string_view> source_line)
{
	CompilerError error;
	error.m_stage = stage;
	error.m_message = std::string(message);

	CompilerErrorLocation location;
	location.m_file_name = std::string(file_name);
	location.m_line = line;
	location.m_column = column;
	location.m_caret_length = caret_length;
	if (source_line.has_value())
	{
		location.m_source_line = std::string(*source_line);
	}
	error.m_location = std::move(location);

	if (suggestion.has_value())
	{
		error.m_suggestion = std::string(*suggestion);
	}

	error.m_rendered = RenderCompilerError(error);
	return error;
}

CompilerError CompilerError::WithToken(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Find column position by searching for the token lexeme in the source line
	std::optional<int> column = std::nullopt;
	std::optional<size_t> caret_length = std::nullopt;
	std::optional<std::string_view> source_line = std::nullopt;

	if (token.m_line > 0 && static_cast<size_t>(token.m_line) <= source_lines.size())
	{
		source_line = source_lines[static_cast<size_t>(token.m_line - 1)];
		size_t col_pos = source_line->find(token.m_lexeme);
		if (col_pos != std::string::npos)
		{
			column = static_cast<int>(col_pos);
			caret_length = std::max(token.m_lexeme.length(), size_t(1u));
		}
	}

	return WithContext(stage, message, token.m_line, file_name, column, caret_length, suggestion, source_line);
}

bool CompilerError::IsNoMatch() const
{
	return m_code == CompilerErrorCode::NoMatch;
}

std::string_view CompilerError::Rendered() const
{
	return m_rendered.empty() ? std::string_view(m_message) : std::string_view(m_rendered);
}

CompilerWarning::CompilerWarning(std::string message)
	: m_stage(CompilerStage::Compiler),
	m_message(std::move(message))
{
	m_rendered = m_message;
}

CompilerWarning::CompilerWarning(std::string_view message)
	: CompilerWarning(std::string(message))
{
}

CompilerWarning::CompilerWarning(const char* message)
	: CompilerWarning(std::string(message))
{
}

CompilerWarning::CompilerWarning(CompilerStage stage, std::string message)
	: m_stage(stage),
	m_message(std::move(message))
{
	m_rendered = RenderCompilerWarning(*this);
}

CompilerWarning CompilerWarning::Simple(CompilerStage stage, std::string_view message)
{
	CompilerWarning warning(stage, std::string(message));
	return warning;
}

CompilerWarning CompilerWarning::WithContext(CompilerStage stage, std::string_view message, int line, std::string_view file_name, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, std::optional<std::string_view> source_line)
{
	CompilerWarning warning;
	warning.m_stage = stage;
	warning.m_message = std::string(message);

	CompilerErrorLocation location;
	location.m_file_name = std::string(file_name);
	location.m_line = line;
	location.m_column = column;
	location.m_caret_length = caret_length;
	if (source_line.has_value())
	{
		location.m_source_line = std::string(*source_line);
	}
	warning.m_location = std::move(location);

	if (suggestion.has_value())
	{
		warning.m_suggestion = std::string(*suggestion);
	}

	warning.m_rendered = RenderCompilerWarning(warning);
	return warning;
}

CompilerWarning CompilerWarning::WithToken(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	std::optional<int> column = std::nullopt;
	std::optional<size_t> caret_length = std::nullopt;
	std::optional<std::string_view> source_line = std::nullopt;

	if (token.m_line > 0 && static_cast<size_t>(token.m_line) <= source_lines.size())
	{
		source_line = source_lines[static_cast<size_t>(token.m_line - 1)];
		size_t col_pos = source_line->find(token.m_lexeme);
		if (col_pos != std::string::npos)
		{
			column = static_cast<int>(col_pos);
			caret_length = std::max(token.m_lexeme.length(), size_t(1u));
		}
	}

	return WithContext(stage, message, token.m_line, file_name, column, caret_length, suggestion, source_line);
}

std::string_view CompilerWarning::Rendered() const
{
	return m_rendered.empty() ? std::string_view(m_message) : std::string_view(m_rendered);
}

CompilerError MidoriError::GenerateRichError(CompilerStage stage, std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion)
{
	std::optional<std::string_view> source_line = std::nullopt;
	if (line > 0 && static_cast<size_t>(line) <= source_lines.size())
	{
		source_line = source_lines[line - 1];
	}

	return CompilerError::WithContext(stage, message, line, file_name, column, caret_length, suggestion, source_line);
}

CompilerError MidoriError::GenerateRichError(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return CompilerError::WithToken(stage, message, token, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateCodeGeneratorErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError(CompilerStage::CodeGenerator, message, token, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateCodeGeneratorErrorWithContext(std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Use unified implementation without column info (no caret)
	return GenerateRichError(CompilerStage::CodeGenerator, message, line, file_name, source_lines, std::nullopt, std::nullopt, suggestion);
}

CompilerError MidoriError::GenerateLexerErrorWithContext(std::string_view message, int line, int column, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Use unified implementation with column and single caret
	return GenerateRichError(CompilerStage::Lexer, message, line, file_name, source_lines, column, 1, suggestion);
}

CompilerError MidoriError::GenerateModuleErrorWithContext(std::string_view message, int line, std::string_view file_name, std::optional<std::string_view> suggestion)
{
	return CompilerError::WithContext(CompilerStage::Module, message, line, file_name, std::nullopt, std::nullopt, suggestion);
}

CompilerError MidoriError::GenerateParserErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError(CompilerStage::Parser, message, token, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError(CompilerStage::TypeChecker, message, token, file_name, source_lines, suggestion);
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
