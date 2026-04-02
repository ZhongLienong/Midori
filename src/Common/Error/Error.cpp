#include "Error.h"
#include "Common/Printer/Printer.h"
#include <algorithm>
#include <filesystem>
#include <sstream>

std::string_view CompilerStageName(CompilerStage stage)
{
	switch (stage)
	{
	case CompilerStage::Lexer:
		return "Lexer";
	case CompilerStage::Parser:
		return "Parser";
	case CompilerStage::TypeChecker:
		return "TypeChecker";
	case CompilerStage::StaticAnalyzer:
		return "StaticAnalyzer";
	case CompilerStage::CodeGenerator:
		return "CodeGenerator";
	case CompilerStage::Module:
		return "Module";
	case CompilerStage::Optimizer:
		return "Optimizer";
	case CompilerStage::BytecodeLinker:
		return "BytecodeLinker";
	case CompilerStage::Compiler:
		return "Compiler";
	case CompilerStage::Runtime:
		return "Runtime";
	case CompilerStage::Unknown:
	default:
		return "Unknown";
	}
}

std::string_view CompilerWarningCodeName(CompilerWarningCode code)
{
	switch (code)
	{
	case CompilerWarningCode::NameShadowing:
		return "NameShadowing";
	case CompilerWarningCode::UnusedLocal:
		return "UnusedLocal";
	case CompilerWarningCode::UnreachableCode:
		return "UnreachableCode";
	case CompilerWarningCode::CaptureEscape:
		return "CaptureEscape";
	case CompilerWarningCode::None:
	default:
		return "None";
	}
}

namespace
{
	enum class DiagnosticSeverity
	{
		Error,
		Warning
	};

	struct TokenLocationContext
	{
		std::optional<int> m_column = std::nullopt;
		std::optional<size_t> m_caret_length = std::nullopt;
		std::optional<std::string_view> m_source_line = std::nullopt;
	};

	std::string_view StageLabel(CompilerStage stage, DiagnosticSeverity severity)
	{
		if (severity == DiagnosticSeverity::Error)
		{
			switch (stage)
			{
			case CompilerStage::Lexer:
				return "Lexer Error";
			case CompilerStage::Parser:
				return "Parser Error";
			case CompilerStage::TypeChecker:
				return "Type Checker Error";
			case CompilerStage::StaticAnalyzer:
				return "Static Analyzer Error";
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

		switch (stage)
		{
		case CompilerStage::Lexer:
			return "Lexer Warning";
		case CompilerStage::Parser:
			return "Parser Warning";
		case CompilerStage::TypeChecker:
			return "Type Checker Warning";
		case CompilerStage::StaticAnalyzer:
			return "Static Analyzer Warning";
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

	Printer::Color DiagnosticAccentColor(DiagnosticSeverity severity)
	{
		return severity == DiagnosticSeverity::Error
			? Printer::Color::BRIGHT_RED
			: Printer::Color::BRIGHT_YELLOW;
	}

	std::string RenderLabeledLine(Printer::Color label_color, Printer::Color message_color, std::string_view label, std::string_view message)
	{
		std::ostringstream oss;
		oss << Printer::Detail::GetColorCode(label_color);
		oss << "[" << label << "] ";
		oss << Printer::Detail::GetColorCode(message_color);
		oss << message;
		oss << "\033[0m";
		return oss.str();
	}

	std::string EscapeJsonString(std::string_view value)
	{
		std::string escaped;
		escaped.reserve(value.size());

		for (const unsigned char ch : value)
		{
			switch (ch)
			{
			case '\"':
				escaped += "\\\"";
				break;
			case '\\':
				escaped += "\\\\";
				break;
			case '\b':
				escaped += "\\b";
				break;
			case '\f':
				escaped += "\\f";
				break;
			case '\n':
				escaped += "\\n";
				break;
			case '\r':
				escaped += "\\r";
				break;
			case '\t':
				escaped += "\\t";
				break;
			default:
				if (ch < 0x20u)
				{
					escaped += std::format("\\u{:04X}", static_cast<unsigned int>(ch));
				}
				else
				{
					escaped.push_back(static_cast<char>(ch));
				}
				break;
			}
		}

		return escaped;
	}

	void AppendJsonFieldPrefix(std::string& out, std::string_view key, bool& first_field)
	{
		if (!first_field)
		{
			out.push_back(',');
		}
		first_field = false;

		out.push_back('\"');
		out += key;
		out += "\":";
	}

	void AppendJsonStringField(std::string& out, std::string_view key, std::optional<std::string_view> value, bool& first_field)
	{
		AppendJsonFieldPrefix(out, key, first_field);
		if (!value.has_value())
		{
			out += "null";
			return;
		}

		out.push_back('\"');
		out += EscapeJsonString(*value);
		out.push_back('\"');
	}

	void AppendJsonStringField(std::string& out, std::string_view key, std::string_view value, bool& first_field)
	{
		AppendJsonStringField(out, key, std::optional<std::string_view>(value), first_field);
	}

	template <typename NumberType>
	void AppendJsonNumberField(std::string& out, std::string_view key, std::optional<NumberType> value, bool& first_field)
	{
		AppendJsonFieldPrefix(out, key, first_field);
		if (!value.has_value())
		{
			out += "null";
			return;
		}

		out += std::to_string(*value);
	}

	std::string RenderCompilerDiagnostic(
		CompilerStage stage,
		std::string_view message,
		const std::optional<CompilerErrorLocation>& location,
		const std::optional<std::string>& suggestion,
		DiagnosticSeverity severity)
	{
		if (!location.has_value())
		{
			return std::string(message);
		}

		const CompilerErrorLocation& resolved_location = *location;
		std::ostringstream oss;

		oss << Printer::Detail::GetStyleCode(Printer::Style::BOLD);
		oss << Printer::Detail::GetColorCode(DiagnosticAccentColor(severity));
		oss << StageLabel(stage, severity);
		oss << "\033[0m";
		oss << " at ";
		oss << Printer::Detail::GetColorCode(Printer::Color::BRIGHT_CYAN);
		oss << resolved_location.m_file_name << ":" << resolved_location.m_line;
		oss << "\033[0m\n";

		if (resolved_location.m_source_line.has_value() && resolved_location.m_line > 0)
		{
			std::string_view source_line = *resolved_location.m_source_line;
			const std::string line_num_str = std::to_string(resolved_location.m_line);
			const int gutter_width = static_cast<int>(line_num_str.length()) + 1;

			oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
			for (int i = 0; i < gutter_width; i += 1)
			{
				oss << " ";
			}
			oss << "|\n";

			oss << line_num_str << " |";
			oss << "\033[0m";
			oss << " " << source_line << "\n";

			oss << Printer::Detail::GetColorCode(Printer::Color::BLUE);
			for (int i = 0; i < gutter_width; i += 1)
			{
				oss << " ";
			}
			oss << "|";
			oss << Printer::Detail::GetColorCode(DiagnosticAccentColor(severity));

			if (resolved_location.m_column.has_value())
			{
				oss << " ";
				for (int i = 0; i < *resolved_location.m_column; i += 1)
				{
					oss << " ";
				}

				const size_t caret_length = resolved_location.m_caret_length.value_or(1u);
				for (size_t i = 0u; i < caret_length; i += 1u)
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
			oss << *suggestion;
			oss << "\033[0m\n";
		}

		return oss.str();
	}

	TokenLocationContext GetTokenLocationContext(const Token& token, const std::vector<std::string>& source_lines)
	{
		TokenLocationContext context;
		if (token.m_line <= 0 || static_cast<size_t>(token.m_line) > source_lines.size())
		{
			return context;
		}

		context.m_source_line = source_lines[static_cast<size_t>(token.m_line - 1)];
		if (!token.m_column.has_value())
		{
			return context;
		}

		context.m_column = token.m_column;
		context.m_caret_length = std::max(token.m_source_length.value_or(0u), size_t(1u));
		return context;
	}
}

std::string RenderWarningGroupHeader(size_t warning_count, std::string_view file_path)
{
	const std::string summary = file_path.empty()
		? std::format("{} warning(s)\n", warning_count)
		: std::format("{} warning(s) in {}\n", warning_count, std::filesystem::path(file_path).filename().string());
	return RenderLabeledLine(Printer::Color::YELLOW, Printer::Color::WHITE, "warning", summary);
}

std::string SerializeMachineReadableWarning(const CompilerWarning& warning)
{
	std::string serialized = "MIDORI_WARNING\t{";
	bool first_field = true;

	AppendJsonStringField(serialized, "stage", CompilerStageName(warning.m_stage), first_field);
	AppendJsonStringField(serialized, "code", CompilerWarningCodeName(warning.m_code), first_field);

	std::optional<std::string_view> file_path = std::nullopt;
	std::optional<int> line = std::nullopt;
	std::optional<int> column = std::nullopt;
	std::optional<size_t> caret_length = std::nullopt;
	if (warning.m_location.has_value())
	{
		file_path = warning.m_location->m_file_name;
		if (warning.m_location->m_line > 0)
		{
			line = warning.m_location->m_line;
		}
		column = warning.m_location->m_column;
		caret_length = warning.m_location->m_caret_length;
	}
	const std::optional<std::string_view> suggestion =
		warning.m_suggestion.has_value() ? std::optional<std::string_view>(*warning.m_suggestion) : std::optional<std::string_view>(std::nullopt);

	AppendJsonStringField(serialized, "file_path", file_path, first_field);
	AppendJsonNumberField(serialized, "line", line, first_field);
	AppendJsonNumberField(serialized, "column", column, first_field);
	AppendJsonNumberField(serialized, "caret_length", caret_length, first_field);
	AppendJsonStringField(serialized, "message", std::string_view(warning.m_message), first_field);
	AppendJsonStringField(serialized, "suggestion", suggestion, first_field);
	serialized.push_back('}');
	return serialized;
}

namespace
{
	std::string RenderCompilerError(const CompilerError& error)
	{
		return RenderCompilerDiagnostic(error.m_stage, error.m_message, error.m_location, error.m_suggestion, DiagnosticSeverity::Error);
	}

	std::string RenderCompilerWarning(const CompilerWarning& warning)
	{
		return RenderCompilerDiagnostic(warning.m_stage, warning.m_message, warning.m_location, warning.m_suggestion, DiagnosticSeverity::Warning);
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

CompilerError CompilerError::Simple(CompilerStage stage, std::string_view message, CompilerErrorCode code)
{
	CompilerError error(stage, std::string(message));
	error.m_code = code;
	return error;
}

CompilerError CompilerError::WithContext(CompilerStage stage, std::string_view message, int line, std::string_view file_name, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, std::optional<std::string_view> source_line, CompilerErrorCode code)
{
	CompilerError error;
	error.m_stage = stage;
	error.m_code = code;
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

CompilerError CompilerError::WithToken(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion, CompilerErrorCode code)
{
	const TokenLocationContext context = GetTokenLocationContext(token, source_lines);
	return WithContext(stage, message, token.m_line, file_name, context.m_column, context.m_caret_length, suggestion, context.m_source_line, code);
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

CompilerWarning CompilerWarning::Simple(CompilerStage stage, std::string_view message, CompilerWarningCode code)
{
	CompilerWarning warning(stage, std::string(message));
	warning.m_code = code;
	return warning;
}

CompilerWarning CompilerWarning::WithContext(CompilerStage stage, std::string_view message, int line, std::string_view file_name, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, std::optional<std::string_view> source_line, CompilerWarningCode code)
{
	CompilerWarning warning;
	warning.m_stage = stage;
	warning.m_code = code;
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

CompilerWarning CompilerWarning::WithToken(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion, CompilerWarningCode code)
{
	const TokenLocationContext context = GetTokenLocationContext(token, source_lines);
	return WithContext(stage, message, token.m_line, file_name, context.m_column, context.m_caret_length, suggestion, context.m_source_line, code);
}

std::string_view CompilerWarning::Rendered() const
{
	return m_rendered.empty() ? std::string_view(m_message) : std::string_view(m_rendered);
}

CompilerError MidoriError::GenerateRichError(CompilerStage stage, std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, CompilerErrorCode code)
{
	std::optional<std::string_view> source_line = std::nullopt;
	if (line > 0 && static_cast<size_t>(line) <= source_lines.size())
	{
		source_line = source_lines[line - 1];
	}

	return CompilerError::WithContext(stage, message, line, file_name, column, caret_length, suggestion, source_line, code);
}

CompilerError MidoriError::GenerateRichError(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion, CompilerErrorCode code)
{
	return CompilerError::WithToken(stage, message, token, file_name, source_lines, suggestion, code);
}

CompilerError MidoriError::GenerateCodeGeneratorErrorWithContext(CompilerErrorCode code, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError(CompilerStage::CodeGenerator, message, token, file_name, source_lines, suggestion, code);
}

CompilerError MidoriError::GenerateCodeGeneratorErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateCodeGeneratorErrorWithContext(CompilerErrorCode::None, message, token, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateCodeGeneratorErrorWithContext(CompilerErrorCode code, std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Use unified implementation without column info (no caret)
	return GenerateRichError(CompilerStage::CodeGenerator, message, line, file_name, source_lines, std::nullopt, std::nullopt, suggestion, code);
}

CompilerError MidoriError::GenerateCodeGeneratorErrorWithContext(std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateCodeGeneratorErrorWithContext(CompilerErrorCode::None, message, line, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateLexerErrorWithContext(std::string_view message, int line, int column, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	// Use unified implementation with column and single caret
	return GenerateRichError(CompilerStage::Lexer, message, line, file_name, source_lines, column, 1, suggestion);
}

CompilerError MidoriError::GenerateModuleErrorWithContext(std::string_view message, int line, std::string_view file_name, std::optional<std::string_view> suggestion)
{
	return GenerateModuleErrorWithContext(CompilerErrorCode::None, message, line, file_name, suggestion);
}

CompilerError MidoriError::GenerateModuleErrorWithContext(CompilerErrorCode code, std::string_view message, int line, std::string_view file_name, std::optional<std::string_view> suggestion)
{
	return CompilerError::WithContext(CompilerStage::Module, message, line, file_name, std::nullopt, std::nullopt, suggestion, std::nullopt, code);
}

CompilerError MidoriError::GenerateParserErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError(CompilerStage::Parser, message, token, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateTypeCheckerErrorWithContext(CompilerErrorCode::None, message, token, file_name, source_lines, suggestion);
}

CompilerError MidoriError::GenerateTypeCheckerErrorWithContext(CompilerErrorCode code, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion)
{
	return GenerateRichError(CompilerStage::TypeChecker, message, token, file_name, source_lines, suggestion, code);
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
