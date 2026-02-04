#pragma once

#include <array>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "Compiler/AbstractSyntaxTree/Type.h"
#include "Compiler/Token/Token.h"

enum class CompilerStage
{
	Lexer,
	Parser,
	TypeChecker,
	CodeGenerator,
	Module,
	Optimizer,
	BytecodeLinker,
	Compiler,
	Runtime,
	Unknown
};

enum class CompilerErrorCode
{
	None,
	NoMatch
};

enum class CompilerWarningCode
{
	None,
	NameShadowing
};

struct CompilerErrorLocation
{
	std::string m_file_name;
	int m_line = 0;
	std::optional<int> m_column = std::nullopt;
	std::optional<size_t> m_caret_length = std::nullopt;
	std::optional<std::string> m_source_line = std::nullopt;
};

struct CompilerError
{
	CompilerStage m_stage = CompilerStage::Unknown;
	CompilerErrorCode m_code = CompilerErrorCode::None;
	std::string m_message;
	std::optional<CompilerErrorLocation> m_location = std::nullopt;
	std::optional<std::string> m_suggestion = std::nullopt;
	std::string m_rendered;

	CompilerError() = default;
	CompilerError(std::string message);
	CompilerError(std::string_view message);
	CompilerError(const char* message);
	CompilerError(CompilerStage stage, std::string message);

	static CompilerError NoMatch();
	static CompilerError Simple(CompilerStage stage, std::string_view message);
	static CompilerError WithContext(CompilerStage stage, std::string_view message, int line, std::string_view file_name, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, std::optional<std::string_view> source_line = std::nullopt);
	static CompilerError WithToken(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	bool IsNoMatch() const;
	std::string_view Rendered() const;
};

struct CompilerWarning
{
	CompilerStage m_stage = CompilerStage::Unknown;
	CompilerWarningCode m_code = CompilerWarningCode::None;
	std::string m_message;
	std::optional<CompilerErrorLocation> m_location = std::nullopt;
	std::optional<std::string> m_suggestion = std::nullopt;
	std::string m_rendered;

	CompilerWarning() = default;
	CompilerWarning(std::string message);
	CompilerWarning(std::string_view message);
	CompilerWarning(const char* message);
	CompilerWarning(CompilerStage stage, std::string message);

	static CompilerWarning Simple(CompilerStage stage, std::string_view message);
	static CompilerWarning WithContext(CompilerStage stage, std::string_view message, int line, std::string_view file_name, std::optional<int> column, std::optional<size_t> caret_length, std::optional<std::string_view> suggestion, std::optional<std::string_view> source_line = std::nullopt);
	static CompilerWarning WithToken(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	std::string_view Rendered() const;
};

namespace std
{
	template<>
	struct formatter<CompilerError> : formatter<std::string_view>
	{
		auto format(const CompilerError& error, format_context& ctx) const
		{
			return formatter<std::string_view>::format(error.Rendered(), ctx);
		}
	};

	template<>
	struct formatter<CompilerWarning> : formatter<std::string_view>
	{
		auto format(const CompilerWarning& warning, format_context& ctx) const
		{
			return formatter<std::string_view>::format(warning.Rendered(), ctx);
		}
	};
}

class MidoriError
{
private:

	static CompilerError GenerateRichError(CompilerStage stage, std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<int> column = std::nullopt, std::optional<size_t> caret_length = std::nullopt, std::optional<std::string_view> suggestion = std::nullopt);

	static CompilerError GenerateRichError(CompilerStage stage, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

public:
	static CompilerError GenerateCodeGeneratorErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static CompilerError GenerateCodeGeneratorErrorWithContext(std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static CompilerError GenerateLexerErrorWithContext(std::string_view message, int line, int column, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static CompilerError GenerateModuleErrorWithContext(std::string_view message, int line, std::string_view file_name, std::optional<std::string_view> suggestion = std::nullopt);

	static CompilerError GenerateParserErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static CompilerError GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	template <typename... ExpectedTypes>
	static CompilerError GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, const std::shared_ptr<MidoriType>& actual = nullptr, ExpectedTypes&&... expected)
	{
		std::string full_message = std::string(message);

		if constexpr (sizeof...(expected) > 0)
		{
			std::array<std::string, sizeof...(expected)> expected_names{ expected->ToString()... };
			std::string expected_types;

			if constexpr (expected_names.size() > 1u)
			{
				for (size_t i = 0u; i < expected_names.size(); i += 1u)
				{
					expected_types.append(expected_names[i]);
					if (i != expected_names.size() - 1u)
					{
						expected_types.append(" or ");
					}
				}
			}
			else
			{
				expected_types = expected_names[0u];
			}

			full_message = std::format("{}\nExpected {}, but got {}", message, expected_types, actual->ToString());
		}

		return GenerateRichError(CompilerStage::TypeChecker, full_message, token, file_name, source_lines);
	}

	static std::string GenerateRuntimeError(std::string_view message, int line);
};
