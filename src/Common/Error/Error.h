#pragma once

#include <format>
#include <vector>
#include <optional>

#include "Compiler/AbstractSyntaxTree/Type.h"
#include "Compiler/Token/Token.h"

class MidoriError
{
private:

	static std::string GenerateRichError(std::string_view error_type, std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<int> column = std::nullopt, std::optional<size_t> caret_length = std::nullopt, std::optional<std::string_view> suggestion = std::nullopt);

	static std::string GenerateRichError(std::string_view error_type, std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

public:
	static std::string GenerateCodeGeneratorErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static std::string GenerateCodeGeneratorErrorWithContext(std::string_view message, int line, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static std::string GenerateLexerErrorWithContext(std::string_view message, int line, int column, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static std::string GenerateModuleErrorWithContext(std::string_view message, int line, std::string_view file_name, std::optional<std::string_view> suggestion = std::nullopt);

	static std::string GenerateParserErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	static std::string GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, std::optional<std::string_view> suggestion = std::nullopt);

	template <typename... ExpectedTypes>
	static std::string GenerateTypeCheckerErrorWithContext(std::string_view message, const Token& token, std::string_view file_name, const std::vector<std::string>& source_lines, const std::shared_ptr<MidoriType>& actual = nullptr, ExpectedTypes&&... expected)
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

		return GenerateRichError("Type Checker Error", full_message, token, file_name, source_lines);
	}

	static std::string GenerateRuntimeError(std::string_view message, int line);
};