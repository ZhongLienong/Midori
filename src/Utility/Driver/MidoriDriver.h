#pragma once

#include <expected>
#include <filesystem>
#include <optional>
#include <string>

#include "Compiler/Result/Result.h"

namespace MidoriDriver
{
	struct DriverError
	{
		std::string m_message;
		std::optional<CompilerError> m_compiler_error = std::nullopt;

		static DriverError FileSystem(std::string message);
		static DriverError Compilation(CompilerError compiler_error);

		[[nodiscard]] std::string Rendered() const;
	};

	using SourceReadResult = std::expected<std::string, DriverError>;
	using CompileFileResult = std::expected<MidoriExecutable, DriverError>;
	using RunResult = std::expected<int, CompilerError>;
	using DriverResult = std::expected<int, DriverError>;

	[[nodiscard]] SourceReadResult ReadSourceFile(const std::filesystem::path& file_path);
	[[nodiscard]] MidoriResult::CompilerResult CompileSource(std::string source_code, std::string file_name);
	[[nodiscard]] CompileFileResult CompileFile(const std::filesystem::path& file_path);
	[[nodiscard]] RunResult RunExecutable(MidoriExecutable&& executable);
	[[nodiscard]] DriverResult CompileAndRunFile(const std::filesystem::path& file_path);
}
