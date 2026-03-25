#include "Utility/Driver/MidoriDriver.h"

#include <format>
#include <fstream>
#include <sstream>
#include <system_error>
#include <utility>

#include "Compiler/Compiler.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"
#include "Utility/Project/ProjectManifest.h"

namespace
{
	[[nodiscard]] std::filesystem::path ResolveManifestInputPath(const std::filesystem::path& file_path)
	{
		std::error_code path_error;
		std::filesystem::path resolved_path = std::filesystem::absolute(file_path, path_error);
		if (path_error)
		{
			return file_path;
		}

		return resolved_path;
	}
}

namespace MidoriDriver
{
	DriverError DriverError::FileSystem(std::string message)
	{
		DriverError error;
		error.m_message = std::move(message);
		return error;
	}

	DriverError DriverError::Compilation(CompilerError compiler_error)
	{
		DriverError error;
		error.m_compiler_error = std::move(compiler_error);
		return error;
	}

	std::string DriverError::Rendered() const
	{
		if (m_compiler_error.has_value())
		{
			return std::format("Compilation failed :( \n{}", m_compiler_error.value());
		}

		return m_message;
	}

	SourceReadResult ReadSourceFile(const std::filesystem::path& file_path)
	{
		std::ifstream file(file_path, std::ios::binary);
		if (!file.is_open())
		{
			return std::unexpected(DriverError::FileSystem(std::format("Could not open file: {}\n", file_path.string())));
		}

		std::ostringstream buffer;
		buffer << file.rdbuf();
		if (!buffer)
		{
			return std::unexpected(DriverError::FileSystem(std::format("Could not read file to buffer: {}\n", file_path.string())));
		}

		return buffer.str();
	}

	MidoriResult::CompilerResult CompileSource(std::string source_code, std::string file_name)
	{
		return Compiler(std::move(source_code), std::move(file_name)).Compile();
	}

	CompileFileResult CompileFile(const std::filesystem::path& file_path)
	{
		const std::filesystem::path manifest_input_path = ResolveManifestInputPath(file_path);
		MidoriProject::ApplyProjectManifestToEnvironment(manifest_input_path);

		SourceReadResult source_result = ReadSourceFile(file_path);
		if (!source_result.has_value())
		{
			return std::unexpected(std::move(source_result.error()));
		}

		MidoriResult::CompilerResult compile_result = CompileSource(std::move(source_result.value()), file_path.string());
		if (!compile_result.has_value())
		{
			return std::unexpected(DriverError::Compilation(std::move(compile_result.error())));
		}

		return std::move(compile_result.value());
	}

	RunResult RunExecutable(MidoriExecutable&& executable)
	{
		VirtualMachine vm(std::move(executable));
		return vm.Execute();
	}

	DriverResult CompileAndRunFile(const std::filesystem::path& file_path)
	{
		CompileFileResult compile_result = CompileFile(file_path);
		if (!compile_result.has_value())
		{
			return std::unexpected(std::move(compile_result.error()));
		}

		RunResult run_result = RunExecutable(std::move(compile_result.value()));
		if (!run_result.has_value())
		{
			return std::unexpected(DriverError::Compilation(std::move(run_result.error())));
		}

		return run_result.value();
	}
}
