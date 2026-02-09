#include <filesystem>
#include <format>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>

#include "Common/Printer/Printer.h"
#include "Compiler/Compiler.h"
#include "Interpreter/Runtime/MidoriRuntime.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"
#include "Utility/Project/ProjectManifest.h"

using namespace std::string_literals;

namespace
{
	void PrintUsage()
	{
		Printer::Print<Printer::Color::BRIGHT_WHITE>("Usage:\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe <source_file_path>\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe init [path] [--name <project_name>]\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe init --package [path] [--name <package_name>]\n");
	}

	int HandleInit(int argc, char* argv[])
	{
		std::filesystem::path target_dir;
		std::string init_name;
		bool init_package = false;

		for (int i = 2; i < argc; i += 1)
		{
			const std::string_view arg = argv[i];
			if (arg == "--name")
			{
				if (i + 1 >= argc)
				{
					Printer::Print<Printer::Color::RED>("Missing value for --name.\n");
					PrintUsage();
					return EXIT_FAILURE;
				}
				init_name = argv[++i];
				continue;
			}

			if (arg == "--package")
			{
				init_package = true;
				continue;
			}

			if (arg == "-h" || arg == "--help")
			{
				PrintUsage();
				return EXIT_SUCCESS;
			}

			if (!arg.empty() && arg.front() == '-')
			{
				Printer::Print<Printer::Color::RED>(std::format("Unknown option: {}\n", arg));
				PrintUsage();
				return EXIT_FAILURE;
			}

			if (!target_dir.empty())
			{
				Printer::Print<Printer::Color::RED>("Only one target directory is allowed for init.\n");
				PrintUsage();
				return EXIT_FAILURE;
			}

			target_dir = std::filesystem::path(arg);
		}

		std::string error_message;
		if (init_package)
		{
			if (!MidoriPackage::InitializePackage(target_dir, init_name, error_message))
			{
				Printer::Print<Printer::Color::RED>(std::format("Package init failed: {}\n", error_message));
				return EXIT_FAILURE;
			}
		}
		else if (!MidoriProject::InitializeProject(target_dir, init_name, error_message))
		{
			Printer::Print<Printer::Color::RED>(std::format("Project init failed: {}\n", error_message));
			return EXIT_FAILURE;
		}

		std::error_code ec;
		std::filesystem::path resolved_target = target_dir;
		if (resolved_target.empty())
		{
			resolved_target = std::filesystem::current_path(ec);
			if (ec)
			{
				resolved_target = ".";
			}
		}

		const char* init_label = init_package ? "package" : "project";
		Printer::Print<Printer::Color::GREEN>(
			std::format("Initialized Midori {} at {}\n", init_label, resolved_target.string()));
		return EXIT_SUCCESS;
	}
}

std::string ReadFile(const char* filename)
{
	std::ifstream file(filename);
	if (!file.is_open())
	{
		Printer::Print<Printer::Color::RED>(std::format("Could not open file: {}\n", filename));
		std::exit(EXIT_FAILURE);
	}

	std::ostringstream buffer;
	buffer << file.rdbuf();
	if (!buffer)
	{
		Printer::Print<Printer::Color::RED>(std::format("Could not read file to buffer: {}\n", filename));
		std::exit(EXIT_FAILURE);
	}

	return buffer.str();
}

int main(int argc, char* argv[])
{
	if (argc < 2)
	{
		PrintUsage();
		return EXIT_FAILURE;
	}

	const std::string_view command = argv[1u];
	if (command == "init")
	{
		return HandleInit(argc, argv);
	}

	if (command == "-h" || command == "--help")
	{
		PrintUsage();
		return EXIT_SUCCESS;
	}

	std::string file_name = argv[1u];
	std::error_code path_error;
	std::filesystem::path input_path = std::filesystem::absolute(std::filesystem::path(file_name), path_error);
	if (path_error)
	{
		input_path = std::filesystem::path(file_name);
	}
	MidoriProject::ApplyProjectManifestToEnvironment(input_path);
	std::string file_content = ReadFile(file_name.data());

	return Compiler(std::move(file_content), std::move(file_name))
		.Compile()
		.and_then
		(
			[](MidoriExecutable&& executable) -> std::expected<int, CompilerError>
			{
				MidoriRuntime runtime(std::move(executable));
				VirtualMachine vm(runtime);
				return vm.Execute();
			}
		)
		.or_else
		(
			[](CompilerError&& compilation_error) -> std::expected<int, CompilerError>
			{
				Printer::Print<Printer::Color::RED>("Compilation failed :( \n");
				Printer::Print<Printer::Color::RED>(std::format("{}", compilation_error));
				return EXIT_FAILURE;
			}
		)
		.value();
}
