#include <filesystem>
#include <format>
#include <print>
#include <string>
#include <string_view>
#include <system_error>

#include "Common/BuildConfig/BuildConfig.h"
#include "Common/Printer/Printer.h"
#include "Utility/Driver/MidoriDriver.h"
#include "Utility/Project/ProjectManifest.h"

namespace
{
	void PrintUsage()
	{
		Printer::Print<Printer::Color::BRIGHT_WHITE>("Usage:\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe <source_file_path>\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe check <source_file_path> [--format json]\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe init [path] [--name <project_name>]\n");
		Printer::Print<Printer::Color::BRIGHT_WHITE>("  Midori.exe init --package [path] [--name <package_name>]\n");
	}

	MidoriResult::CompilerReport WrapDriverErrorAsReport(const MidoriDriver::DriverError& error)
	{
		if (error.m_report.has_value())
		{
			return *error.m_report;
		}

		return MidoriResult::CompilerReport(MidoriResult::CompilerDiagnostics(
			CompilerError::Simple(CompilerStage::Compiler, error.m_message)));
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

	int HandleCheck(int argc, char* argv[])
	{
		std::filesystem::path source_file_path;
		bool json_output = false;

		for (int i = 2; i < argc; i += 1)
		{
			const std::string_view arg = argv[i];
			if (arg == "--format")
			{
				if (i + 1 >= argc)
				{
					Printer::Print<Printer::Color::RED>("Missing value for --format.\n");
					PrintUsage();
					return EXIT_FAILURE;
				}

				const std::string_view format = argv[++i];
				if (format == "json")
				{
					json_output = true;
					continue;
				}

				Printer::Print<Printer::Color::RED>(std::format("Unknown format: {}\n", format));
				PrintUsage();
				return EXIT_FAILURE;
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

			if (!source_file_path.empty())
			{
				Printer::Print<Printer::Color::RED>("Only one source file is allowed for check.\n");
				PrintUsage();
				return EXIT_FAILURE;
			}

			source_file_path = std::filesystem::path(arg);
		}

		if (source_file_path.empty())
		{
			Printer::Print<Printer::Color::RED>("Missing source file for check.\n");
			PrintUsage();
			return EXIT_FAILURE;
		}

		const MidoriBuild::ScopedTestModeOverride suppress_internal_diagnostics(true);
		const MidoriDriver::CompileFileWithReportResult compile_result = MidoriDriver::CompileFileWithReport(source_file_path);
		if (!compile_result.has_value())
		{
			if (json_output)
			{
				std::print("{}", WrapDriverErrorAsReport(compile_result.error()).MachineReadableJson());
			}
			else
			{
				std::print("{}", compile_result.error().Rendered());
			}
			return EXIT_FAILURE;
		}

		const MidoriResult::CompilerReport& report = compile_result->Report();
		if (json_output)
		{
			std::print("{}", report.MachineReadableJson());
		}
		else
		{
			std::print("{}", report.RenderedWarnings());
		}

		return EXIT_SUCCESS;
	}
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

	if (command == "check")
	{
		return HandleCheck(argc, argv);
	}

	if (command == "-h" || command == "--help")
	{
		PrintUsage();
		return EXIT_SUCCESS;
	}

	const MidoriDriver::DriverResult run_result = MidoriDriver::CompileAndRunFile(std::filesystem::path(argv[1u]));
	if (!run_result.has_value())
	{
		std::print("{}", run_result.error().Rendered());
		return EXIT_FAILURE;
	}

	return run_result.value();
}
