#include <catch2/catch_test_macros.hpp>

#include "support/CompileHelpers.h"
#include "support/TempDir.h"

#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <string>

namespace
{
	std::filesystem::path RepositoryRoot()
	{
		return std::filesystem::path(__FILE__).parent_path().parent_path().parent_path().parent_path();
	}

	std::string MidoriPathLiteral(const std::filesystem::path& path)
	{
		std::error_code error_code;
		const std::filesystem::path canonical_path = std::filesystem::weakly_canonical(path, error_code);
		if (!error_code)
		{
			return canonical_path.generic_string();
		}

		return path.lexically_normal().generic_string();
	}

	const MidoriTest::ExecutedSnippet& RequireExecutedSnippet(const std::expected<MidoriTest::ExecutedSnippet, CompilerError>& run_result)
	{
		if (!run_result.has_value())
		{
			FAIL(std::string(run_result.error().Rendered()));
		}

		return run_result.value();
	}
}

TEST_CASE("Joining a cancelled spinning worker reports a cancellation error", "[runtime][worker][cancel]")
{
	const std::filesystem::path system_module_path = RepositoryRoot() / "MidoriPrelude" / "System.mdr";
	const MidoriTest::TempDir temp_dir("midori-worker-cancel");
	const std::filesystem::path source_file_path = temp_dir.Path() / "WorkerCancelJoin.mdr";

	const std::string source_code = std::format(
		R"(module WorkerCancelJoin
import {{ "{}" }}
defun Spin(dummy: Int) : Int => {{
    def i = 0;
    loop
    {{
        i = i + 1;
        if i < 0 then break () else ();
    }};
    i
}};
def w = spawn Spin(0);
System::Sleep(50);
def cancelled = cancel(w);
def r = join w;
defun main(): Int => 0;
)",
		MidoriPathLiteral(system_module_path));

	const std::expected<MidoriTest::ExecutedSnippet, CompilerError> run_result =
		MidoriTest::ExecuteSnippet(source_code, source_file_path.string());
	const MidoriTest::ExecutedSnippet& executed = RequireExecutedSnippet(run_result);

	REQUIRE(executed.m_exit_code != EXIT_SUCCESS);
	REQUIRE(executed.m_output.m_stdout.find("cancelled") != std::string::npos);
}

TEST_CASE("Joining a cancelled tail-recursive worker reports a cancellation error", "[runtime][worker][cancel]")
{
	const std::filesystem::path system_module_path = RepositoryRoot() / "MidoriPrelude" / "System.mdr";
	const MidoriTest::TempDir temp_dir("midori-worker-cancel-tail");
	const std::filesystem::path source_file_path = temp_dir.Path() / "WorkerCancelTailJoin.mdr";

	const std::string source_code = std::format(
		R"(module WorkerCancelTailJoin
import {{ "{}" }}
defun SpinTail(i: Int) : Int => SpinTail(i + 1);
def w = spawn SpinTail(0);
System::Sleep(50);
def cancelled = cancel(w);
def r = join w;
defun main(): Int => 0;
)",
		MidoriPathLiteral(system_module_path));

	const std::expected<MidoriTest::ExecutedSnippet, CompilerError> run_result =
		MidoriTest::ExecuteSnippet(source_code, source_file_path.string());
	const MidoriTest::ExecutedSnippet& executed = RequireExecutedSnippet(run_result);

	REQUIRE(executed.m_exit_code != EXIT_SUCCESS);
	REQUIRE(executed.m_output.m_stdout.find("cancelled") != std::string::npos);
}

TEST_CASE("Joining a cancelled worker preserves the WorkerCancelled error code", "[runtime][worker][cancel]")
{
	const std::filesystem::path system_module_path = RepositoryRoot() / "MidoriPrelude" / "System.mdr";
	const MidoriTest::TempDir temp_dir("midori-worker-cancel-code");
	const std::filesystem::path source_file_path = temp_dir.Path() / "WorkerCancelCode.mdr";

	const std::string source_code = std::format(
		R"(module WorkerCancelCode
import {{ "{}" }}
defun Spin(_dummy: Int) : Int => {{
    def i = 0;
    loop
    {{
        i = i + 1;
        if i < 0 then break () else ();
    }};
    i
}};
def w = spawn Spin(0);
System::Sleep(50);
def cancelled = cancel(w);
def r = join w;
defun main(): Int => 0;
)",
		MidoriPathLiteral(system_module_path));

	const std::expected<MidoriTest::ExecutedSnippet, CompilerError> run_result =
		MidoriTest::ExecuteSnippet(source_code, source_file_path.string());
	const MidoriTest::ExecutedSnippet& executed = RequireExecutedSnippet(run_result);

	REQUIRE(executed.m_exit_code != EXIT_SUCCESS);
	REQUIRE(executed.m_output.m_stdout.find("error[WorkerCancelled]") != std::string::npos);
	REQUIRE(executed.m_output.m_stdout.find("InternalTypeError") == std::string::npos);
}
