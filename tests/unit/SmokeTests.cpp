#include <cstdlib>
#include <string>
#include <utility>

#include <catch2/catch_test_macros.hpp>

#include "Utility/Driver/MidoriDriver.h"

TEST_CASE("MidoriCore links into the unit test target", "[smoke]")
{
	std::string source_code =
		"module Main\n"
		"\n"
		"defun main(): Int => 0;\n";

	MidoriResult::CompilerResult compile_result = MidoriDriver::CompileSource(std::move(source_code), "Smoke.mdr");
	if (!compile_result.has_value())
	{
		FAIL(std::string(compile_result.error().Rendered()));
	}

	MidoriDriver::RunResult run_result = MidoriDriver::RunExecutable(std::move(compile_result.value()));
	if (!run_result.has_value())
	{
		FAIL(std::string(run_result.error().Rendered()));
	}

	REQUIRE(run_result.value() == EXIT_SUCCESS);
}
