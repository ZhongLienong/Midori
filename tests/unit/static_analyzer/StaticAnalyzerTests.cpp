#include <catch2/catch_test_macros.hpp>

#include "support/CompileHelpers.h"
#include "support/DiagnosticMatchers.h"

#include <expected>
#include <string>

namespace
{
	void RequireWarningMatches(const std::vector<CompilerWarning>& warnings, CompilerWarningCode code, const MidoriTest::WarningExpectation& expectation)
	{
		const CompilerWarning* warning = MidoriTest::FindWarning(warnings, code);
		REQUIRE(warning != nullptr);

		std::string mismatch;
		const bool matched = MidoriTest::Matches(*warning, expectation, &mismatch);
		CAPTURE(mismatch);
		REQUIRE(matched);
	}
}

TEST_CASE("StaticAnalyzer reports unread locals with source context", "[static-analyzer]")
{
	const std::string source_code =
		R"(module AnalyzerUnused
defun Compute() : Int => {
	def used = 1;
	def unused = 2;
	used
};
)";

	std::expected<MidoriTest::AnalyzedSnippet, CompilerError> analyze_result = MidoriTest::AnalyzeSnippet(source_code, "AnalyzerUnused.mdr");
	if (!analyze_result.has_value())
	{
		FAIL(std::string(analyze_result.error().Rendered()));
	}

	REQUIRE(analyze_result->m_errors.empty());
	RequireWarningMatches(
		analyze_result->m_warnings,
		CompilerWarningCode::UnusedLocal,
		MidoriTest::WarningExpectation
		{
			.m_stage = CompilerStage::StaticAnalyzer,
			.m_code = CompilerWarningCode::UnusedLocal,
			.m_line = 4,
			.m_message_substrings = { "Binding 'unused' is never read." },
			.m_rendered_substrings = { "AnalyzerUnused.mdr:4", "def unused = 2;" }
		});
}

TEST_CASE("StaticAnalyzer flags unreachable final expressions after an early return", "[static-analyzer]")
{
	const std::string source_code =
		R"(module AnalyzerUnreachable
defun Compute() : Int => {
	return 1;
	2
};
)";

	std::expected<MidoriTest::AnalyzedSnippet, CompilerError> analyze_result = MidoriTest::AnalyzeSnippet(source_code, "AnalyzerUnreachable.mdr");
	if (!analyze_result.has_value())
	{
		FAIL(std::string(analyze_result.error().Rendered()));
	}

	REQUIRE(analyze_result->m_errors.empty());
	RequireWarningMatches(
		analyze_result->m_warnings,
		CompilerWarningCode::UnreachableCode,
		MidoriTest::WarningExpectation
		{
			.m_stage = CompilerStage::StaticAnalyzer,
			.m_code = CompilerWarningCode::UnreachableCode,
			.m_line = 4,
			.m_message_substrings = { "unreachable" },
			.m_rendered_substrings = { "AnalyzerUnreachable.mdr:4", "2" }
		});
}

TEST_CASE("StaticAnalyzer warns when an inner scope shadows an outer binding", "[static-analyzer]")
{
	const std::string source_code =
		R"(module AnalyzerShadowing
defun Compute(value : Int) : Int => {
	{
		def value = 2;
		value
	};
	value
};
)";

	std::expected<MidoriTest::AnalyzedSnippet, CompilerError> analyze_result = MidoriTest::AnalyzeSnippet(source_code, "AnalyzerShadowing.mdr");
	if (!analyze_result.has_value())
	{
		FAIL(std::string(analyze_result.error().Rendered()));
	}

	REQUIRE(analyze_result->m_errors.empty());
	RequireWarningMatches(
		analyze_result->m_warnings,
		CompilerWarningCode::NameShadowing,
		MidoriTest::WarningExpectation
		{
			.m_stage = CompilerStage::StaticAnalyzer,
			.m_code = CompilerWarningCode::NameShadowing,
			.m_line = 4,
			.m_message_substrings = { "Name 'value' shadows a variable from an outer scope." },
			.m_rendered_substrings = { "AnalyzerShadowing.mdr:4", "def value = 2;" }
		});
}

TEST_CASE("StaticAnalyzer reports captured closures that escape as return values", "[static-analyzer]")
{
	const std::string source_code =
		R"(module AnalyzerCapture
defun MakeCounter() : fn() -> Int => {
	def value = 1;
	def next = fn() : Int => value;
	return next;
};
)";

	std::expected<MidoriTest::AnalyzedSnippet, CompilerError> analyze_result = MidoriTest::AnalyzeSnippet(source_code, "AnalyzerCapture.mdr");
	if (!analyze_result.has_value())
	{
		FAIL(std::string(analyze_result.error().Rendered()));
	}

	REQUIRE(analyze_result->m_errors.empty());
	RequireWarningMatches(
		analyze_result->m_warnings,
		CompilerWarningCode::CaptureEscape,
		MidoriTest::WarningExpectation
		{
			.m_stage = CompilerStage::StaticAnalyzer,
			.m_code = CompilerWarningCode::CaptureEscape,
			.m_line = 5,
			.m_message_substrings = { "Captured closure 'next' escapes its defining scope as a return value." },
			.m_rendered_substrings = { "AnalyzerCapture.mdr:5", "return next;" }
		});
}
