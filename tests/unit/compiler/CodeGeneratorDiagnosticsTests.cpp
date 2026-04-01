#include <string>

#include <catch2/catch_test_macros.hpp>

#include "support/CompileHelpers.h"
#include "support/DiagnosticMatchers.h"

namespace
{
	void RequireErrorMatches(const CompilerError& error, const MidoriTest::ErrorExpectation& expectation)
	{
		std::string mismatch;
		const bool matched = MidoriTest::Matches(error, expectation, &mismatch);
		CAPTURE(mismatch);
		REQUIRE(matched);
	}

	std::string UnsupportedForeignReturnSource()
	{
		return
			R"(module ForeignDiagnostics
struct Pair
{
	value: Int
};
foreign "MIDORI_FFI_ReadPairA" ReadPairA : fn() -> Pair;
foreign "MIDORI_FFI_ReadPairB" ReadPairB : fn() -> Pair;
defun main(): Int => {
	ReadPairA();
	ReadPairB();
	return 0;
};
)";
	}
}

TEST_CASE("CodeGenerator preserves structured diagnostics for recoverable lowering failures", "[compiler][codegen][diagnostics]")
{
	std::expected<BytecodeModule, MidoriResult::CompilerDiagnostics> bytecode_result =
		MidoriTest::GenerateBytecodeSnippetWithDiagnostics(UnsupportedForeignReturnSource(), "ForeignDiagnostics.mdr");
	REQUIRE_FALSE(bytecode_result.has_value());
	REQUIRE(bytecode_result.error().Size() == 2u);
	REQUIRE(MidoriTest::FindError(bytecode_result.error(), CompilerStage::CodeGenerator, CompilerErrorCode::CodeGeneratorUnsupportedLowering) != nullptr);

	MidoriTest::ErrorExpectation first_expectation;
	first_expectation.m_stage = CompilerStage::CodeGenerator;
	first_expectation.m_code = CompilerErrorCode::CodeGeneratorUnsupportedLowering;
	first_expectation.m_line = 6;
	first_expectation.m_message_substrings = { "Unsupported return type for foreign function" };
	first_expectation.m_rendered_substrings = { "Code Generator Error", "ForeignDiagnostics.mdr:6" };
	RequireErrorMatches(bytecode_result.error().m_errors[0u], first_expectation);

	MidoriTest::ErrorExpectation second_expectation;
	second_expectation.m_stage = CompilerStage::CodeGenerator;
	second_expectation.m_code = CompilerErrorCode::CodeGeneratorUnsupportedLowering;
	second_expectation.m_line = 7;
	second_expectation.m_message_substrings = { "Unsupported return type for foreign function" };
	second_expectation.m_rendered_substrings = { "Code Generator Error", "ForeignDiagnostics.mdr:7" };
	RequireErrorMatches(bytecode_result.error().m_errors[1u], second_expectation);
}

TEST_CASE("Compiler preserves all codegen diagnostics through the compile boundary", "[compiler][codegen][diagnostics]")
{
	MidoriResult::CompilerResult compile_result = MidoriTest::CompileSnippet(UnsupportedForeignReturnSource(), "ForeignDiagnostics.mdr");
	REQUIRE_FALSE(compile_result.has_value());
	REQUIRE(compile_result.error().Size() == 2u);
	REQUIRE(MidoriTest::FindError(compile_result.error(), CompilerStage::CodeGenerator, CompilerErrorCode::CodeGeneratorUnsupportedLowering) != nullptr);

	MidoriTest::ErrorExpectation expectation;
	expectation.m_stage = CompilerStage::CodeGenerator;
	expectation.m_code = CompilerErrorCode::CodeGeneratorUnsupportedLowering;
	expectation.m_line = 6;
	expectation.m_message_substrings = { "Unsupported return type for foreign function" };
	expectation.m_rendered_substrings = { "Code Generator Error", "ForeignDiagnostics.mdr:6" };
	RequireErrorMatches(compile_result.error().m_errors[0u], expectation);

	MidoriTest::ErrorExpectation second_expectation;
	second_expectation.m_stage = CompilerStage::CodeGenerator;
	second_expectation.m_code = CompilerErrorCode::CodeGeneratorUnsupportedLowering;
	second_expectation.m_line = 7;
	second_expectation.m_message_substrings = { "Unsupported return type for foreign function" };
	second_expectation.m_rendered_substrings = { "Code Generator Error", "ForeignDiagnostics.mdr:7" };
	RequireErrorMatches(compile_result.error().m_errors[1u], second_expectation);
}
