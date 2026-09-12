#include <cstddef>
#include <optional>
#include <string>

#include <catch2/catch_test_macros.hpp>

#include "Common/Constant/Constant.h"
#include "Common/Executable/Executable.h"
#include "Compiler/BytecodeModule/BytecodeModule.h"
#include "support/CompileHelpers.h"

namespace
{
	// Pins CodeGenerator::IsFreshConcatTemporary (CodeGenerator.cpp) at the
	// bytecode level, independent of both the optimizer pipeline and observable
	// program behaviour.
	//
	// test/prelude/success/concat_does_not_mutate_aliases.mdr pins the same
	// property end-to-end, but it can only observe *behaviour*: it runs the
	// full compiler, including ConstantFolding, and checks printed values. If
	// a future pass learned to fold an array index into a literal - or a
	// future constant-propagation pass started covering module-level globals,
	// which LocalConstantPropagation does not today (it is scoped to
	// NameContext::Local) - several of that file's probes would stop reaching
	// EXTEND_TEXT at all while still printing the same, correct output. The
	// .mdr test would keep passing and nobody would notice that it had
	// stopped testing anything. This is exactly the failure mode this branch
	// has hit before: a benchmark that timed a stack overflow, and the
	// vacuous literal-only Text Case 6 found in review of this test's first
	// commit.
	//
	// GenerateBytecodeSnippetWithDiagnostics calls the type checker and
	// CodeGenerator directly, bypassing OptimizerManager entirely, so these
	// two cases assert on IsFreshConcatTemporary's classification of the raw
	// AST shape and cannot be neutralised by any future optimizer change.

	[[nodiscard]] std::optional<std::size_t> FindMainProcedureIndex(const BytecodeModule& module)
	{
		for (std::size_t index = 0u; index < module.m_procedure_names.size(); index += 1u)
		{
			if (module.m_procedure_names[index].starts_with(MAIN_PROCEDURE_PREFIX))
			{
				return index;
			}
		}

		return std::nullopt;
	}

	[[nodiscard]] bool ContainsOpCode(const BytecodeStream& procedure, OpCode target)
	{
		for (BytecodeStream::const_iterator it = procedure.cbegin(); it != procedure.cend(); ++it)
		{
			if (*it == target)
			{
				return true;
			}
		}

		return false;
	}

	[[nodiscard]] const BytecodeStream& MainProcedureOrFail(const BytecodeModule& module)
	{
		const std::optional<std::size_t> main_index = FindMainProcedureIndex(module);
		if (!main_index.has_value())
		{
			FAIL("Compiled module has no $main$ procedure.");
		}

		return module.m_procedures[main_index.value()];
	}

	// The Case 2/6 shape: a fresh text literal on the left, and a right
	// operand read through an array index so it cannot be mistaken for a
	// second literal by anything that inspects only the AST's leaves.
	const std::string EXTEND_SHAPE_SOURCE =
		"module ExtendTextProbe\n"
		"\n"
		"def src : Array<Text> = [\"y\"];\n"
		"def rt = src[0];\n"
		"def result = \"x\" ++ rt;\n";

	// The Case 1 shape: the left operand is a NameAccess to an already-bound
	// binding, never a literal, so IsFreshConcatTemporary must reject it.
	const std::string CONCAT_SHAPE_SOURCE =
		"module ConcatTextProbe\n"
		"\n"
		"def a = \"x\";\n"
		"def result = a ++ \"y\";\n";
}

TEST_CASE("A text literal left operand of ++ emits EXTEND_TEXT", "[compiler][codegen][concat]")
{
	std::expected<BytecodeModule, MidoriResult::CompilerDiagnostics> module_result =
		MidoriTest::GenerateBytecodeSnippetWithDiagnostics(EXTEND_SHAPE_SOURCE, "ExtendTextProbe.mdr");
	REQUIRE(module_result.has_value());

	const BytecodeStream& main_procedure = MainProcedureOrFail(module_result.value());
	REQUIRE(ContainsOpCode(main_procedure, OpCode::EXTEND_TEXT));
}

TEST_CASE("A NameAccess left operand of ++ emits CONCAT_TEXT and never EXTEND_TEXT", "[compiler][codegen][concat]")
{
	std::expected<BytecodeModule, MidoriResult::CompilerDiagnostics> module_result =
		MidoriTest::GenerateBytecodeSnippetWithDiagnostics(CONCAT_SHAPE_SOURCE, "ConcatTextProbe.mdr");
	REQUIRE(module_result.has_value());

	const BytecodeStream& main_procedure = MainProcedureOrFail(module_result.value());
	REQUIRE(ContainsOpCode(main_procedure, OpCode::CONCAT_TEXT));
	REQUIRE_FALSE(ContainsOpCode(main_procedure, OpCode::EXTEND_TEXT));
}
