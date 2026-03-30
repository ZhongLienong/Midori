#include <catch2/catch_test_macros.hpp>

#include "Common/Constant/Constant.h"
#include "Compiler/Token/Token.h"
#include "support/CompileHelpers.h"
#include "support/TempProject.h"

#include <expected>
#include <format>
#include <memory>
#include <string>
#include <string_view>

namespace
{
	const MidoriStatement::VariableDefinition& RequireVariableDefinition(const MidoriProgramTree& program, size_t index, std::string_view expected_name)
	{
		REQUIRE(index < program.size());
		REQUIRE(program[index] != nullptr);
		REQUIRE(program[index]->IsStatement<MidoriStatement::VariableDefinition>());

		const MidoriStatement::VariableDefinition& definition = program[index]->GetStatement<MidoriStatement::VariableDefinition>();
		REQUIRE(definition.m_name.m_lexeme == expected_name);
		return definition;
	}

	template<typename ExpressionType>
	const ExpressionType& RequireExpression(const std::unique_ptr<MidoriExpression>& expression)
	{
		REQUIRE(expression != nullptr);
		REQUIRE(expression->template IsExpression<ExpressionType>());
		return expression->template GetExpression<ExpressionType>();
	}

	template<typename PatternType>
	const PatternType& RequirePattern(const std::unique_ptr<MidoriPattern>& pattern)
	{
		REQUIRE(pattern != nullptr);
		REQUIRE(pattern->template IsPattern<PatternType>());
		return pattern->template GetPattern<PatternType>();
	}
}

TEST_CASE("Parser preserves arithmetic precedence and tuple element grouping", "[parser]")
{
	const std::string source_code =
		R"(module ParserPrecedence
def arithmetic = 1 + 2 * 3;
def tuple_value = (1 + 2, 3);
)";

	std::expected<MidoriTest::ParsedSnippet, CompilerError> parse_result = MidoriTest::ParseSnippet(source_code, "ParserPrecedence.mdr");
	if (!parse_result.has_value())
	{
		FAIL(std::string(parse_result.error().Rendered()));
	}

	const MidoriProgramTree& program = parse_result->m_program;
	REQUIRE(program.size() == 2u);

	const MidoriStatement::VariableDefinition& arithmetic_definition = RequireVariableDefinition(program, 0u, "arithmetic");
	const MidoriExpression::Binary& arithmetic_expr = RequireExpression<MidoriExpression::Binary>(arithmetic_definition.m_value);
	REQUIRE(arithmetic_expr.m_op.m_token_name == Token::Name::SINGLE_PLUS);
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(arithmetic_expr.m_left).m_token.m_lexeme == "1");

	const MidoriExpression::Binary& multiplied_expr = RequireExpression<MidoriExpression::Binary>(arithmetic_expr.m_right);
	REQUIRE(multiplied_expr.m_op.m_token_name == Token::Name::STAR);
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(multiplied_expr.m_left).m_token.m_lexeme == "2");
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(multiplied_expr.m_right).m_token.m_lexeme == "3");

	const MidoriStatement::VariableDefinition& tuple_definition = RequireVariableDefinition(program, 1u, "tuple_value");
	const MidoriExpression::Tuple& tuple_expr = RequireExpression<MidoriExpression::Tuple>(tuple_definition.m_value);
	REQUIRE(tuple_expr.m_elements.size() == 2u);

	const MidoriExpression::Binary& tuple_first_element = RequireExpression<MidoriExpression::Binary>(tuple_expr.m_elements[0u]);
	REQUIRE(tuple_first_element.m_op.m_token_name == Token::Name::SINGLE_PLUS);
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(tuple_first_element.m_left).m_token.m_lexeme == "1");
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(tuple_first_element.m_right).m_token.m_lexeme == "2");
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(tuple_expr.m_elements[1u]).m_token.m_lexeme == "3");
}

TEST_CASE("Parser exposes module exports and use imports extracted from the module preamble", "[parser]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile(
			"Helper.mdr",
			R"(module Helper
public export { increment, decrement }
def increment = 1;
def decrement = 0;
)"),
		MidoriTest::TempProjectFile(
			"Main.mdr",
			R"(module Main
public export { local_value }
import { "Helper.mdr" }
use Helper.{increment, decrement}
def local_value = 1;
)")
	});

	const std::filesystem::path main_path = project.Path("Main.mdr");
	const std::string main_source_code =
		R"(module Main
public export { local_value }
import { "Helper.mdr" }
use Helper.{increment, decrement}
def local_value = 1;
)";

	std::expected<MidoriTest::ParsedSnippet, CompilerError> parse_result = MidoriTest::ParseSnippet(main_source_code, main_path.string());
	if (!parse_result.has_value())
	{
		FAIL(std::string(parse_result.error().Rendered()));
	}

	REQUIRE(parse_result->m_module_declaration.has_value());
	const ModuleDeclaration& module_declaration = parse_result->m_module_declaration.value();
	REQUIRE(module_declaration.ModuleName() == "Main");
	REQUIRE(module_declaration.HasExport("local_value"));
	REQUIRE(module_declaration.GetExportVisibility("local_value") == VisibilityLevel::Public);

	REQUIRE(parse_result->m_use_imports.size() == 2u);
	REQUIRE(parse_result->m_use_imports[0u].m_module_name == "Helper");
	REQUIRE(parse_result->m_use_imports[0u].m_symbol_name == "increment");
	REQUIRE(parse_result->m_use_imports[1u].m_module_name == "Helper");
	REQUIRE(parse_result->m_use_imports[1u].m_symbol_name == "decrement");

	const MidoriProgramTree& program = parse_result->m_program;
	REQUIRE(program.size() == 1u);
	static_cast<void>(RequireVariableDefinition(program, 0u, "local_value"));
}

TEST_CASE("Parser rejects qualified access to private exports outside the current namespace", "[parser][module]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile(
			"Secrets.mdr",
			R"(module Secrets
private export { hidden }
def hidden = 7;
)"),
		MidoriTest::TempProjectFile(
			"Main.mdr",
			R"(module Main
import { "./Secrets.mdr" }
def value = Secrets::hidden;
defun main(): Int => value;
)")
	});

	const std::filesystem::path main_path = project.Path("Main.mdr");
	const std::string main_source_code =
		R"(module Main
import { "./Secrets.mdr" }
def value = Secrets::hidden;
defun main(): Int => value;
)";

	MidoriResult::CompilerResult compile_result = MidoriTest::CompileSnippet(main_source_code, main_path.string());
	REQUIRE_FALSE(compile_result.has_value());

	const CompilerError& error = compile_result.error();
	CHECK(error.m_message.find("private to module 'Secrets'") != std::string::npos);
	CHECK(error.m_message.find("current namespace 'Main'") != std::string::npos);
}

TEST_CASE("Parser resolves dotted use imports against the full module name", "[parser][module]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"MathVector.mdr",
			R"(module Math.Vector
public export { add }
defun add(): Int => 41;
)"
		),
		MidoriTest::TempProjectFile
		(
			"Main.mdr",
			R"(module Main
import { "./MathVector.mdr" }
use Math.Vector.{add}
defun main(): Int => add();
)"
		)
	});

	const std::filesystem::path main_path = project.Path("Main.mdr");
	const std::string main_source_code =
		R"(module Main
import { "./MathVector.mdr" }
use Math.Vector.{add}
defun main(): Int => add();
)";

	MidoriResult::CompilerResult compile_result = MidoriTest::CompileSnippet(main_source_code, main_path.string());
	REQUIRE(compile_result.has_value());
}

TEST_CASE("Parser rejects ambiguous use imports from different modules", "[parser][module]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"Left.mdr",
			R"(module Left
public export { value }
defun value(): Int => 1;
)"
		),
		MidoriTest::TempProjectFile
		(
			"Right.mdr",
			R"(module Right
public export { value }
defun value(): Int => 2;
)"
		),
		MidoriTest::TempProjectFile
		(
			"Main.mdr",
			R"(module Main
import { "./Left.mdr", "./Right.mdr" }
use Left.{value}
use Right.{value}
defun main(): Int => value();
)"
		)
	});

	const std::filesystem::path main_path = project.Path("Main.mdr");
	const std::string main_source_code =
		R"(module Main
import { "./Left.mdr", "./Right.mdr" }
use Left.{value}
use Right.{value}
defun main(): Int => value();
)";

	MidoriResult::CompilerResult compile_result = MidoriTest::CompileSnippet(main_source_code, main_path.string());
	REQUIRE_FALSE(compile_result.has_value());

	const CompilerError& error = compile_result.error();
	CHECK(error.m_message.find("Ambiguous use import for symbol 'value'") != std::string::npos);
	CHECK(error.m_message.find("Left::value") != std::string::npos);
}

TEST_CASE("Parser treats duplicate same-module use imports as idempotent", "[parser][module]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"Helper.mdr",
			R"(module Helper
public export { value }
defun value(): Int => 7;
)"
		),
		MidoriTest::TempProjectFile
		(
			"Main.mdr",
			R"(module Main
import { "./Helper.mdr" }
use Helper.{value}
use Helper.{value}
defun main(): Int => value();
)"
		)
	});

	const std::filesystem::path main_path = project.Path("Main.mdr");
	const std::string main_source_code =
		R"(module Main
import { "./Helper.mdr" }
use Helper.{value}
use Helper.{value}
defun main(): Int => value();
)";

	MidoriResult::CompilerResult compile_result = MidoriTest::CompileSnippet(main_source_code, main_path.string());
	REQUIRE(compile_result.has_value());
}

TEST_CASE("Compiler uses the declared entry module name for linked executable metadata", "[parser][module]")
{
	const std::string source_code =
		R"(module App.Main
defun main(): Int => 0;
)";

	MidoriResult::CompilerResult compile_result = MidoriTest::CompileSnippet(source_code, "EntryPoint.mdr");
	REQUIRE(compile_result.has_value());

	const MidoriExecutable& executable = compile_result.value();
	CHECK(executable.GetFileName() == "App.Main");
	REQUIRE_FALSE(executable.m_procedure_names.empty());
	CHECK(std::string(executable.m_procedure_names[0u].GetCString()) == std::format("{}@{}", MODULE_BOOTSTRAP_PREFIX, "App.Main"));
}

TEST_CASE("Parser builds constructor and wildcard match patterns without brittle tree snapshots", "[parser]")
{
	const std::string source_code =
		R"(module ParserMatch
union Option = None | Some(Int);
def result = match new Option::Some(7) with
	case Option::Some(_) => 1
	case Option::None() => 0
;
)";

	std::expected<MidoriTest::ParsedSnippet, CompilerError> parse_result = MidoriTest::ParseSnippet(source_code, "ParserMatch.mdr");
	if (!parse_result.has_value())
	{
		FAIL(std::string(parse_result.error().Rendered()));
	}

	const MidoriProgramTree& program = parse_result->m_program;
	REQUIRE(program.size() == 2u);

	const MidoriStatement::VariableDefinition& result_definition = RequireVariableDefinition(program, 1u, "result");
	const MidoriExpression::Match& match_expr = RequireExpression<MidoriExpression::Match>(result_definition.m_value);
	REQUIRE(match_expr.m_cases.size() == 2u);

	const MidoriExpression::Case& some_case = RequireExpression<MidoriExpression::Case>(match_expr.m_cases[0u]);
	const MidoriPattern::Constructor& some_pattern = RequirePattern<MidoriPattern::Constructor>(some_case.m_pattern);
	REQUIRE(some_pattern.m_args.size() == 1u);
	static_cast<void>(RequirePattern<MidoriPattern::Wildcard>(some_pattern.m_args[0u]));
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(some_case.m_expr).m_token.m_lexeme == "1");

	const MidoriExpression::Case& none_case = RequireExpression<MidoriExpression::Case>(match_expr.m_cases[1u]);
	const MidoriPattern::Constructor& none_pattern = RequirePattern<MidoriPattern::Constructor>(none_case.m_pattern);
	REQUIRE(none_pattern.m_args.empty());
	REQUIRE(RequireExpression<MidoriExpression::IntegerLiteral>(none_case.m_expr).m_token.m_lexeme == "0");
}
