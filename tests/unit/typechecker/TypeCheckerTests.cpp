#include <catch2/catch_test_macros.hpp>

#include "support/CompileHelpers.h"
#include "support/DiagnosticMatchers.h"

#include <expected>
#include <string>
#include <string_view>

namespace
{
	const MidoriStatement::VariableDefinition* FindVariableDefinition(const MidoriProgramTree& program, std::string_view name)
	{
		for (const std::unique_ptr<MidoriStatement>& statement : program)
		{
			if (statement == nullptr || !statement->IsStatement<MidoriStatement::VariableDefinition>())
			{
				continue;
			}

			const MidoriStatement::VariableDefinition& definition = statement->GetStatement<MidoriStatement::VariableDefinition>();
			if (definition.m_name.m_lexeme == name)
			{
				return std::addressof(definition);
			}
		}

		return nullptr;
	}
}

TEST_CASE("TypeChecker infers lambda parameter and return types from an annotated binding", "[typechecker]")
{
	const std::string source_code =
		R"(module TypeInference
def doubler : fn(Int) -> Int = fn(x) => { x * 2 };
def result = doubler(21);
)";

	std::expected<MidoriTest::TypedSnippet, CompilerError> typecheck_result = MidoriTest::TypeCheckSnippet(source_code, "TypeInference.mdr");
	if (!typecheck_result.has_value())
	{
		FAIL(std::string(typecheck_result.error().Rendered()));
	}

	const MidoriStatement::VariableDefinition* doubler_definition = FindVariableDefinition(typecheck_result->m_program, "doubler");
	REQUIRE(doubler_definition != nullptr);
	REQUIRE(doubler_definition->m_value->IsExpression<MidoriExpression::Function>());

	const MidoriExpression::Function& lambda = doubler_definition->m_value->GetExpression<MidoriExpression::Function>();
	REQUIRE(lambda.m_param_types.size() == 1u);
	REQUIRE(lambda.m_param_types[0u]->IsType<MidoriType::IntegerType>());
	REQUIRE(lambda.m_return_type->IsType<MidoriType::IntegerType>());
	REQUIRE(doubler_definition->m_value->GetType()->IsType<MidoriType::FunctionType>());
	REQUIRE(doubler_definition->m_value->GetType()->ToString() == "fn(Int) -> Int");

	const MidoriStatement::VariableDefinition* result_definition = FindVariableDefinition(typecheck_result->m_program, "result");
	REQUIRE(result_definition != nullptr);
	REQUIRE(result_definition->m_value->GetType()->IsType<MidoriType::IntegerType>());
}

TEST_CASE("TypeChecker resolves generic type aliases to their instantiated underlying types", "[typechecker]")
{
	const std::string source_code =
		R"(module AliasTypes
struct Pair<A, B>
{
	first: A,
	second: B
};
type IntPair = Pair<Int, Int>;
def pair : IntPair = new Pair(1, 2);
)";

	std::expected<MidoriTest::TypedSnippet, CompilerError> typecheck_result = MidoriTest::TypeCheckSnippet(source_code, "AliasTypes.mdr");
	if (!typecheck_result.has_value())
	{
		FAIL(std::string(typecheck_result.error().Rendered()));
	}

	const MidoriStatement::VariableDefinition* pair_definition = FindVariableDefinition(typecheck_result->m_program, "pair");
	REQUIRE(pair_definition != nullptr);
	REQUIRE(pair_definition->m_annotated_type.has_value());
	REQUIRE(pair_definition->m_annotated_type.value()->IsType<MidoriType::StructType>());

	const MidoriType::StructType& pair_type = pair_definition->m_annotated_type.value()->GetType<MidoriType::StructType>();
	REQUIRE(pair_type.m_name == "Pair");
	REQUIRE(pair_type.m_member_types.size() == 2u);
	REQUIRE(pair_type.m_member_types[0u]->IsType<MidoriType::IntegerType>());
	REQUIRE(pair_type.m_member_types[1u]->IsType<MidoriType::IntegerType>());
	REQUIRE(pair_definition->m_value->GetType()->IsType<MidoriType::StructType>());

	const MidoriType::StructType& constructed_pair_type = pair_definition->m_value->GetType()->GetType<MidoriType::StructType>();
	REQUIRE(constructed_pair_type.m_name == "Pair");
	REQUIRE(constructed_pair_type.m_member_types.size() == 2u);
	REQUIRE(constructed_pair_type.m_member_types[0u]->IsType<MidoriType::IntegerType>());
	REQUIRE(constructed_pair_type.m_member_types[1u]->IsType<MidoriType::IntegerType>());
}

TEST_CASE("TypeChecker reports deterministic class-constraint failures at the call site", "[typechecker]")
{
	const std::string source_code =
		R"(module ConstraintFailure
class Show<T>
{
	show: fn(value: T) -> Text;
};
struct Hidden
{
	value: Int
};
defun Display<T>(value: T) : Text where Show<T> => {
	return Show::show(value);
};
def rendered = Display(new Hidden(1));
)";

	std::expected<MidoriTest::TypedSnippet, CompilerError> typecheck_result = MidoriTest::TypeCheckSnippet(source_code, "ConstraintFailure.mdr");
	REQUIRE_FALSE(typecheck_result.has_value());

	std::string mismatch;
	MidoriTest::ErrorExpectation expectation;
	expectation.m_message_substrings = { "Type Hidden does not satisfy constraint Show<Hidden>", "no matching instance found" };
	expectation.m_rendered_substrings = { "Type Checker Error", "ConstraintFailure.mdr:13", "Display(new Hidden(1))" };

	const bool matched = MidoriTest::Matches(typecheck_result.error(), expectation, &mismatch);
	CAPTURE(mismatch);
	REQUIRE(matched);
}
