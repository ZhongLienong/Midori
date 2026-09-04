#include <catch2/catch_test_macros.hpp>

#include "Compiler/AbstractSyntaxTree/Type.h"

#include <memory>
#include <string>
#include <vector>

TEST_CASE("A newtype renders as its own name, never its representation", "[type]")
{
	// ToString is what InstanceKey and MangleInstanceMethodName are keyed on, so
	// this single behaviour is what gives a newtype independent typeclass
	// instances. If it ever renders as "Int", Hashable<Meters> collapses into
	// Hashable<Int> and the feature is gone.
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeLiteralType<MidoriType::IntegerType>();
	const std::shared_ptr<MidoriType> meters = MidoriType::MakeNewType("Meters", representation, {});

	REQUIRE(meters->ToString() == "Meters");
	REQUIRE(meters->ToString() != representation->ToString());
}

TEST_CASE("A newtype is unequal to its representation in both directions", "[type]")
{
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeLiteralType<MidoriType::IntegerType>();
	const std::shared_ptr<MidoriType> meters = MidoriType::MakeNewType("Meters", representation, {});

	REQUIRE_FALSE(*meters == *representation);
	REQUIRE_FALSE(*representation == *meters);
}

TEST_CASE("Two newtypes over one representation are distinct from each other", "[type]")
{
	// Without this, `Meters` and `Seconds` would silently interconvert, which is
	// the exact confusion the feature exists to prevent.
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeLiteralType<MidoriType::IntegerType>();
	const std::shared_ptr<MidoriType> meters = MidoriType::MakeNewType("Meters", representation, {});
	const std::shared_ptr<MidoriType> seconds = MidoriType::MakeNewType("Seconds", representation, {});

	REQUIRE_FALSE(*meters == *seconds);
}

TEST_CASE("A newtype equals another newtype with the same name", "[type]")
{
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeLiteralType<MidoriType::IntegerType>();
	const std::shared_ptr<MidoriType> left = MidoriType::MakeNewType("Meters", representation, {});
	const std::shared_ptr<MidoriType> right = MidoriType::MakeNewType("Meters", representation, {});

	REQUIRE(*left == *right);
}

TEST_CASE("A self-referential newtype's ToString recursion guard preserves nominal identity", "[type]")
{
	// FINDING 1 regression test. MidoriType::ToString()'s outer recursion guard
	// special-cases StructType and UnionType to return their own name on cycle
	// re-entry, and previously fell through to the literal string "Recursive"
	// for every other alternative, NewType included. Since InstanceKey and
	// MangleInstanceMethodName are both keyed on ToString, two distinct
	// self-referential newtypes both rendering "Recursive" would collapse into
	// one shared instance slot -- exactly the nominality collision this feature
	// exists to prevent.
	//
	// A cycle planted in m_representation cannot reach this guard: the NewType
	// arm of ToStringVisitor never stringifies m_representation at all (that is
	// the point of the type -- see the "never renders its representation" case
	// above), so a self-reference there is simply never visited and the guard
	// is never exercised. The reachable cycle is through m_type_arguments on a
	// generic instantiation, which StringifyTypeArguments does recurse into, so
	// that is what this test constructs -- directly through the public API, no
	// undefined behaviour.
	const std::shared_ptr<MidoriType> meters = MidoriType::MakeNewType("Meters", MidoriType::MakeLiteralType<MidoriType::IntegerType>(), {});
	MidoriType::NewType& meters_data = meters->GetType<MidoriType::NewType>();
	meters_data.m_is_generic_instantiation = true;
	meters_data.m_type_arguments = { meters };

	const std::shared_ptr<MidoriType> seconds = MidoriType::MakeNewType("Seconds", MidoriType::MakeLiteralType<MidoriType::IntegerType>(), {});
	MidoriType::NewType& seconds_data = seconds->GetType<MidoriType::NewType>();
	seconds_data.m_is_generic_instantiation = true;
	seconds_data.m_type_arguments = { seconds };

	REQUIRE(meters->ToString() == "Meters<Meters>");
	REQUIRE(seconds->ToString() == "Seconds<Seconds>");
	REQUIRE(meters->ToString() != seconds->ToString());
}

TEST_CASE("Substituting a newtype's parameter rewrites its representation", "[type]")
{
	const std::shared_ptr<MidoriType> element = MidoriType::MakeGenericType("T");
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeArrayType(element);
	const std::shared_ptr<MidoriType> boxed = MidoriType::MakeNewType("Boxed", representation, {"T"});

	std::unordered_map<std::string, std::shared_ptr<MidoriType>> substitutions;
	substitutions["T"] = MidoriType::MakeLiteralType<MidoriType::IntegerType>();

	const std::shared_ptr<MidoriType> instantiated = MidoriType::SubstituteTypeParams(boxed, substitutions);

	REQUIRE(instantiated->IsType<MidoriType::NewType>());

	const MidoriType::NewType& result = instantiated->GetType<MidoriType::NewType>();
	REQUIRE(result.m_name == "Boxed");
	REQUIRE(result.m_representation->IsType<MidoriType::ArrayType>());
	REQUIRE(result.m_representation->GetType<MidoriType::ArrayType>().m_element_type->IsType<MidoriType::IntegerType>());
}

TEST_CASE("Substituting a newtype with an empty map preserves its parameters", "[type]")
{
	// Trap 1: since e730762, SubstituteTypeParams with an empty map is not the
	// identity for StructType - it rebuilds with m_generic_params cleared. The
	// NewType arm must not inherit that behaviour.
	const std::shared_ptr<MidoriType> element = MidoriType::MakeGenericType("T");
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeArrayType(element);
	const std::shared_ptr<MidoriType> boxed = MidoriType::MakeNewType("Boxed", representation, {"T"});

	const std::unordered_map<std::string, std::shared_ptr<MidoriType>> empty;
	const std::shared_ptr<MidoriType> result = MidoriType::SubstituteTypeParams(boxed, empty);

	REQUIRE(result->IsType<MidoriType::NewType>());
	REQUIRE(result->GetType<MidoriType::NewType>().m_generic_params == std::vector<std::string>{"T"});
}
