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
