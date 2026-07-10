#include <catch2/catch_test_macros.hpp>

#include "Common/Value/Value.h"
#include "Interpreter/Allocator/MidoriAllocator.h"
#include "Interpreter/GarbageCollector/GarbageCollector.h"

namespace
{
	MidoriTraceable* AllocateText(MidoriAllocator& allocator, GarbageCollector& gc, const char* content)
	{
		void* memory = allocator.Allocate(sizeof(MidoriTraceable));
		MidoriTraceable* traceable = new(memory) MidoriTraceable(MidoriText(content));
		gc.RegisterObject(traceable);
		return traceable;
	}

	MidoriTraceable* AllocateArrayOf(MidoriAllocator& allocator, GarbageCollector& gc, MidoriTraceable* element)
	{
		MidoriArray array;
		array.AddBack(MidoriValue(element));
		void* memory = allocator.Allocate(sizeof(MidoriTraceable));
		MidoriTraceable* traceable = new(memory) MidoriTraceable(std::move(array));
		gc.RegisterObject(traceable);
		return traceable;
	}
}

TEST_CASE("Collection keeps rooted objects and reclaims garbage", "[gc]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	MidoriTraceable* rooted = AllocateText(allocator, gc, "rooted");
	MidoriTraceable* garbage = AllocateText(allocator, gc, "garbage");

	GarbageCollector::GarbageCollectionRoots roots{ rooted };
	gc.ReclaimMemory(roots, allocator, true);

	REQUIRE(allocator.Contains(rooted));
	REQUIRE_FALSE(allocator.Contains(garbage));

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.ReclaimMemory(no_roots, allocator, true);
	REQUIRE_FALSE(allocator.Contains(rooted));
}

TEST_CASE("Collection traces through containers", "[gc]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	MidoriTraceable* leaf = AllocateText(allocator, gc, "leaf");
	MidoriTraceable* holder = AllocateArrayOf(allocator, gc, leaf);

	GarbageCollector::GarbageCollectionRoots roots{ holder };
	gc.ReclaimMemory(roots, allocator, true);

	REQUIRE(allocator.Contains(holder));
	REQUIRE(allocator.Contains(leaf));

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.ReclaimMemory(no_roots, allocator, true);
	REQUIRE_FALSE(allocator.Contains(holder));
	REQUIRE_FALSE(allocator.Contains(leaf));
}

TEST_CASE("Collection survives repeated cycles with the same roots", "[gc]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	MidoriTraceable* rooted = AllocateText(allocator, gc, "stable");
	GarbageCollector::GarbageCollectionRoots roots{ rooted };

	for (int cycle = 0; cycle < 5; cycle += 1)
	{
		AllocateText(allocator, gc, "transient");
		gc.ReclaimMemory(roots, allocator, true);
		REQUIRE(allocator.Contains(rooted));
	}

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.ReclaimMemory(no_roots, allocator, true);
}
