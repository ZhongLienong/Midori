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

TEST_CASE("Collection updates byte accounting for reclaimed objects", "[gc]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	REQUIRE(gc.TotalBytesAllocated() == 0uz);

	size_t expected_bytes = 0uz;
	MidoriTraceable* first = AllocateText(allocator, gc, "first");
	expected_bytes += first->GetSize();
	REQUIRE(gc.TotalBytesAllocated() == expected_bytes);

	MidoriTraceable* second = AllocateText(allocator, gc, "second");
	expected_bytes += second->GetSize();
	MidoriTraceable* third = AllocateText(allocator, gc, "third");
	expected_bytes += third->GetSize();
	REQUIRE(gc.TotalBytesAllocated() == expected_bytes);

	// A handful of texts is nowhere near the initial threshold.
	REQUIRE_FALSE(gc.ShouldCollect());

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.ReclaimMemory(no_roots, allocator, true);

	REQUIRE(allocator.LiveSlotCount() == 0uz);
	REQUIRE(gc.TotalBytesAllocated() == 0uz);
}

TEST_CASE("Collection sweeps garbage across multiple bitmap words", "[gc]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	constexpr size_t OBJECT_COUNT = 100uz;
	GarbageCollector::GarbageCollectionRoots roots;
	for (size_t idx = 0uz; idx < OBJECT_COUNT; idx += 1uz)
	{
		MidoriTraceable* traceable = AllocateText(allocator, gc, "multi-word");
		if (idx % 10uz == 0uz)
		{
			roots.emplace_back(traceable);
		}
	}

	gc.ReclaimMemory(roots, allocator, true);

	for (MidoriTraceable* rooted : roots)
	{
		REQUIRE(allocator.Contains(rooted));
	}
	REQUIRE(allocator.LiveSlotCount() == roots.size());

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.ReclaimMemory(no_roots, allocator, true);
	REQUIRE(allocator.LiveSlotCount() == 0uz);
}
