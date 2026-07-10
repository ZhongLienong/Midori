# Midori GC Overhaul Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the vector-based stop-the-world mark-sweep GC with an allocator-integrated bitmap mark-sweep (Phase 1), then add sticky-mark-bit generational collection with write barriers (Phase 2), per the approved spec at `docs/superpowers/specs/2026-07-10-gc-overhaul-design.md`.

**Architecture:** All GC-managed objects are fixed 80-byte slots in `MidoriAllocator`. The GC gains a mark bitmap parallel to the allocator's live bitmap; sweeping becomes a word-level `live & ~mark` walk. Phase 2 keeps mark bits sticky across minor collections (marked = old generation), traces minors from roots + a remembered set fed by write barriers at the interpreter's container-mutation opcodes, and runs full (major) collections on a policy trigger.

**Tech Stack:** C++23, CMake + Ninja presets (`x64-development` for tests, `x64-release` for benchmarks), Catch2 unit tests via ctest, Python runners `scripts/run_tests.py` and `scripts/bench.py`.

**Repo conventions (from AGENTS.md):** no `auto` unless obvious; `m_`/`s_` prefixes; PascalCase functions; always braces; implementations in `.cpp` where practical; no comments for self-explanatory code.

**All paths below are relative to** `C:\Users\jk381\source\repos\ZhongLienong\Midori`.

---

## Task 0: Baseline — build current Release and snapshot it for benchmark comparison

**Files:** none modified.

- [ ] **Step 0.1: Configure and build Release**

```powershell
cmake --preset x64-release
cmake --build out/build/ninja/x64-release
```

Expected: build succeeds, `out/build/ninja/x64-release/out/Midori.exe` exists.

- [ ] **Step 0.2: Snapshot the baseline executable**

```powershell
New-Item -ItemType Directory -Force out/gc-baseline
Copy-Item out/build/ninja/x64-release/out/Midori.exe out/gc-baseline/Midori-baseline.exe
```

- [ ] **Step 0.3: Confirm the development build and test suites are green before touching anything**

```powershell
cmake --preset x64-development
cmake --build out/build/ninja/x64-development
python scripts/run_tests.py --build Development
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

Expected: all `.mdr` tests and all unit tests PASS. If anything fails, STOP and report — do not build on a red baseline.

---

## Task 1: Allocator slot-index API (native path)

Slot indices are **global bit indices** compatible with the existing live-bit layout: each block contributes `LIVE_WORDS_PER_BLOCK * 64` bit positions (`BITS_PER_BLOCK`), of which only the first `SLOTS_PER_BLOCK` are real slots; the padding bits are never set, so word-level sweeps skip them for free.

**Files:**
- Modify: `src/Interpreter/Allocator/MidoriAllocator.h`
- Modify: `src/Interpreter/Allocator/MidoriAllocator.cpp`
- Create: `tests/unit/runtime/AllocatorSlotIndexTests.cpp`

- [ ] **Step 1.1: Write the failing tests**

Create `tests/unit/runtime/AllocatorSlotIndexTests.cpp`:

```cpp
#include <catch2/catch_test_macros.hpp>

#include "Interpreter/Allocator/MidoriAllocator.h"

#include <cstdint>
#include <optional>

TEST_CASE("Slot index round-trips through SlotAt", "[allocator][gc]")
{
	MidoriAllocator allocator;

	void* first = allocator.Allocate(MidoriAllocator::SLOT_SIZE);
	void* second = allocator.Allocate(MidoriAllocator::SLOT_SIZE);
	REQUIRE(first != nullptr);
	REQUIRE(second != nullptr);

	std::optional<size_t> first_index = allocator.TryGetSlotIndex(first);
	std::optional<size_t> second_index = allocator.TryGetSlotIndex(second);
	REQUIRE(first_index.has_value());
	REQUIRE(second_index.has_value());
	REQUIRE(*first_index != *second_index);
	REQUIRE(allocator.SlotAt(*first_index) == first);
	REQUIRE(allocator.SlotAt(*second_index) == second);

	allocator.Free(first, MidoriAllocator::SLOT_SIZE);
	allocator.Free(second, MidoriAllocator::SLOT_SIZE);
}

TEST_CASE("Slot index rejects foreign and misaligned pointers", "[allocator][gc]")
{
	MidoriAllocator allocator;

	void* slot = allocator.Allocate(MidoriAllocator::SLOT_SIZE);
	REQUIRE(slot != nullptr);

	int stack_object = 0;
	REQUIRE_FALSE(allocator.TryGetSlotIndex(&stack_object).has_value());
	REQUIRE_FALSE(allocator.TryGetSlotIndex(nullptr).has_value());

	uint8_t* misaligned = static_cast<uint8_t*>(slot) + 1;
	REQUIRE_FALSE(allocator.TryGetSlotIndex(misaligned).has_value());

	allocator.Free(slot, MidoriAllocator::SLOT_SIZE);
}

TEST_CASE("Live bit words reflect allocation state", "[allocator][gc]")
{
	MidoriAllocator allocator;

	void* slot = allocator.Allocate(MidoriAllocator::SLOT_SIZE);
	std::optional<size_t> index = allocator.TryGetSlotIndex(slot);
	REQUIRE(index.has_value());

	const uint64_t* words = allocator.LiveBitWords();
	REQUIRE(allocator.SlotWordCount() > *index / 64uz);
	REQUIRE((words[*index / 64uz] & (1ull << (*index % 64uz))) != 0ull);

	allocator.Free(slot, MidoriAllocator::SLOT_SIZE);
	REQUIRE((words[*index / 64uz] & (1ull << (*index % 64uz))) == 0ull);
}
```

- [ ] **Step 1.2: Run the tests to verify they fail to compile**

```powershell
cmake --build out/build/ninja/x64-development --target MidoriUnitTests
```

Expected: compile FAILURE — `TryGetSlotIndex`, `SlotAt`, `SlotWordCount`, `LiveBitWords` are not members of `MidoriAllocator`.

- [ ] **Step 1.3: Implement the API**

In `src/Interpreter/Allocator/MidoriAllocator.h`, add `#include <optional>` to the includes, then inside the class after `bool Contains(const void* ptr) const noexcept;` (public section, both platform variants must expose these — Task 2 covers the Emscripten implementation; for now guard the declarations so the native path compiles):

```cpp
	std::optional<size_t> TryGetSlotIndex(const void* ptr) const noexcept;

	void* SlotAt(size_t slot_index) const noexcept;

	size_t SlotWordCount() const noexcept;

	const uint64_t* LiveBitWords() const noexcept;

	size_t LiveSlotCount() const noexcept;
```

In the private native section add the constant next to `LIVE_WORDS_PER_BLOCK`:

```cpp
	static constexpr size_t BITS_PER_BLOCK = LIVE_WORDS_PER_BLOCK * 64uz;
```

In `src/Interpreter/Allocator/MidoriAllocator.cpp` (inside the `#else` / non-Emscripten section), add:

```cpp
std::optional<size_t> MidoriAllocator::TryGetSlotIndex(const void* ptr) const noexcept
{
	const size_t offset = static_cast<size_t>(reinterpret_cast<uintptr_t>(ptr) - reinterpret_cast<uintptr_t>(m_region_base));
	if (offset >= m_committed_bytes)
	{
		return std::nullopt;
	}

	const size_t block_offset = offset % BLOCK_SIZE;
	if (block_offset % SLOT_SIZE != 0uz || block_offset >= USABLE_BLOCK_BYTES)
	{
		return std::nullopt;
	}

	return (offset / BLOCK_SIZE) * BITS_PER_BLOCK + block_offset / SLOT_SIZE;
}

void* MidoriAllocator::SlotAt(size_t slot_index) const noexcept
{
	const size_t block_index = slot_index / BITS_PER_BLOCK;
	const size_t slot_in_block = slot_index % BITS_PER_BLOCK;
	return m_region_base + block_index * BLOCK_SIZE + slot_in_block * SLOT_SIZE;
}

size_t MidoriAllocator::SlotWordCount() const noexcept
{
	return m_live_bits.size();
}

const uint64_t* MidoriAllocator::LiveBitWords() const noexcept
{
	return m_live_bits.data();
}

size_t MidoriAllocator::LiveSlotCount() const noexcept
{
	size_t count = 0uz;
	for (uint64_t word : m_live_bits)
	{
		count += static_cast<size_t>(std::popcount(word));
	}
	return count;
}
```

Add `#include <bit>` and `#include <optional>` to the cpp includes.

Refactor `Contains` and `SetLiveBit` to reuse the same index math (both currently duplicate it). `Contains` becomes:

```cpp
bool MidoriAllocator::Contains(const void* ptr) const noexcept
{
	const std::optional<size_t> slot_index = TryGetSlotIndex(ptr);
	if (slot_index.has_value())
	{
		return (m_live_bits[*slot_index / 64uz] & (1ull << (*slot_index % 64uz))) != 0ull;
	}
	return ContainsLargeAllocation(ptr);
}
```

and `SetLiveBit`:

```cpp
bool MidoriAllocator::SetLiveBit(void* ptr, bool is_live) noexcept
{
	const std::optional<size_t> slot_index = TryGetSlotIndex(ptr);
	if (!slot_index.has_value())
	{
		return false;
	}

	const uint64_t mask = 1ull << (*slot_index % 64uz);
	if (is_live)
	{
		m_live_bits[*slot_index / 64uz] |= mask;
	}
	else
	{
		m_live_bits[*slot_index / 64uz] &= ~mask;
	}
	return true;
}
```

NOTE: the old code computed `word_index = (offset / BLOCK_SIZE) * LIVE_WORDS_PER_BLOCK + slot_index / 64` — the new global-bit-index scheme (`(offset / BLOCK_SIZE) * BITS_PER_BLOCK + slot`) produces the exact same word/bit positions because `BITS_PER_BLOCK = LIVE_WORDS_PER_BLOCK * 64`. No live-bit layout change.

- [ ] **Step 1.4: Build and run the new tests**

```powershell
cmake --build out/build/ninja/x64-development --target MidoriUnitTests
ctest --test-dir out/build/ninja/x64-development --output-on-failure -R "Slot|Live bit"
```

Expected: 3 tests PASS.

- [ ] **Step 1.5: Run the full unit suite and commit**

```powershell
ctest --test-dir out/build/ninja/x64-development --output-on-failure
git add src/Interpreter/Allocator tests/unit/runtime/AllocatorSlotIndexTests.cpp
git commit -m "feat(gc): add slot-index API to MidoriAllocator"
```

---

## Task 2: Emscripten allocator on the same block/bitmap model

The `__EMSCRIPTEN__` variant currently uses `std::unordered_set<void*>` and cannot support the bitmap GC. Rebuild it on malloc'd 64 KB blocks with the identical live-bit layout; pointer→slot lookup binary-searches a sorted block table.

**Files:**
- Modify: `src/Interpreter/Allocator/MidoriAllocator.h`
- Modify: `src/Interpreter/Allocator/MidoriAllocator.cpp`

- [ ] **Step 2.1: Replace the Emscripten private section in the header**

Replace the `#else` (Emscripten) private section (`std::unordered_set<void*> m_allocated;`) with:

```cpp
	struct FreeNode
	{
		FreeNode* m_next;
	};

	static constexpr size_t USABLE_BLOCK_BYTES = SLOTS_PER_BLOCK * SLOT_SIZE;
	static constexpr size_t LIVE_WORDS_PER_BLOCK = (SLOTS_PER_BLOCK + 63uz) / 64uz;
	static constexpr size_t BITS_PER_BLOCK = LIVE_WORDS_PER_BLOCK * 64uz;

	std::vector<uint8_t*> m_blocks;
	std::vector<uint64_t> m_live_bits;
	FreeNode* m_free_list = nullptr;
	std::vector<void*> m_large_allocs;

	void* AllocateSmall();
	void* AllocateLarge(size_t size);
	bool AllocateBlock();
	bool EnsureFreeList();
	FreeNode* PopFreeNode() noexcept;
	FreeNode* PushFreeNode(FreeNode* node) noexcept;
	bool SetLiveBit(void* ptr, bool is_live) noexcept;
	bool TrackLargeAllocation(void* ptr);
	bool UntrackLargeAllocation(void* ptr) noexcept;
	bool ContainsLargeAllocation(const void* ptr) const noexcept;
	std::optional<size_t> FindBlockIndex(const void* ptr) const noexcept;
```

The public slot-index API declarations from Task 1 move out of any platform guard — they are unconditional.

Remove `#include <unordered_set>` from the header.

- [ ] **Step 2.2: Implement the Emscripten variant in the cpp**

Replace the whole `#ifdef __EMSCRIPTEN__` implementation block with:

```cpp
#ifdef __EMSCRIPTEN__

#include <algorithm>
#include <bit>

MidoriAllocator::MidoriAllocator()
{
	AllocateBlock();
}

MidoriAllocator::~MidoriAllocator()
{
	for (uint8_t* block : m_blocks)
	{
		std::free(block);
	}
	m_blocks.clear();
	m_live_bits.clear();
	m_large_allocs.clear();
	m_free_list = nullptr;
}

void* MidoriAllocator::Allocate(size_t size)
{
	if (size == 0uz)
	{
		return nullptr;
	}

	if (size > SLOT_SIZE)
	{
		return AllocateLarge(size);
	}

	return AllocateSmall();
}

void* MidoriAllocator::AllocateSmall()
{
	if (!EnsureFreeList())
	{
		return nullptr;
	}

	FreeNode* node = PopFreeNode();
	if (node == nullptr)
	{
		return nullptr;
	}

	if (!SetLiveBit(node, true))
	{
		PushFreeNode(node);
		return nullptr;
	}
	return static_cast<void*>(node);
}

void* MidoriAllocator::AllocateLarge(size_t size)
{
	void* ptr = std::malloc(size);
	if (ptr == nullptr)
	{
		return nullptr;
	}

	if (!TrackLargeAllocation(ptr))
	{
		std::free(ptr);
		return nullptr;
	}
	return ptr;
}

MidoriAllocator& MidoriAllocator::Free(void* ptr, size_t size) &
{
	if (ptr == nullptr || size == 0uz)
	{
		return *this;
	}

	if (size <= SLOT_SIZE)
	{
		if (SetLiveBit(ptr, false))
		{
			PushFreeNode(static_cast<FreeNode*>(ptr));
		}
		return *this;
	}

	UntrackLargeAllocation(ptr);
	std::free(ptr);
	return *this;
}

MidoriAllocator&& MidoriAllocator::Free(void* ptr, size_t size) &&
{
	static_cast<MidoriAllocator&>(*this).Free(ptr, size);
	return std::move(*this);
}

bool MidoriAllocator::AllocateBlock()
{
	uint8_t* block = static_cast<uint8_t*>(std::malloc(BLOCK_SIZE));
	if (block == nullptr)
	{
		return false;
	}

	m_blocks.push_back(block);
	m_live_bits.insert(m_live_bits.end(), LIVE_WORDS_PER_BLOCK, 0ull);

	uint8_t* slot_ptr = block;
	for (size_t i = 0uz; i < SLOTS_PER_BLOCK; i += 1uz)
	{
		PushFreeNode(reinterpret_cast<FreeNode*>(slot_ptr));
		slot_ptr += SLOT_SIZE;
	}

	return true;
}

std::optional<size_t> MidoriAllocator::FindBlockIndex(const void* ptr) const noexcept
{
	const uint8_t* address = static_cast<const uint8_t*>(ptr);
	for (size_t block_index = 0uz; block_index < m_blocks.size(); block_index += 1uz)
	{
		const uint8_t* base = m_blocks[block_index];
		if (address >= base && address < base + BLOCK_SIZE)
		{
			return block_index;
		}
	}
	return std::nullopt;
}

std::optional<size_t> MidoriAllocator::TryGetSlotIndex(const void* ptr) const noexcept
{
	const std::optional<size_t> block_index = FindBlockIndex(ptr);
	if (!block_index.has_value())
	{
		return std::nullopt;
	}

	const size_t block_offset = static_cast<size_t>(static_cast<const uint8_t*>(ptr) - m_blocks[*block_index]);
	if (block_offset % SLOT_SIZE != 0uz || block_offset >= USABLE_BLOCK_BYTES)
	{
		return std::nullopt;
	}

	return *block_index * BITS_PER_BLOCK + block_offset / SLOT_SIZE;
}

void* MidoriAllocator::SlotAt(size_t slot_index) const noexcept
{
	const size_t block_index = slot_index / BITS_PER_BLOCK;
	const size_t slot_in_block = slot_index % BITS_PER_BLOCK;
	return m_blocks[block_index] + slot_in_block * SLOT_SIZE;
}

size_t MidoriAllocator::SlotWordCount() const noexcept
{
	return m_live_bits.size();
}

const uint64_t* MidoriAllocator::LiveBitWords() const noexcept
{
	return m_live_bits.data();
}

size_t MidoriAllocator::LiveSlotCount() const noexcept
{
	size_t count = 0uz;
	for (uint64_t word : m_live_bits)
	{
		count += static_cast<size_t>(std::popcount(word));
	}
	return count;
}

bool MidoriAllocator::Contains(const void* ptr) const noexcept
{
	const std::optional<size_t> slot_index = TryGetSlotIndex(ptr);
	if (slot_index.has_value())
	{
		return (m_live_bits[*slot_index / 64uz] & (1ull << (*slot_index % 64uz))) != 0ull;
	}
	return ContainsLargeAllocation(ptr);
}

bool MidoriAllocator::SetLiveBit(void* ptr, bool is_live) noexcept
{
	const std::optional<size_t> slot_index = TryGetSlotIndex(ptr);
	if (!slot_index.has_value())
	{
		return false;
	}

	const uint64_t mask = 1ull << (*slot_index % 64uz);
	if (is_live)
	{
		m_live_bits[*slot_index / 64uz] |= mask;
	}
	else
	{
		m_live_bits[*slot_index / 64uz] &= ~mask;
	}
	return true;
}

bool MidoriAllocator::EnsureFreeList()
{
	if (m_free_list != nullptr)
	{
		return true;
	}

	return AllocateBlock();
}

MidoriAllocator::FreeNode* MidoriAllocator::PopFreeNode() noexcept
{
	if (m_free_list == nullptr)
	{
		return nullptr;
	}

	FreeNode* node = m_free_list;
	m_free_list = node->m_next;
	return node;
}

MidoriAllocator::FreeNode* MidoriAllocator::PushFreeNode(FreeNode* node) noexcept
{
	if (node == nullptr)
	{
		return m_free_list;
	}

	node->m_next = m_free_list;
	m_free_list = node;
	return node;
}

bool MidoriAllocator::TrackLargeAllocation(void* ptr)
{
	if (ptr == nullptr)
	{
		return false;
	}

	m_large_allocs.push_back(ptr);
	return true;
}

bool MidoriAllocator::UntrackLargeAllocation(void* ptr) noexcept
{
	if (ptr == nullptr || m_large_allocs.empty())
	{
		return false;
	}

	std::vector<void*>::iterator it = std::find(m_large_allocs.begin(), m_large_allocs.end(), ptr);
	if (it == m_large_allocs.end())
	{
		return false;
	}

	*it = m_large_allocs.back();
	m_large_allocs.pop_back();
	return true;
}

bool MidoriAllocator::ContainsLargeAllocation(const void* ptr) const noexcept
{
	if (ptr == nullptr || m_large_allocs.empty())
	{
		return false;
	}

	return std::find(m_large_allocs.begin(), m_large_allocs.end(), const_cast<void*>(ptr)) != m_large_allocs.end();
}

#endif
```

**DESIGN CONSTRAINT (do not "optimize" this):** blocks are **appended, never sorted**, and `FindBlockIndex` is a deliberate linear scan. A sorted block table with binary search would renumber the slot indices of later blocks whenever a new block sorts before them — and Phase 2 stores mark/logged bits keyed by slot index across allocations, so renumbering would corrupt generation state. Appending keeps every slot index stable for the allocator's lifetime. Block count stays small on wasm workloads; the scan is not a bottleneck. (This supersedes the spec's suggestion of a sorted-table binary search, for the correctness reason above.)

- [ ] **Step 2.3: Verify the native build still compiles and passes**

```powershell
cmake --build out/build/ninja/x64-development
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

Expected: PASS (the Emscripten code is `#ifdef`'d out natively; this verifies the header changes didn't break the native path).

- [ ] **Step 2.4: If the Emscripten SDK is available, smoke-build wasm; otherwise skip**

```powershell
python scripts/build_and_deploy_wasm.py --help
```

If the script and emsdk work, run its build step and confirm it compiles. If emsdk is not installed, note that in the commit message and move on — the code is structured identically to the tested native path.

- [ ] **Step 2.5: Commit**

```powershell
git add src/Interpreter/Allocator
git commit -m "feat(gc): unify Emscripten allocator on block/bitmap model"
```

---

## Task 3: Bitmap mark-sweep GarbageCollector (Phase 1 core)

**Files:**
- Modify: `src/Interpreter/GarbageCollector/GarbageCollector.h` (rewrite)
- Modify: `src/Interpreter/GarbageCollector/GarbageCollector.cpp` (rewrite)
- Modify: `src/Common/Value/Value.h` (remove `m_is_marked`, `Mark`, `Unmark`, `IsMarked`)
- Modify: `src/Common/Value/Value.cpp` (remove the three method definitions)
- Create: `tests/unit/runtime/GarbageCollectorTests.cpp`

- [ ] **Step 3.1: Audit remaining users of the old API**

```powershell
Select-String -Path src -Pattern "IsMarked|->Mark\(\)|Unmark|m_traceables|RegisterObject|GarbageCollectionRoots" -Recurse -SimpleMatch:$false | Select-Object Path, LineNumber, Line
```

Expected users: `GarbageCollector.*` (rewritten here), `VirtualMachine.h/.cpp` (root building + `RegisterObject` + `Contains` — signatures unchanged). If ANY other file uses `Mark/Unmark/IsMarked`, list it and update it in this task using the bitmap equivalents. Also check whether `GarbageCollector::Deallocator` (the `std::function` alias in the header) has any users; if none, delete the alias and the `#include <functional>`.

- [ ] **Step 3.2: Write the failing unit tests**

Create `tests/unit/runtime/GarbageCollectorTests.cpp`:

```cpp
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
```

- [ ] **Step 3.3: Build to confirm current state compiles the tests (they should pass against the OLD GC too — that is fine; they pin behavior)**

```powershell
cmake --build out/build/ninja/x64-development --target MidoriUnitTests
ctest --test-dir out/build/ninja/x64-development --output-on-failure -R "Collection"
```

Expected: PASS against the old implementation. These tests are the safety net for the rewrite.

- [ ] **Step 3.4: Rewrite `GarbageCollector.h`**

Replace the class body with:

```cpp
#pragma once

#include "Common/Value/Value.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Interpreter/Allocator/MidoriAllocator.h"

#include <cstddef>
#include <cstdint>
#include <vector>

class GarbageCollector
{
public:
	static constexpr inline size_t INITIAL_GC_THRESHOLD = 512000uz * 8uz;
	static constexpr inline size_t MIN_GC_THRESHOLD = 512000uz * 4uz;
	static constexpr inline size_t MAX_GC_THRESHOLD = 512000uz * 128uz;
	static constexpr inline double GC_GROWTH_FACTOR = 1.5;

	using GarbageCollectionRoots = std::vector<MidoriTraceable*>;

private:
	size_t m_total_bytes_allocated = 0uz;
	size_t m_gc_threshold = INITIAL_GC_THRESHOLD;
	std::vector<uint64_t> m_mark_bits;
	std::vector<MidoriTraceable*> m_mark_stack;
	const MidoriAllocator* m_allocator = nullptr;

public:
	GarbageCollector() = default;
	~GarbageCollector() = default;

	GarbageCollector(const GarbageCollector&) = delete;
	GarbageCollector& operator=(const GarbageCollector&) = delete;

	void ReclaimMemory(const GarbageCollectionRoots& roots, MidoriAllocator& allocator, bool force_clean = false);

	MIDORI_FORCE_INLINE bool ShouldCollect() const noexcept
	{
		return m_total_bytes_allocated >= m_gc_threshold;
	}

	void SetAllocator(const MidoriAllocator* allocator) noexcept { m_allocator = allocator; }

	MIDORI_FORCE_INLINE void RegisterObject(MidoriTraceable* traceable) noexcept
	{
		m_total_bytes_allocated += traceable->GetSize();
	}

#if MIDORI_DEBUG_INFO
	void PrintMemoryTelemetry();
#endif

	bool Contains(MidoriTraceable* ptr) const;

private:
	void Trace(const GarbageCollectionRoots& roots);
	void TryMark(MidoriTraceable* child_ptr);
	void Sweep(MidoriAllocator& allocator, size_t& sweep_count, size_t& bytes_reclaimed);
};
```

- [ ] **Step 3.5: Rewrite `GarbageCollector.cpp`**

Keep the anonymous-namespace `FormatTime` / `FormatBytes` helpers and the `#if MIDORI_DEBUG_INFO` timing scaffolding in `ReclaimMemory` exactly as they are today. Replace the rest:

```cpp
#include "GarbageCollector.h"
#include "Common/BuildConfig/BuildConfig.h"

#include <algorithm>
#include <bit>

// ... existing #if MIDORI_DEBUG_INFO helpers unchanged ...

bool GarbageCollector::Contains(MidoriTraceable* ptr) const
{
	return m_allocator != nullptr && m_allocator->Contains(ptr);
}

void GarbageCollector::TryMark(MidoriTraceable* child_ptr)
{
	if (child_ptr == nullptr || m_allocator == nullptr)
	{
		return;
	}

	const std::optional<size_t> slot_index = m_allocator->TryGetSlotIndex(child_ptr);
	if (!slot_index.has_value())
	{
		return;
	}

	const size_t word_index = *slot_index / 64uz;
	const uint64_t mask = 1ull << (*slot_index % 64uz);
	if ((m_allocator->LiveBitWords()[word_index] & mask) == 0ull)
	{
		return;
	}
	if ((m_mark_bits[word_index] & mask) != 0ull)
	{
		return;
	}

	m_mark_bits[word_index] |= mask;
	m_mark_stack.emplace_back(child_ptr);
}

void GarbageCollector::Trace(const GarbageCollectionRoots& roots)
{
	m_mark_stack.clear();

	auto mark_tuple_values = [this](MidoriTuple& tuple)
		{
			const int length = tuple.GetLength();
			for (int idx = 0; idx < length; idx += 1)
			{
				TryMark(tuple[idx].GetPointer());
			}
		};

	for (MidoriTraceable* root : roots)
	{
		TryMark(root);
	}

	while (!m_mark_stack.empty())
	{
		MidoriTraceable* current = m_mark_stack.back();
		m_mark_stack.pop_back();

		if (current->IsTraceable<MidoriArray>())
		{
			MidoriArray& arr = current->GetTraceable<MidoriArray>();
			int length = arr.GetLength();
			for (int idx = 0; idx < length; idx += 1)
			{
				TryMark(arr[idx].GetPointer());
			}
		}
		else if (current->IsTraceable<MidoriTuple>())
		{
			mark_tuple_values(current->GetTraceable<MidoriTuple>());
		}
		else if (current->IsTraceable<MidoriClosure>())
		{
			mark_tuple_values(current->GetTraceable<MidoriClosure>().m_cell_values);
		}
		else if (current->IsTraceable<MidoriCellValue>())
		{
			MidoriValue cell_value = current->GetTraceable<MidoriCellValue>().GetValue();
			TryMark(cell_value.GetPointer());
		}
		else if (current->IsTraceable<MidoriStruct>())
		{
			mark_tuple_values(current->GetTraceable<MidoriStruct>().m_values);
		}
		else if (current->IsTraceable<MidoriUnion>())
		{
			mark_tuple_values(current->GetTraceable<MidoriUnion>().m_values);
		}
	}
}

void GarbageCollector::Sweep(MidoriAllocator& allocator, size_t& sweep_count, size_t& bytes_reclaimed)
{
	const uint64_t* live_words = allocator.LiveBitWords();
	const size_t word_count = allocator.SlotWordCount();

	for (size_t word_index = 0uz; word_index < word_count; word_index += 1uz)
	{
		uint64_t garbage = live_words[word_index] & ~m_mark_bits[word_index];
		while (garbage != 0ull)
		{
			const size_t bit = static_cast<size_t>(std::countr_zero(garbage));
			garbage &= garbage - 1ull;

			MidoriTraceable* ptr = static_cast<MidoriTraceable*>(allocator.SlotAt(word_index * 64uz + bit));
			const size_t registered_size = ptr->GetSize();
			bytes_reclaimed += registered_size;
			m_total_bytes_allocated -= std::min(registered_size, m_total_bytes_allocated);
			sweep_count += 1uz;

			ptr->~MidoriTraceable();
			allocator.Free(ptr, sizeof(MidoriTraceable));
		}
	}
}

void GarbageCollector::ReclaimMemory(const GarbageCollectionRoots& roots, MidoriAllocator& allocator, bool force_clean)
{
	if (m_total_bytes_allocated < m_gc_threshold && !force_clean)
	{
		return;
	}

	// ... existing MIDORI_DEBUG_INFO "before" telemetry + timing unchanged ...

	m_mark_bits.assign(allocator.SlotWordCount(), 0ull);
	Trace(roots);

	size_t sweep_count = 0uz;
	size_t bytes_reclaimed = 0uz;
	Sweep(allocator, sweep_count, bytes_reclaimed);

	// ... existing MIDORI_DEBUG_INFO "after" telemetry unchanged, with mark_count
	//     replaced by allocator.LiveSlotCount() for the survivor figure ...

	size_t new_threshold = static_cast<size_t>(static_cast<double>(m_total_bytes_allocated) * GC_GROWTH_FACTOR);
	new_threshold = std::max(new_threshold, MIN_GC_THRESHOLD);
	new_threshold = std::min(new_threshold, MAX_GC_THRESHOLD);
	m_gc_threshold = new_threshold;
}

#if MIDORI_DEBUG_INFO
void GarbageCollector::PrintMemoryTelemetry()
{
	Printer::Print<Printer::Color::BLUE>
		(
			std::format
			(
				"Total allocated: {}\nObject count:    {}\n----------------------------------------------\n",
				FormatBytes(m_total_bytes_allocated),
				m_allocator != nullptr ? m_allocator->LiveSlotCount() : 0uz
			)
		);
}
#endif
```

Sweep note: `allocator.Free` clears the live bit of the word currently being iterated, but `garbage` was snapshotted before the inner loop, so the iteration is unaffected. The `live_words` pointer stays valid because sweeping never grows `m_live_bits`.

- [ ] **Step 3.6: Remove the mark flag from `MidoriTraceable`**

In `src/Common/Value/Value.h`: delete `bool m_is_marked = false;` and the declarations `void Mark(); void Unmark(); bool IsMarked() const;`.

In `src/Common/Value/Value.cpp`: delete the definitions of `MidoriTraceable::Mark`, `MidoriTraceable::Unmark`, `MidoriTraceable::IsMarked` (currently around lines 512-525).

- [ ] **Step 3.7: Build everything, fix any missed callers found by the compiler, run unit tests**

```powershell
cmake --build out/build/ninja/x64-development
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

Expected: all unit tests PASS, including the three `[gc]` tests from Step 3.2.

- [ ] **Step 3.8: Run the full `.mdr` suite**

```powershell
python scripts/run_tests.py --build Development
```

Expected: all PASS. If failures appear, use superpowers:systematic-debugging before changing anything.

- [ ] **Step 3.9: Commit**

```powershell
git add src/Interpreter/GarbageCollector src/Common/Value tests/unit/runtime/GarbageCollectorTests.cpp
git commit -m "feat(gc): bitmap mark-sweep collector integrated with allocator live bits"
```

---

## Task 4: Phase 1 verification — benchmarks vs baseline

**Files:** none modified.

- [ ] **Step 4.1: Build Release and compare against the Task 0 baseline**

```powershell
cmake --build out/build/ninja/x64-release
python scripts/bench.py --runs 7 --compare out/gc-baseline/Midori-baseline.exe
```

Expected: no benchmark regresses beyond noise; allocation-heavy workloads (`str_concat`, `array_operations`, `sorting`, `generate_paren` inside `all.mdr`, plus `perf_sort_100k`, `perf_text_midsize`) should improve or hold. Record the table in the commit message / progress notes.

- [ ] **Step 4.2: If any benchmark regresses >3%, STOP and investigate before Phase 2** (suspects: `TryGetSlotIndex` not inlining — check that it is defined in the cpp; if profiling shows it hot, move the native implementation into the header as `MIDORI_FORCE_INLINE`).

- [ ] **Step 4.3: Commit the benchmark record**

```powershell
git commit --allow-empty -m "perf(gc): phase 1 benchmark record vs pre-rewrite baseline"
```

Paste the bench.py comparison table into the commit body.

---

## Task 5: Generational mechanism in the GC (sticky marks, remembered set, minor/major)

**Files:**
- Modify: `src/Interpreter/GarbageCollector/GarbageCollector.h`
- Modify: `src/Interpreter/GarbageCollector/GarbageCollector.cpp`
- Modify: `tests/unit/runtime/GarbageCollectorTests.cpp`

- [ ] **Step 5.1: Write the failing tests**

Append to `tests/unit/runtime/GarbageCollectorTests.cpp`:

```cpp
TEST_CASE("Minor collection skips old objects but write barrier keeps old-to-young edges alive", "[gc][generational]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	MidoriTraceable* old_holder = AllocateArrayOf(allocator, gc, nullptr);
	GarbageCollector::GarbageCollectionRoots holder_roots{ old_holder };
	gc.CollectNow(holder_roots, allocator, GarbageCollector::CollectionKind::Minor);
	REQUIRE(allocator.Contains(old_holder));

	MidoriTraceable* young_child = AllocateText(allocator, gc, "young");
	gc.WriteBarrier(old_holder);
	old_holder->GetTraceable<MidoriArray>()[0] = MidoriValue(young_child);

	gc.CollectNow(holder_roots, allocator, GarbageCollector::CollectionKind::Minor);
	REQUIRE(allocator.Contains(old_holder));
	REQUIRE(allocator.Contains(young_child));

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.CollectNow(no_roots, allocator, GarbageCollector::CollectionKind::Major);
	REQUIRE_FALSE(allocator.Contains(old_holder));
	REQUIRE_FALSE(allocator.Contains(young_child));
}

TEST_CASE("Minor collection without barrier does not retain unreferenced young objects", "[gc][generational]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	MidoriTraceable* rooted = AllocateText(allocator, gc, "rooted");
	GarbageCollector::GarbageCollectionRoots roots{ rooted };
	gc.CollectNow(roots, allocator, GarbageCollector::CollectionKind::Minor);

	MidoriTraceable* young_garbage = AllocateText(allocator, gc, "young-garbage");
	gc.CollectNow(roots, allocator, GarbageCollector::CollectionKind::Minor);

	REQUIRE(allocator.Contains(rooted));
	REQUIRE_FALSE(allocator.Contains(young_garbage));

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.CollectNow(no_roots, allocator, GarbageCollector::CollectionKind::Major);
}

TEST_CASE("Write barrier deduplicates remembered objects", "[gc][generational]")
{
	MidoriAllocator allocator;
	GarbageCollector gc;
	gc.SetAllocator(&allocator);

	MidoriTraceable* old_holder = AllocateArrayOf(allocator, gc, nullptr);
	GarbageCollector::GarbageCollectionRoots roots{ old_holder };
	gc.CollectNow(roots, allocator, GarbageCollector::CollectionKind::Minor);

	gc.WriteBarrier(old_holder);
	gc.WriteBarrier(old_holder);
	gc.WriteBarrier(old_holder);
	REQUIRE(gc.RememberedSetSize() == 1uz);

	GarbageCollector::GarbageCollectionRoots no_roots;
	gc.CollectNow(no_roots, allocator, GarbageCollector::CollectionKind::Major);
	REQUIRE(gc.RememberedSetSize() == 0uz);
}
```

Also update the helper `AllocateArrayOf` to accept `nullptr` meaning "one default (non-pointer) element":

```cpp
	MidoriTraceable* AllocateArrayOf(MidoriAllocator& allocator, GarbageCollector& gc, MidoriTraceable* element)
	{
		MidoriArray array;
		if (element != nullptr)
		{
			array.AddBack(MidoriValue(element));
		}
		else
		{
			array.AddBack(MidoriValue(static_cast<MidoriInteger>(0)));
		}
		void* memory = allocator.Allocate(sizeof(MidoriTraceable));
		MidoriTraceable* traceable = new(memory) MidoriTraceable(std::move(array));
		gc.RegisterObject(traceable);
		return traceable;
	}
```

NOTE on the first test: storing `MidoriValue(static_cast<MidoriInteger>(0))` then overwriting index 0 with a pointer mimics the interpreter's `SET_ARRAY`. The integer 0 is not a valid slot pointer, so conservative tracing ignores it.

- [ ] **Step 5.2: Run to confirm compile failure**

```powershell
cmake --build out/build/ninja/x64-development --target MidoriUnitTests
```

Expected: FAILURE — `CollectionKind`, `CollectNow`, `WriteBarrier`, `RememberedSetSize` do not exist.

- [ ] **Step 5.3: Extend `GarbageCollector.h`**

Add to the public section:

```cpp
	enum class CollectionKind : uint8_t
	{
		Minor,
		Major
	};

	void CollectNow(const GarbageCollectionRoots& roots, MidoriAllocator& allocator, CollectionKind kind);

	size_t RememberedSetSize() const noexcept { return m_remembered_set.size(); }

	MIDORI_FORCE_INLINE void WriteBarrier(MidoriTraceable* target) noexcept
	{
		if (m_allocator == nullptr)
		{
			return;
		}

		const std::optional<size_t> slot_index = m_allocator->TryGetSlotIndex(target);
		if (!slot_index.has_value())
		{
			return;
		}

		const size_t word_index = *slot_index / 64uz;
		const uint64_t mask = 1ull << (*slot_index % 64uz);
		if (word_index >= m_mark_bits.size() || (m_mark_bits[word_index] & mask) == 0ull)
		{
			return;
		}
		if (word_index < m_logged_bits.size() && (m_logged_bits[word_index] & mask) != 0ull)
		{
			return;
		}

		if (m_logged_bits.size() < m_mark_bits.size())
		{
			m_logged_bits.resize(m_mark_bits.size(), 0ull);
		}
		m_logged_bits[word_index] |= mask;
		m_remembered_set.emplace_back(target);
	}
```

Add to the private section:

```cpp
	size_t m_live_bytes_after_major = INITIAL_GC_THRESHOLD;
	std::vector<uint64_t> m_logged_bits;
	std::vector<MidoriTraceable*> m_remembered_set;
#if MIDORI_DEBUG_INFO
	size_t m_minor_collection_count = 0uz;
	size_t m_major_collection_count = 0uz;
#endif

	void ClearRememberedSet() noexcept;
```

- [ ] **Step 5.4: Implement in `GarbageCollector.cpp`**

```cpp
void GarbageCollector::ClearRememberedSet() noexcept
{
	m_remembered_set.clear();
	std::fill(m_logged_bits.begin(), m_logged_bits.end(), 0ull);
}

void GarbageCollector::CollectNow(const GarbageCollectionRoots& roots, MidoriAllocator& allocator, CollectionKind kind)
{
	// ... MIDORI_DEBUG_INFO before-telemetry + timing scaffolding as in ReclaimMemory ...

	m_mark_bits.resize(allocator.SlotWordCount(), 0ull);

	if (kind == CollectionKind::Major)
	{
		std::fill(m_mark_bits.begin(), m_mark_bits.end(), 0ull);
		ClearRememberedSet();
#if MIDORI_DEBUG_INFO
		m_major_collection_count += 1uz;
#endif
	}
#if MIDORI_DEBUG_INFO
	else
	{
		m_minor_collection_count += 1uz;
	}
#endif

	Trace(roots);

	size_t sweep_count = 0uz;
	size_t bytes_reclaimed = 0uz;
	Sweep(allocator, sweep_count, bytes_reclaimed);

	if (kind == CollectionKind::Minor)
	{
		ClearRememberedSet();
	}
	else
	{
		m_live_bytes_after_major = std::max(m_total_bytes_allocated, MIN_GC_THRESHOLD);
	}

	// ... MIDORI_DEBUG_INFO after-telemetry, including kind, minor/major counters,
	//     sweep_count, bytes_reclaimed ...

	size_t new_threshold = static_cast<size_t>(static_cast<double>(m_total_bytes_allocated) * GC_GROWTH_FACTOR);
	new_threshold = std::max(new_threshold, MIN_GC_THRESHOLD);
	new_threshold = std::min(new_threshold, MAX_GC_THRESHOLD);
	m_gc_threshold = new_threshold;
}

void GarbageCollector::ReclaimMemory(const GarbageCollectionRoots& roots, MidoriAllocator& allocator, bool force_clean)
{
	if (m_total_bytes_allocated < m_gc_threshold && !force_clean)
	{
		return;
	}

	if (force_clean)
	{
		CollectNow(roots, allocator, CollectionKind::Major);
		return;
	}

	CollectNow(roots, allocator, CollectionKind::Minor);
	if (m_total_bytes_allocated > 2uz * m_live_bytes_after_major)
	{
		CollectNow(roots, allocator, CollectionKind::Major);
	}
}
```

Extend `Trace` so remembered objects act as gray (their children get marked). Add after the roots loop, before the mark-stack drain:

```cpp
	for (MidoriTraceable* remembered : m_remembered_set)
	{
		m_mark_stack.emplace_back(remembered);
	}
```

(Remembered objects are already marked, so pushing them directly — without `TryMark` — is what forces their children to be scanned. Duplicates from repeated logging cannot occur because `WriteBarrier` deduplicates via `m_logged_bits`.)

Move the telemetry/timing scaffolding from `ReclaimMemory` into `CollectNow` so both minor and major report.

The Phase 1 line `m_mark_bits.assign(allocator.SlotWordCount(), 0ull);` is REPLACED by the resize + conditional-clear shown above — after this task, mark bits are sticky across minor collections. `std::vector::resize` zero-fills only new words, which is exactly the "new blocks start young" rule from the spec.

- [ ] **Step 5.5: Build and run the unit tests**

```powershell
cmake --build out/build/ninja/x64-development
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

Expected: all PASS, including the three new `[generational]` tests AND the Phase 1 `[gc]` tests (`ReclaimMemory(..., force_clean=true)` now runs a Major, which behaves identically to the old full collection).

- [ ] **Step 5.6: Run the full `.mdr` suite (barriers are not wired yet, but ReclaimMemory's non-forced path now does minor collections — programs that mutate old containers may FAIL here; that is EXPECTED and is why this commit must not be pushed alone)**

```powershell
python scripts/run_tests.py --build Development
```

If failures occur, verify they are retention bugs of the un-barriered kind (old container mutated to point at young object). Do NOT debug them now — Task 6 wires the barriers. If failures look unrelated (crashes in programs that never mutate containers), STOP and use superpowers:systematic-debugging.

- [ ] **Step 5.7: Commit**

```powershell
git add src/Interpreter/GarbageCollector tests/unit/runtime/GarbageCollectorTests.cpp
git commit -m "feat(gc): sticky-mark generational core with remembered set (barriers not yet wired)"
```

---

## Task 6: Wire write barriers into the interpreter

**Rule:** any store of a `MidoriValue` into memory owned by an *existing* `MidoriTraceable` (array element/append, cell value, struct member) must be preceded by `m_gc.WriteBarrier(owning_traceable)`. Stores into stack slots, globals, and freshly constructed traceables need no barrier (they are roots or young).

**Files:**
- Modify: `src/Interpreter/VirtualMachine/VirtualMachine.cpp`

- [ ] **Step 6.1: Audit ALL mutation sites**

```powershell
Select-String -Path src/Interpreter/VirtualMachine/VirtualMachine.cpp -Pattern "GetTraceable<MidoriArray>\(\)\.(AddBack|AddFront|Extend|\[)|GetTraceable<MidoriCellValue>\(\)\.GetValue\(\) *=|GetTraceable<MidoriStruct>\(\)|GetTraceable<MidoriUnion>\(\)" | Select-Object LineNumber, Line
```

Compare hits against the list below. Every hit that WRITES (not reads) into an existing traceable must get a barrier. Known sites as of commit `e9ae37c` (line numbers approximate):

| Opcode / site | ~Line | Barrier target |
|---|---|---|
| `SET_ARRAY` | 1268 | traceable owning the finally-written array |
| `ADD_BACK_ARRAY` | 1314 | `arr.GetPointer()` |
| `ADD_FRONT_ARRAY` | 1324 | `arr.GetPointer()` |
| `ARRAY_APPEND` | 1865 | `container.GetPointer()` |
| FFI call, dynamic (array args) | ~2518 | `ptr` (each array passed to FFI) |
| FFI call, builtin (array args) | ~2622 | `ptr` (each array passed to FFI) |
| `SET_CELL` | 3093 | `(*env)[offset].GetPointer()` |
| `SET_LOCAL_CELL_WIDE` | 3160 | `ptr` (cell branch only) |
| `SET_CELL_WIDE` | 3187 | `(*env)[offset].GetPointer()` |
| `SET_MEMBER` | 3203 | `var.GetPointer()` |

Also check for narrow `SET_LOCAL_CELL` (non-wide) and any `Extend`/array-write sites the grep reveals that are not in this table — apply the same pattern. `TEXT_APPEND` and other `MidoriText` mutations need NO barrier (texts contain no `MidoriValue`s).

- [ ] **Step 6.2: Apply the barriers**

`SET_ARRAY` — track the owning traceable while descending nested arrays:

```cpp
		case OpCode::SET_ARRAY:
		{
			int num_indices = static_cast<int>(ReadByte(ip));
			MidoriValue value_to_set = Pop(sp);
			if (num_indices <= 0)
			{
				(void)Pop(sp);
				Push(sp, value_to_set);
				break;
			}

			MidoriValue* indices_begin = sp - num_indices;
			MidoriValue* arr_slot = indices_begin - 1;
			MidoriValue arr = *arr_slot;
			MidoriTraceable* arr_owner = arr.GetPointer();
			MidoriArray* arr_ref = &arr_owner->GetTraceable<MidoriArray>();
			MidoriInteger arr_size = static_cast<MidoriInteger>(arr_ref->GetLength());
			const int last_index = num_indices - 1;
			m_instruction_pointer = inst_ip;

			for (int i = 0; i < num_indices; i += 1)
			{
				MidoriValue& index = indices_begin[i];
				int return_code = CheckIndexBounds(index, arr_size);
				if (return_code != 0)
				{
					m_value_stack_pointer = arr_slot;
					m_value_stack_base_pointer = bp;
					m_curr_environment = env;
					return return_code;
				}
				MidoriValue& next_val = (*arr_ref)[static_cast<int>(index.GetInteger())];
				if (i != last_index)
				{
					arr_owner = next_val.GetPointer();
					arr_ref = &arr_owner->GetTraceable<MidoriArray>();
					arr_size = static_cast<MidoriInteger>(arr_ref->GetLength());
				}
				else
				{
					m_gc.WriteBarrier(arr_owner);
					next_val = value_to_set;
				}
			}

			*arr_slot = value_to_set;
			sp = arr_slot + 1;
			break;
		}
```

`ADD_BACK_ARRAY`:

```cpp
		case OpCode::ADD_BACK_ARRAY:
		{
			MidoriValue val = Pop(sp);
			MidoriValue& arr = Peek(sp);

			m_gc.WriteBarrier(arr.GetPointer());
			MidoriArray& arr_ref = arr.GetPointer()->GetTraceable<MidoriArray>();
			arr_ref.AddBack(val);

			break;
		}
```

`ADD_FRONT_ARRAY`: add `m_gc.WriteBarrier(arr.GetPointer());` immediately before `arr_ref.AddFront(val);`.

`ARRAY_APPEND`: add `m_gc.WriteBarrier(container.GetPointer());` immediately before the `AddBack` line.

`SET_CELL`:

```cpp
		case OpCode::SET_CELL:
		{
			int offset = static_cast<int>(ReadByte(ip));
			MidoriTraceable* cell_owner = (*env)[offset].GetPointer();
			m_gc.WriteBarrier(cell_owner);
			cell_owner->GetTraceable<MidoriCellValue>().GetValue() = Peek(sp);
			break;
		}
```

`SET_CELL_WIDE`: same transformation (keep the two-byte offset decode).

`SET_LOCAL_CELL_WIDE`: inside the `if (ptr != nullptr && m_gc.Contains(ptr) && ptr->IsTraceable<MidoriCellValue>())` branch, add `m_gc.WriteBarrier(ptr);` before the assignment. (Same for a narrow `SET_LOCAL_CELL` if the audit finds one.)

`SET_MEMBER`:

```cpp
		case OpCode::SET_MEMBER:
		{
			int index = static_cast<int>(ReadByte(ip));
			MidoriValue value = Pop(sp);
			MidoriValue& var = Peek(sp);
			m_gc.WriteBarrier(var.GetPointer());
			MidoriValue& member = var.GetPointer()->GetTraceable<MidoriStruct>().m_values[index];
			member = value;
			break;
		}
```

FFI sites (both): in the argument-marshalling loop, right after `m_ffi_array_args.push_back(array_arg);`, add `m_gc.WriteBarrier(ptr);` — the FFI function may write arbitrary values into the array buffer, so the array is conservatively logged. (Logging before the call is safe: no collection can run between marshalling and the FFI return.)

- [ ] **Step 6.3: Build and run the full `.mdr` suite**

```powershell
cmake --build out/build/ninja/x64-development
python scripts/run_tests.py --build Development
```

Expected: ALL PASS, including any failures observed in Step 5.6. If a test still fails, a mutation site is missing a barrier — re-run the Step 6.1 audit grep and inspect the failing program's opcodes before touching GC internals (superpowers:systematic-debugging).

- [ ] **Step 6.4: Run the unit suite**

```powershell
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

Expected: PASS.

- [ ] **Step 6.5: Commit**

```powershell
git add src/Interpreter/VirtualMachine/VirtualMachine.cpp
git commit -m "feat(gc): write barriers at container-mutation and FFI array sites"
```

---

## Task 7: GC stress test program

**Files:**
- Create: `test/gc/generational_churn.mdr`
- Create: `test/gc/generational_churn.expected`

The program keeps a 32-element text array and a mutable closure cell alive across heavy short-lived allocation churn (forcing many minor collections at the ~4 MB threshold), overwrites old-array slots and the closure cell with freshly allocated values (old→young edges through `SET_ARRAY` and `SET_CELL`), then prints everything. If a barrier is missing or promotion is broken, the printed values will be corrupted/crash and the `.expected` comparison fails.

- [ ] **Step 7.1: Write the stress program**

Create `test/gc/generational_churn.mdr` (syntax modeled on `benchmark/array_operations.mdr`, `test/closure/mutable.mdr`, `test/for_loop/array_iteration.mdr` — if any construct below fails to compile, cross-check against those files and adjust the syntax, not the test's structure):

```
module GenerationalChurn

import
{
	"../../MidoriPrelude/Appendable.mdr",
	"../../MidoriPrelude/IO.mdr"
}

def make_counter = fn(seed : Int) : fn(Int) -> Int => fn(delta : Int) : Int => { seed = seed + delta; seed };

def counter = make_counter(0);

def keeper : Array<Text> = [];
def init = 0;
loop
{
	if init >= 32
	then break ()
	else
	{
		Appendable::Append(keeper, "seed-" ++ (init as Text));
		init = init + 1;
	}
};

def i = 0;
loop
{
	if i >= 32000
	then break ()
	else
	{
		def scratch : Array<Text> = [];
		Appendable::Append(scratch, "churn-" ++ (i as Text));
		Appendable::Append(scratch, scratch[0] ++ "-x");
		Appendable::Append(scratch, scratch[1] ++ "-y");
		Appendable::Append(scratch, scratch[2] ++ "-z");

		if i % 1000 == 0
		then
		{
			keeper[i / 1000] = "fresh-" ++ (i as Text);
			counter(1);
			()
		}
		else ();

		i = i + 1;
	}
};

for item in keeper {
	IO::PrintLine(item);
};
IO::PrintLine(counter(0) as Text);
```

Each `keeper` slot `k` (0-31) is overwritten exactly once at iteration `i = 1000 * k`, and `counter` accumulates one increment per overwrite, so the final output is fully deterministic.

Create `test/gc/generational_churn.expected` with exactly these 33 lines (the runner compares against the sibling `.expected` file — `run_tests.py` line 173):

```
fresh-0
fresh-1000
fresh-2000
fresh-3000
fresh-4000
fresh-5000
fresh-6000
fresh-7000
fresh-8000
fresh-9000
fresh-10000
fresh-11000
fresh-12000
fresh-13000
fresh-14000
fresh-15000
fresh-16000
fresh-17000
fresh-18000
fresh-19000
fresh-20000
fresh-21000
fresh-22000
fresh-23000
fresh-24000
fresh-25000
fresh-26000
fresh-27000
fresh-28000
fresh-29000
fresh-30000
fresh-31000
32
```

(`fresh-N` values are the texts written at each overwrite; `32` is the counter total. If the program needed syntax adjustments in a way that changes output — e.g. `keeper[i / 1000] = ...` needs different assignment syntax — regenerate the `.expected` from a manual run ONLY after verifying by eye that every line matches what the program logically must print. Never paste failing output into `.expected` to make a test green.)

- [ ] **Step 7.2: Run it in isolation**

```powershell
python scripts/run_tests.py --build Development --test gc/generational_churn.mdr --verbose
```

Expected: PASS. To confirm it actually exercises the generational machinery, also run the Development binary directly with internal diagnostics enabled (`MidoriBuild::ShouldEmitInternalDiagnostics` is env-gated — find the exact variable name with `Select-String -Path src -Pattern "EnvironmentFlagEnabledUncached" -Recurse` and check its call sites) and verify the telemetry shows BOTH minor and major collections, and a non-zero remembered set.

- [ ] **Step 7.3: Full suites once more, then commit**

```powershell
python scripts/run_tests.py --build Development
ctest --test-dir out/build/ninja/x64-development --output-on-failure
git add test/gc
git commit -m "test(gc): generational churn stress program"
```

---

## Task 8: Final verification and benchmark report

**Files:** none modified (telemetry tweaks only if needed).

- [ ] **Step 8.1: Release build + benchmark comparison against the Task 0 baseline**

```powershell
cmake --build out/build/ninja/x64-release
python scripts/bench.py --runs 7 --compare out/gc-baseline/Midori-baseline.exe
```

Success criteria from the spec: allocation-heavy workloads improve; `fib35` within noise. If a workload regressed: check whether minor collections are running too often (threshold policy) or the barrier is in a hot non-mutating path (it must only be on the opcodes listed in Task 6).

- [ ] **Step 8.2: Full test suites, one last time**

```powershell
python scripts/run_tests.py --build Development
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

Expected: ALL PASS. Do not claim completion without this output (superpowers:verification-before-completion).

- [ ] **Step 8.3: Format check (repo has one)**

```powershell
python scripts/check_format.py
```

Fix anything it flags.

- [ ] **Step 8.4: Final commit with the benchmark table in the body**

```powershell
git commit --allow-empty -m "perf(gc): generational GC final benchmark record"
```

- [ ] **Step 8.5: Use superpowers:finishing-a-development-branch** to decide merge/PR/cleanup (work is on branch `mono-vm` — confirm with the user how they want it integrated).

---

## Self-review notes (already applied)

- Spec coverage: allocator API (Task 1), Emscripten unification (Task 2), bitmap mark-sweep + `m_is_marked` removal (Task 3), Phase 1 gate + benchmarks (Task 4), sticky marks/remembered set/minor-major policy incl. escalation and `force_clean`⇒Major (Task 5), write barriers incl. sites beyond the spec's four (FFI, `SET_MEMBER`, `ADD_FRONT_ARRAY`, `ARRAY_APPEND`, `SET_LOCAL_CELL_WIDE`) (Task 6), stress test (Task 7), success criteria (Task 8). Out-of-bitmap mark-bit reads treated as young: `WriteBarrier` bounds-checks `word_index >= m_mark_bits.size()`.
- Type consistency: `CollectionKind`, `CollectNow`, `WriteBarrier`, `RememberedSetSize`, `TryGetSlotIndex`, `SlotAt`, `SlotWordCount`, `LiveBitWords`, `LiveSlotCount` used identically across tasks.
- Known intentional deviation from spec: spec's `Deallocator` alias removal is conditional on the Task 3.1 audit; spec's "logged bits parallel to mark bits" implemented as lazily-resized vector.
