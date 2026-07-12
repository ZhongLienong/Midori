# Midori GC Overhaul — Design Spec

Date: 2026-07-10
Status: Approved
Goal: **Throughput** — lower total GC overhead so programs run faster overall.

## Background

Each `VirtualMachine` owns a private heap: a `MidoriAllocator` (contiguous reserved
region, 64 KB blocks, fixed 80-byte slots) plus a stop-the-world mark-sweep
`GarbageCollector`. Workers are separate threads with separate heaps; values cross
threads only via serialization, so the GC is single-threaded by construction.

Facts that constrain the design:

- Every GC-managed object is a `MidoriTraceable`, exactly one 80-byte slot.
  Large `malloc` allocations are only internal buffers of `MidoriText` /
  `MidoriArray` / `MidoriTuple`, owned and destroyed by their traceable. They are
  never traced and never roots.
- `MidoriValue` is an untagged 8-byte union in release builds. Root and child
  pointer identification is conservative (region range + slot alignment + live
  bit). **Moving/copying collectors are therefore off the table.** Non-moving
  designs are unaffected: a false-positive "pointer" merely over-retains.
- Pointer-mutation sites in the interpreter are few: `SET_ARRAY`, `SET_CELL`,
  `SET_CELL_WIDE`, and array `AddBack` (two call sites). This makes write
  barriers cheap and auditable.

Current inefficiencies this design removes:

1. `GarbageCollector::m_traceables` — a vector holding every live object —
   duplicates tracking the allocator's live bitmap already does; every
   allocation pays a vector push, every sweep walks the whole vector.
2. Every collection is a full-heap trace: long-lived objects (string literal
   cache, small-string pool, static closures, globals) are re-marked and
   re-swept each cycle.
3. The mark flag lives inside each object (`m_is_marked`), dirtying live
   objects' cache lines during marking.
4. Two divergent allocator implementations (native region vs Emscripten
   `unordered_set`), forcing the GC to stay lowest-common-denominator.

## Phase 1 — Allocator-integrated bitmap mark-sweep

### MidoriAllocator (owner of memory, live bits, free list, slot indexing)

New public API:

- `std::optional<size_t> TryGetSlotIndex(const void* ptr) const noexcept` —
  returns the global slot index if `ptr` is a committed, slot-aligned address;
  does not consult live bits.
- `void* SlotAt(size_t slot_index) const noexcept`
- `size_t SlotWordCount() const noexcept` — number of 64-bit live-bit words.
- `const uint64_t* LiveBitWords() const noexcept` — read-only view for the
  sweep's `live & ~mark` word walk.
- `Contains(ptr)` keeps its current meaning (range + alignment + live bit).

The Emscripten path is rebuilt on the same block/slot/bitmap structure: blocks
are individually `std::malloc`'d (or `aligned_alloc`'d) 64 KB chunks tracked in
a sorted vector of block base addresses; `TryGetSlotIndex`/`Contains` binary-search
the block table. The `unordered_set` path is deleted. Result: one allocator
model, one GC implementation.

Traceables are always slot-sized (static-asserted today); the large-allocation
path remains only for internal buffers and is untouched by the GC.

### GarbageCollector (owner of mark bits, mark stack, policy)

- Delete `m_traceables` and per-object `m_is_marked` /
  `Mark()` / `Unmark()` / `IsMarked()` on `MidoriTraceable`.
- Add `std::vector<uint64_t> m_mark_bits`, kept sized to the allocator's
  `SlotWordCount()` (resized lazily at collection start). Any read of a mark
  bit whose slot index is beyond the current bitmap (a block committed since
  the last collection) treats the slot as unmarked/young.
- `TryMark(ptr)`: `TryGetSlotIndex` → live-bit test → mark-bit test-and-set →
  push on mark stack if newly marked. No object memory is written during mark.
- Sweep: for each word, `garbage = live_word & ~mark_word`; for each set bit,
  `SlotAt` → run `~MidoriTraceable()` → free the slot. Bulk word-level skip of
  fully-live and fully-free words.
- `RegisterObject` reduces to byte accounting (`m_total_bytes_allocated +=
  size`); no per-object bookkeeping.
- Threshold policy (`INITIAL/MIN/MAX_GC_THRESHOLD`, `GC_GROWTH_FACTOR`, clamp
  behavior) is unchanged in Phase 1.

### Out of scope for Phase 1

- No interpreter-loop changes.
- `AllocateTraceable` does not null-check the allocator result today
  (placement-new into null is UB on true OOM). Pre-existing, unrelated; noted,
  not fixed here.

## Phase 2 — Generational collection via sticky mark bits

Non-moving generational: "old" = mark bit still set from a previous cycle.

- **Minor collection** (the default): do *not* clear mark bits beforehand.
  Trace from VM roots plus the remembered set. Already-marked (old) objects
  terminate tracing immediately; only young objects get traced and marked.
  Sweep as in Phase 1 (`live & ~mark`) — old objects are marked, so they
  survive without being visited individually. Young survivors keep their mark
  bit: that is promotion.
- **Remembered set**: a `std::vector<MidoriTraceable*>` plus a "logged" side
  bitmap (parallel to mark bits) for O(1) dedup. During a minor trace,
  remembered objects are treated as gray: their children are marked, they stay
  logged-cleared afterward.
- **Write barrier** at the four mutation sites (`SET_ARRAY`, `SET_CELL`,
  `SET_CELL_WIDE`, `AddBack` call sites): when storing a value into an object
  whose mark bit is set (old), and the stored value's bits identify a live young
  heap slot, log the *target* object. Cost: one bit test + rarely-taken branch.
  Conservative value test is fine — false positives only add a remembered-set
  entry.
- **Major collection**: clear all mark bits and the remembered set, then full
  trace + sweep exactly as Phase 1. Triggered when live bytes after a minor
  exceed 2× live bytes after the last major, or on `force_clean`
  (VM shutdown path).
- VM-pinned objects (string literal cache, small-string pool, static closures)
  become old after the first cycle and stop being re-traced — the main
  throughput win for steady-state programs.

## Component boundaries

| Component | Owns | Interface |
|---|---|---|
| `MidoriAllocator` | region/blocks, live bits, free list, slot indexing | `Allocate`, `Free`, `Contains`, `TryGetSlotIndex`, `SlotAt`, `LiveBitWords`, `SlotWordCount` |
| `GarbageCollector` | mark bits, logged bits, remembered set, mark stack, thresholds/policy | `ReclaimMemory(roots, allocator, force_clean)`, `ShouldCollect`, `RegisterObject`, `WriteBarrier(target, value)` |
| `VirtualMachine` | roots, barrier call sites | `BuildGarbageCollectionRoots`, calls `WriteBarrier` at the four mutation sites |

## Telemetry

`MIDORI_DEBUG_INFO` output kept and extended: collection kind (minor/major),
mark/sweep timings, survivor and reclaim counts, remembered-set size, and
minor/major cycle counters.

## Testing & success criteria

- Full `test/` suite passes after each phase (existing runner/scripts).
- `benchmark/` suite: wall-time improvement on allocation-heavy programs
  (`str_concat`, `array_operations`, `sorting`, `generate_paren`);
  no regression on allocation-light `fib35`.
- New stress program under `test/`: churns short-lived arrays/texts while
  mutating a long-lived structure (old→young pointers), validating barrier and
  promotion correctness under forced collections.
- Phase 1 is implemented, verified, and benchmarked before Phase 2 begins.

## Implementation deviations (accepted)

- Emscripten block table is append-only with linear `FindBlockIndex` (not
  sorted/binary-search): slot indices must stay stable because persistent
  mark/logged bitmaps are keyed by them.
- `WriteBarrier` takes only the target (no stored-value young test):
  conservative over-approximation, deduped by logged bits; there are 13
  barrier sites plus a defensive `BIND_CAPTURES` barrier, not four.
- `WriteBarrier` is out-of-line (`MIDORI_NOINLINE`), not an inline bit test:
  `ExecuteLoop` is code-alignment sensitive; see commit 0a7921e.
- Minor/major collection counters are unconditional data members (debug-only
  members caused cross-TU layout divergence; commit 3ae741b).
