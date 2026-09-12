# Delete In-Place Mutation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** No Midori expression writes into an object that already exists.
`Appendable`, `Prependable` and `Extendable` go, with their five FFI functions
and two intrinsic opcodes.

**Architecture:** deleting assignment (`2c2b463`) removed the assignment
*operator* but not mutation — these three typeclasses still write in place, and
two GC tests now depend on them. This plan finishes the job. The replacement
idiom already exists and is already fast; the work is migrating eleven call
sites, splitting one test that covers two subjects, and deleting the machinery.

**Tech Stack:** C++23 compiler; `.mdr` corpus migration; Midori prelude.

---

## Why this is safe to do — measured, not assumed

The obvious objection is performance: immutable append copies. Measured on this
branch at `c925fcf`, building an array of N elements, every result checked:

| N | in-place `Append` | `WithAppended` recursion | `[e for x in it]` | cons `List` + `ListToArray` |
|---|---|---|---|---|
| 5,000 | 95 ms | 640 ms | — | 154 ms |
| 20,000 | 97 ms | 8,481 ms | 90 ms | 172 ms |
| 40,000 | 90 ms | 34,589 ms | — | 155 ms |

`WithAppended` in a loop is quadratic and is **not** the migration target. The
two linear idioms are:

1. **A comprehension** — `[f(x) for x in iterable]`, one pass, works over
   `Range`, `Array` and any `Iterable`.
2. **A cons `List`** — `List::Cons` is O(1), then `List::ListToArray` once.

Both land within ~2x of in-place `Append`, and ~90 ms of every figure above is
process startup. There is no performance argument for keeping mutation.

## What in-place mutation is actively costing

`Appendable::Append` on `Text` mutates through aliases:

```midori
def a = "core";
def b = a;              // bound before the append
Appendable::Append(a, "!");
// a=core!   b=core!    <-- b changed
```

`b` is bound to a value, never reassigned, and still changes. In a language
whose whole point is that bindings do not change, this is the one hole left.

## The cost, stated before starting

**`test/gc/generational_churn` loses its old-to-young write coverage, and
nothing replaces it.** Writing a young pointer into an already-promoted object
is exactly what immutability makes unexpressible, so the write barrier it
exercises has no source-level trigger left. The test keeps its allocation churn
and its promotion, and loses that one thing. Do not paper over this by keeping a
mutating call "just for the GC test" — say it in the commit, as the closure
tests did in `73789c0`.

`test/concurrency/gc_stress_arrays` loses its incremental-growth pattern: it
built an array by repeated `Append`, which reallocates the backing store as it
grows. A comprehension allocates once. The allocation *volume* is the same; the
reallocation path is not exercised.

## Known traps

1. **`test/prelude/success/builtin_type_operations_typeclasses.mdr` covers two
   subjects.** Lines 13-16 and 47-53 are the mutating classes; everything else
   is `Concatenable` (`++`), which survives and is genuinely tested there. Split
   it — do not delete it.
2. **`test/typeclass/success/generic_construction_in_method_argument.mdr` uses
   `Appendable` as a *vehicle*, not a subject.** It pins argument-directed
   instance selection, which needs a class with two type parameters and two
   instances so the container fixes the element type. Deleting `Appendable`
   removes the only such class in the prelude, so the test must carry its own.
   Read its header comment before touching it.
3. **`Append` is not only a prelude function.** `MIDORI_FFI_ArrayAppend` and
   `MIDORI_FFI_TextAppend` are lowered to dedicated opcodes `ARRAY_APPEND` and
   `TEXT_APPEND` in `CodeGenerator.cpp:3883-3898`. The FFI entries, the opcodes,
   the VM cases and the disassembler cases all go.
4. **There are two live preludes.** `<Name>` system imports resolve through
   `MIDORI_PATH` to an installed copy under `AppData`, which nothing syncs. After
   every prelude change run
   `cp -r MidoriPrelude "$LOCALAPPDATA/Midori/MidoriPrelude"` or the
   `<IO>`-importing tests keep compiling the old one. This cost 20 confusing
   failures last time.
5. **`benchmark/` and `misc/` are not a migration cost.** Between them they hold
   78 `Append` call sites, and neither compiles today — `benchmark/` fails on
   `DateTime::GetTime` not being exported, `misc/` on drift that predates this
   branch. Neither is run by `Midori.exe test`. Leave them; do not let them
   inflate the estimate.
6. **Non-tail recursion overflows at roughly 3,333 frames.** Tail calls are
   optimised (100,000 deep is fine), so every helper written here must be
   tail-recursive or bounded. `ListFromArray` was fixed for exactly this in
   `c925fcf`.

**Build and test — PowerShell, NOT Git Bash:**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Baseline **368/368** and **1024 assertions / 176 cases**, green at `c925fcf`. Do
**not** use `build/out/Midori.exe` — a stale April build that silently gives
wrong answers.

Checkout is shared with other sessions. `git status` first; stage by explicit
pathspec, never `git add -A`.

---

## File structure

| file | change |
|---|---|
| `MidoriPrelude/Appendable.mdr` | delete |
| `MidoriPrelude/Prependable.mdr` | delete |
| `MidoriPrelude/Extendable.mdr` | delete |
| `MidoriPrelude/ArrayUtil.mdr:5-7` | delete the three forwarders |
| `test/prelude/success/builtin_type_operations_typeclasses.mdr` | split: keep the `Concatenable` half |
| `test/typeclass/success/generic_construction_in_method_argument.mdr` | carry its own two-instance class |
| `test/gc/generational_churn.mdr` | rewrite; loses write-barrier coverage |
| `test/concurrency/success/gc_stress_arrays.mdr` | build by comprehension |
| `test/concurrency/success/worker_join_index_regression.mdr` | accumulate through recursion |
| `src/Library/MidoriBuiltinFFIRegistry/MidoriFFIRegistry.h` | drop 5 FFI entries |
| `src/Library/BuiltinTypes.cpp`, `src/Library/MidoriStdLibExports.h` | drop 5 implementations |
| `src/Compiler/CodeGenerator/CodeGenerator.cpp:3883-3898` | drop intrinsic lowering |
| `src/Common/Executable/Executable.h` | drop `ARRAY_APPEND`, `TEXT_APPEND` |
| `src/Interpreter/VirtualMachine/VirtualMachine.cpp` | drop both opcode cases |
| `src/Utility/Disassembler/Disassembler.cpp` | drop both opcode cases |
| `docs/superpowers/specs/2026-09-01-...md` §4 | tick the three forwarders; add the classes |

---

## Task 1: Reproduce the baseline before changing anything

The numbers above decide the whole plan. Confirm they still hold, and confirm
you can tell a real run from a crash — the first attempt at this measurement
timed a stack overflow and nearly recorded a 330x improvement that did not
exist.

**Files:**
- Create: `test/_bench_check.mdr` (temporary, deleted in step 4)

- [ ] **Step 1: Write the benchmark, printing the length so a crash is visible**

```midori
module BenchCheck

import
{
	"../MidoriPrelude/IO.mdr",
	"../MidoriPrelude/ArrayUtil.mdr",
	"../MidoriPrelude/Prelude/List.mdr",
}

def N = 40000;

def Build = fn(acc : List<Int>, i : Int, n : Int) -> List<Int> =>
	if i >= n then acc else Build(List::Cons(i, acc), i + 1, n);

def out = List::ListToArray(Build(List::Nil(), 0, N));
IO::PrintLine("len=" ++ (ArrayUtil::Length(out) as Text));
```

- [ ] **Step 2: Run it and read the output, not just the timing**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/_bench_check.mdr
```

Expected: `len=40000` in well under one second. If you see `panic[StackOverflow]`
or no `len=` line, stop — you are timing a crash, and any number you take from
it is meaningless.

- [ ] **Step 3: Confirm the comprehension path too**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/_bench_check.mdr
```

Expected: same, `len=40000`.

- [ ] **Step 4: Delete the temporary file**

```bash
rm -f test/_bench_check.mdr
```

No commit — nothing in the repo changed.

---

## Task 2: Split the test that covers two subjects

`builtin_type_operations_typeclasses.mdr` tests the mutating classes *and*
`Concatenable`. Only the first is going away.

**Files:**
- Modify: `test/prelude/success/builtin_type_operations_typeclasses.mdr`
- Test: the file is its own test; it prints `...: PASS`

- [ ] **Step 1: Rewrite the file keeping only the `Concatenable` coverage**

Replace the whole file with:

```midori
module BuiltinTypeOperationsTypeclasses

import
{
    "../../../MidoriPrelude/IO.mdr",
    "../../../MidoriPrelude/Concatenable.mdr",
    "../../../MidoriPrelude/Prelude/Panic.mdr"
}

// Previously also covered Appendable, Prependable and Extendable. Those three
// mutated their argument in place and were deleted; what remains here is
// Concatenable, which returns a new value and is unaffected.

def ConcatPair = fn<T>(left: T, right: T) -> T
    where Concatenable<T> => left ++ right;

def arr = [1, 2, 3, 4, 5, 6];

def combined = arr ++ [7, 8];
if #combined == 8
then ()
else Panic::Panic("Array Concat should return a larger array");

if combined[0] == 1 && combined[7] == 8
then ()
else Panic::Panic("Array Concat should preserve both sides");

if #arr == 6
then ()
else Panic::Panic("Array Concat should not mutate the left operand");

def combined_via_constraint = ConcatPair(arr, [9, 10]);
if #combined_via_constraint == 8 && combined_via_constraint[0] == 1 && combined_via_constraint[7] == 10
then ()
else Panic::Panic("Array ++ should dispatch through Concatenable in constrained generic code");

def text = "mid-core!";

def joined = text ++ "?";
if joined == "mid-core!?"
then ()
else Panic::Panic("Text Concat should return the combined text");

if text == "mid-core!"
then ()
else Panic::Panic("Text Concat should not mutate the left operand");

def joined_via_constraint = ConcatPair(text, "!");
if joined_via_constraint == "mid-core!!"
then ()
else Panic::Panic("Text ++ should dispatch through Concatenable in constrained generic code");

IO::PrintLine("builtin_type_operations_typeclasses: PASS");
```

Note the two added "should not mutate the left operand" checks. They were
implicit before because the mutating calls came first; now they are the point.

- [ ] **Step 2: Run it**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/prelude/success/builtin_type_operations_typeclasses.mdr
```

Expected: `builtin_type_operations_typeclasses: PASS`

- [ ] **Step 3: Commit**

```bash
git add test/prelude/success/builtin_type_operations_typeclasses.mdr
git commit -m "Narrow builtin_type_operations_typeclasses to Concatenable"
```

---

## Task 3: Give the inference test its own two-instance class

Read the header comment in the file first. It pins argument-directed instance
selection: the class has two type parameters and two instances, so no parameter
type is known until the container argument settles the instance. `Appendable`
was the only prelude class shaped that way.

**Files:**
- Modify: `test/typeclass/success/generic_construction_in_method_argument.mdr`

- [ ] **Step 1: Replace the file, defining the class locally**

```midori
module GenericConstructionInMethodArgument

// The same inference as test/generics/success/generic_construction_in_call_argument.mdr,
// but the callee is a class method rather than an ordinary generic function. The two
// travel different paths in TypeChecker::operator()(MidoriExpression::Call&) and failed
// for different reasons, so they are pinned separately.
//
// Instance selection is argument-directed: `Collect` has an Array instance and a Text
// instance, so no parameter type is known until the arguments have been checked, and the
// arguments were therefore checked with no expected type at all. NarrowClassMethodType
// narrows the instances against the arguments already settled - once the container has
// fixed the instance, the element parameter is known and the construction infers from it.
//
// This used Appendable, which had exactly this shape. Appendable was deleted for
// mutating in place, and no prelude class has two type parameters and two
// instances any more, so the test carries its own.

import
{
	"../../../MidoriPrelude/IO.mdr",
	"../../../MidoriPrelude/ArrayUtil.mdr",
}

type Slot<K, V> = Empty | Occupied(K, V);

class Collect < C, Elem >
{
	Of: fn(container: C, elem: Elem) -> C;
};

instance Collect < Array < T >, T >
{
	def Of = fn(container: Array < T >, elem: T) -> Array < T > => ArrayUtil::WithAppended(container, elem);
};

instance Collect < Text, Text >
{
	def Of = fn(container: Text, elem: Text) -> Text => container ++ elem;
};

def empty : Array<Slot<Int, Text>> = [];
def slots = Collect::Of(Collect::Of(empty, Slot::Empty()), Slot::Occupied(2, "two"));

IO::PrintLine("slots: " ++ (#slots as Text));

// The Text instance of the same class still selects on its own arguments.
def greeting = Collect::Of("hello", " world");

IO::PrintLine("text: " ++ greeting);

match slots[1] with
	case Slot::Occupied(k, v) => IO::PrintLine("occupied: " ++ (k as Text) ++ "/" ++ v)
	case Slot::Empty => IO::PrintLine("empty")
;
```

- [ ] **Step 2: Run it and compare against the old output**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/typeclass/success/generic_construction_in_method_argument.mdr
```

Expected, unchanged from before:

```
slots: 2
text: hello world
occupied: 2/two
```

If the construction `Slot::Empty()` now fails to infer, the local class is not
reproducing the original shape — check it really has two type parameters and two
instances, because that is what the test exists to exercise.

- [ ] **Step 3: Do NOT regenerate the snapshot — check the existing one still passes**

This file already has a `.expected`, committed in `a284fd3` against the old
`Appendable`-based version. (An earlier survey said otherwise; it was wrong.)
That is better than a fresh capture: if the rewritten file reproduces a snapshot
written against the implementation it replaces, the behaviour provably did not
change. Regenerating it would throw that evidence away.

```bash
./out/build/ninja/x64-development/out/Midori.exe test typeclass
```

Expected: 51/51, with this file passing against its unmodified snapshot.

- [ ] **Step 4: Verify the snapshot bites**

```bash
echo CORRUPT >> test/typeclass/success/generic_construction_in_method_argument.expected
./out/build/ninja/x64-development/out/Midori.exe test typeclass
```

Expected: `generic_construction_in_method_argument.mdr` **FAILS**. Then restore
it by regenerating with the step 3 command and re-run to confirm it passes.

- [ ] **Step 5: Commit**

```bash
git add test/typeclass/success/generic_construction_in_method_argument.mdr test/typeclass/success/generic_construction_in_method_argument.expected
git commit -m "Give the instance-selection test its own two-instance class"
```

---

## Task 4: Migrate the three remaining call sites

**Files:**
- Modify: `test/concurrency/success/gc_stress_arrays.mdr:19-27`
- Modify: `test/concurrency/success/worker_join_index_regression.mdr`
- Modify: `test/gc/generational_churn.mdr`

- [ ] **Step 1: `gc_stress_arrays` — build by comprehension**

Replace the `Fill` helper and its call. The current code is:

```midori
def Fill = fn(arr: Array<Int>, i: Int, size: Int) -> Unit =>
    if i >= size
    then ()
    else
    {
        Appendable::Append(arr, i * i);
        Fill(arr, i + 1, size)
    };
```

Delete `Fill` entirely and change `BuildArrayWorker` to:

```midori
def BuildArrayWorker = fn(ch: Channel<Int>, size: Int) -> Int => {
    // Was built by repeated in-place Append. A comprehension allocates the
    // array once: same allocation volume, but the reallocate-as-it-grows path
    // is no longer exercised.
    def arr : Array<Int> = [i * i for i in 0..1..size];
    ch -> Total(arr, 0, 0, size);
    0
};
```

Also remove the now-unused `Appendable` import and its `use` line at the top of
the file.

- [ ] **Step 2: Verify it byte-for-byte against its snapshot**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/concurrency/success/gc_stress_arrays.mdr 2>&1 \
  | sed 's/\x1b\[[0-9;]*m//g' > /tmp/out.txt
diff <(tr -d '\r' < test/concurrency/success/gc_stress_arrays.expected) <(tr -d '\r' < /tmp/out.txt)
```

Expected: no output from `diff`. The `tr -d '\r'` is required — snapshots are
checked out CRLF and fresh output is LF, so a raw `diff` reports every line as
changed even when the values are identical.

- [ ] **Step 3: `worker_join_index_regression` — carry the array through the recursion**

`SpawnBands` currently does `Appendable::Append(workers, worker)` against a
`workers` array declared in the enclosing scope. Change `SpawnBands` to take the
array and return the extended one, and remove the outer `def workers` mutation.

Replace the `def workers` declaration and `SpawnBands` with:

```midori
    // 32 bands, so WithAppended's copy is irrelevant here; the array is rebuilt
    // 32 times at up to 32 elements. Spawn order is unchanged, which is what the
    // join indices below depend on.
    def SpawnBands = fn(workers : Array<Worker<Array<Int>>>, band_index : Int, next_row : Int) -> Array<Worker<Array<Int>>> =>
        if band_index >= worker_count
        then workers
        else
        {
            def band_rows = if band_index < remainder then base_rows + 1 else base_rows;
            def y_start = next_row;
            def y_end = y_start + band_rows;
            def immediate = join spawn Answer(0, 2, 64, 64, 32, 1, -2.5, -1.2, 1.0, 1.2, 0, 0.0, 0.0);
            if immediate[0] == 0
            then ()
            else Panic::Panic("immediate join failed");
            def worker = spawn Answer(y_start, y_end, width, height, limit, samples_per_axis, view_min_r, view_min_i, view_max_r, view_max_i, fractal_kind, julia_cr, julia_ci);

            SpawnBands(ArrayUtil::WithAppended(workers, worker), band_index + 1, y_end)
        };

    def empty_workers : Array<Worker<Array<Int>>> = [];
    def workers = SpawnBands(empty_workers, 0, 0);
```

Replace the `Appendable.mdr` import with `ArrayUtil.mdr`.

- [ ] **Step 4: Verify against its snapshot**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/concurrency/success/worker_join_index_regression.mdr 2>&1 \
  | sed 's/\x1b\[[0-9;]*m//g' > /tmp/out.txt
diff <(tr -d '\r' < test/concurrency/success/worker_join_index_regression.expected) <(tr -d '\r' < /tmp/out.txt)
```

Expected: no output from `diff`, i.e. still `ok`.

- [ ] **Step 5: `generational_churn` — accumulate through a cons List**

This is the one that loses coverage. The code below has been run against the
existing snapshot and matches it byte for byte; it also replaces the `scratch`
allocations, so Task 5 Step 1 is already done by this edit.

Replace the whole file with:

```midori
module GenerationalChurn

import
{
	"../../MidoriPrelude/ArrayUtil.mdr",
	"../../MidoriPrelude/IO.mdr",
	"../../MidoriPrelude/Prelude/List.mdr"
}

def Main = fn() -> Unit =>
{
	// Was: a long-lived `keeper` array receiving young pointers by in-place
	// Append, which is what made this an old-to-young write-barrier test. No
	// expression can write into an existing object any more, so that coverage is
	// gone and nothing replaces it. What remains is the allocation churn and the
	// promotion of a structure that survives the whole run.
	//
	// Cons is O(1), so the accumulation stays linear; the list is converted once
	// at the end.
	def Churn = fn(i : Int, limit : Int, keeper : List<Text>) -> List<Text> =>
		if i >= limit
		then keeper
		else
		{
			def base = "churn-" ++ (i as Text);
			def scratch : Array<Text> = [base, base ++ "-x", base ++ "-x-y", base ++ "-x-y-z"];

			// Reads `scratch`, so the four strings cannot be eliminated as dead. The
			// allocation is the entire point of this test; an unread binding would let
			// the optimizer delete it and the file would pass while measuring nothing.
			def churned = ArrayUtil::Length(scratch);

			Churn(i + 1, limit, if i % 1000 == 0 && churned == 4 then List::Cons("fresh-" ++ (i as Text), keeper) else keeper)
		};

	def keeper = ArrayUtil::Reverse(List::ListToArray(Churn(0, 32000, List::Nil())));

	for item in keeper
	{
		IO::PrintLine(item);
	};

	IO::PrintLine(keeper[ArrayUtil::Length(keeper) - 1]);
	IO::PrintLine(ArrayUtil::Length(keeper) as Text);
};

Main();
```

Two things here are load-bearing and were both got wrong on the first attempt:

- **`ArrayUtil::Reverse`** — `Cons` prepends, so without it the 32 lines come
  out backwards. The snapshot catches this.
- **`def churned`** — without a read of `scratch`, the static analyser reports
  `Binding 'scratch' is never read`, which is a warning today and an invitation
  to dead-code elimination tomorrow. The test would then pass while allocating
  nothing.

- [ ] **Step 6: Verify against its snapshot**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/gc/generational_churn.mdr 2>&1 \
  | sed 's/\x1b\[[0-9;]*m//g' > /tmp/out.txt
diff <(tr -d '\r' < test/gc/generational_churn.expected) <(tr -d '\r' < /tmp/out.txt)
```

Expected: no output. `ArrayUtil::Reverse` is there because `Cons` prepends — drop
it and the 32 lines come out backwards, which the snapshot will catch.

- [ ] **Step 7: Confirm it did not get cheap**

```bash
for r in 1 2 3; do
  s=$(date +%s%N)
  ./out/build/ninja/x64-development/out/Midori.exe run test/gc/generational_churn.mdr > /dev/null 2>&1
  e=$(date +%s%N); echo "$(( (e-s)/1000000 )) ms"
done
```

Expected: 85-110 ms, the same band as before the change. A large drop means the
allocation load was lost, not that the rewrite was clever — investigate before
continuing.

- [ ] **Step 8: Commit**

```bash
git add test/concurrency/success/gc_stress_arrays.mdr test/concurrency/success/worker_join_index_regression.mdr test/gc/generational_churn.mdr
git commit -m "Migrate the remaining in-place Append call sites"
```

---

## Task 5: Delete the prelude modules

**Files:**
- Delete: `MidoriPrelude/Appendable.mdr`, `MidoriPrelude/Prependable.mdr`, `MidoriPrelude/Extendable.mdr`
- Modify: `MidoriPrelude/ArrayUtil.mdr:5-7`
- Modify: `test/gc/generational_churn.mdr` (the `scratch` allocations)

- [ ] **Step 1: Confirm `generational_churn` no longer imports `Appendable`**

Task 4 Step 5 replaced the whole file, including the `scratch` allocations and
the import. Check it:

```bash
grep -c Appendable test/gc/generational_churn.mdr
```

Expected: `0`. If not, Task 4 Step 5 was applied partially.

- [ ] **Step 2: Delete the three forwarders from `ArrayUtil.mdr`**

Delete these three lines:

```midori
def Append = fn < T >(array: Array < T >, value: T) -> Unit => Appendable::Append(array, value);
def Prepend = fn < T >(array: Array < T >, value: T) -> Unit => Prependable::Prepend(array, value);
def Extend = fn < T >(array: Array < T >, other: Array < T >) -> Unit => Extendable::Extend(array, other);
```

and the three corresponding imports at the top of the file.

- [ ] **Step 3: Delete the three modules**

```bash
git rm MidoriPrelude/Appendable.mdr MidoriPrelude/Prependable.mdr MidoriPrelude/Extendable.mdr
```

- [ ] **Step 4: Sync the installed prelude — trap 4**

```bash
cp -r MidoriPrelude "$LOCALAPPDATA/Midori/MidoriPrelude"
```

Skipping this makes every `<IO>`-importing test fail against the old copy, which
looks like a real regression and is not.

- [ ] **Step 5: Run the full suite**

```bash
./out/build/ninja/x64-development/out/Midori.exe test
```

Expected: **368/368**. Any failure naming `Appendable`, `Prependable` or
`Extendable` is a call site the survey missed — migrate it the same way and note
it, because the survey was done by grep and grep has been wrong three times on
this branch.

- [ ] **Step 6: Commit**

```bash
git add MidoriPrelude/ArrayUtil.mdr test/gc/generational_churn.mdr
git commit -m "Delete Appendable, Prependable and Extendable"
```

---

## Task 6: Delete the FFI functions and the intrinsic opcodes

**Files:**
- Modify: `src/Library/MidoriBuiltinFFIRegistry/MidoriFFIRegistry.h`
- Modify: `src/Library/BuiltinTypes.cpp`, `src/Library/MidoriStdLibExports.h`
- Modify: `src/Compiler/CodeGenerator/CodeGenerator.cpp:3883-3898`
- Modify: `src/Common/Executable/Executable.h`
- Modify: `src/Interpreter/VirtualMachine/VirtualMachine.cpp`
- Modify: `src/Utility/Disassembler/Disassembler.cpp`

- [ ] **Step 1: Remove the intrinsic lowering first**

In `CodeGenerator.cpp`, delete this block:

```cpp
				static const std::optional<size_t> s_array_append_index = MidoriFFIRegistry::FindIndex("MIDORI_FFI_ArrayAppend");
				static const std::optional<size_t> s_text_append_index = MidoriFFIRegistry::FindIndex("MIDORI_FFI_TextAppend");
				if (ffi_index_opt == s_array_append_index)
				{
					EmitByte(OpCode::ARRAY_APPEND, line);
					return;
				}
				if (ffi_index_opt == s_text_append_index)
				{
					EmitByte(OpCode::TEXT_APPEND, line);
```

Read the surrounding `if (ffi_index_opt.has_value() && arity == 2)` — if these
were the only two intrinsics in it, remove the enclosing `if` as well rather
than leaving an empty guard.

- [ ] **Step 2: Build and run the suite before deleting anything else**

```bash
./out/build/ninja/x64-development/out/Midori.exe test
```

Expected: **368/368**. This step proves nothing still reaches those opcodes, so
that a failure in the next step is unambiguous.

- [ ] **Step 3: Remove the five FFI entries**

Delete `MIDORI_FFI_ArrayAppend`, `MIDORI_FFI_ArrayPrepend`,
`MIDORI_FFI_ArrayExtend`, `MIDORI_FFI_TextAppend` and `MIDORI_FFI_TextPrepend`
from `MidoriFFIRegistry.h`, their implementations in `BuiltinTypes.cpp`, and
their declarations in `MidoriStdLibExports.h`.

- [ ] **Step 4: Remove the two opcodes**

Delete `ARRAY_APPEND` and `TEXT_APPEND` from the `OpCode` enum in
`Executable.h`, their `case` arms in `VirtualMachine.cpp`, and their `case` arms
in `Disassembler.cpp`.

The `OpCode` enum has no explicit values — it is positional — so removing two
entries from the middle renumbers every opcode after them and invalidates any
serialised bytecode.

Five `.mbc`/`.mbc.json` artifacts are **tracked in git**, which they should not
be: they are build products from `midori build`, nothing loads them by name, and
after this task their opcode numbering is wrong. Remove them from the index as
part of this commit, listing each path explicitly:

```bash
git rm --cached test/closure/closure_inside_conditional.mbc
git rm --cached test/closure/closure_inside_conditional.mbc.json
git rm --cached test/concurrency/success/channel_spawn_syntax.mbc.json
git rm --cached test/concurrency/success/value_transfer_array_syntax.mbc.json
git rm --cached test/concurrency/success/worker_spawn_syntax.mbc.json
find test -name "*.mbc" -delete
find test -name "*.mbc.json" -delete
```

**Do not assume the rest of the compound-assignment opcode block is dead.**
Compound assignment was deleted from the *language*, but `ADD_ASSIGN_INT` and
`SUB_ASSIGN_INT` are still emitted by an increment peephole at
`CodeGenerator.cpp:508`. Only `ARRAY_APPEND` and `TEXT_APPEND` are in scope here.

- [ ] **Step 5: Build**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

Expected: clean. Redirect to a file and grep for `: error` rather than piping to
`Select-Object -First N`, which closes the pipeline and kills the build partway
through with a misleading "stopped by the user".

- [ ] **Step 6: Run both suites**

```bash
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Expected: **368/368** and **1024 assertions / 176 cases**.

- [ ] **Step 7: Commit**

```bash
git add src/
git commit -m "Delete the append/prepend/extend FFI and intrinsic opcodes"
```

---

## Task 7: Prove the hole is closed

**Files:**
- Create: `test/prelude/failure/in_place_mutation_removed.mdr`
- Create: `test/prelude/failure/in_place_mutation_removed.expected`

- [ ] **Step 1: Write a test asserting the classes are gone**

```midori
module InPlaceMutationRemoved

import
{
	"../../../MidoriPrelude/Appendable.mdr",
}

def arr = [1];
Appendable::Append(arr, 2);
```

- [ ] **Step 2: Run it and confirm it fails on the missing import**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/prelude/failure/in_place_mutation_removed.mdr
```

Expected: a non-zero exit with an error naming the missing file. If it fails for
any other reason, the test is not asserting what it claims.

- [ ] **Step 3: Capture the snapshot**

```bash
./out/build/ninja/x64-development/out/Midori.exe run test/prelude/failure/in_place_mutation_removed.mdr 2>&1 \
  | sed 's/\x1b\[[0-9;]*m//g' > test/prelude/failure/in_place_mutation_removed.expected
```

- [ ] **Step 4: Verify it bites**

```bash
echo CORRUPT >> test/prelude/failure/in_place_mutation_removed.expected
./out/build/ninja/x64-development/out/Midori.exe test prelude
```

Expected: that file FAILS. Regenerate with step 3 and confirm it passes again.

- [ ] **Step 5: Verify Text aliasing is actually fixed**

```midori
module TextAliasCheck

import { "../MidoriPrelude/IO.mdr" }

def a = "core";
def b = a;
def c = a ++ "!";
IO::PrintLine("a=" ++ a);
IO::PrintLine("b=" ++ b);
IO::PrintLine("c=" ++ c);
```

Run it as a scratch file. Expected: `a=core`, `b=core`, `c=core!` — `b` no
longer changes, which was the point of the whole exercise. Delete the scratch
file afterwards.

- [ ] **Step 6: Commit**

```bash
git add test/prelude/failure/in_place_mutation_removed.mdr test/prelude/failure/in_place_mutation_removed.expected
git commit -m "Assert the mutating classes are gone"
```

---

## Task 8: Update the spec and record what it cost

**Files:**
- Modify: `docs/superpowers/specs/2026-09-01-expression-oriented-midori-design.md` §4

- [ ] **Step 1: Tick the forwarders, and add the classes themselves**

§4's "Removed" list already names the three `ArrayUtil` forwarders. It says
nothing about the classes. Add a line under Removed:

```markdown
- `Appendable`, `Prependable`, `Extendable` — in-place mutation. Deleting
  assignment removed the operator; these removed the last expressions that
  write into an object that already exists.
```

- [ ] **Step 2: Add the measured note**

Under the counts table, add:

```markdown
In-place mutation was removed on 2026-09-12. The replacement idioms are a
comprehension or a cons `List` converted once, both linear and both within ~2x
of the in-place loop at 40,000 elements; `ArrayUtil::WithAppended` in a loop is
quadratic and is not the replacement. `test/gc/generational_churn` lost its
old-to-young write-barrier coverage in the process, and nothing replaces it: an
immutable language cannot express that write.
```

- [ ] **Step 3: Commit**

```bash
git add docs/superpowers/specs/2026-09-01-expression-oriented-midori-design.md
git commit -m "Record the removal of in-place mutation in spec section 4"
```

---

## Done when

1. `MidoriPrelude/` contains no `Appendable`, `Prependable` or `Extendable`.
2. No `.mdr` under `test/` or `MidoriPrelude/` calls them.
3. The five FFI functions and the two intrinsic opcodes are gone.
4. `def b = a;` followed by anything cannot change `b`.
5. Suite green at 368/368 plus the new failure test; unit tests 1024/176.
6. Spec §4 names the classes as removed and records the coverage lost.

## Open, and not decided by this plan

- **`Array` is still a mutable primitive underneath.** `ArrayUtil::WithReplaced`
  copies, but the VM's array object is writable and `SET_INDEX`-style opcodes
  remain for the code generator's own use. This plan removes the *source-level*
  ability to mutate, not the representation. Whether `Array` should become a
  persistent structure is a separate question, and the answer probably depends
  on whether anything ever needs an O(log n) update rather than O(n).
- **Comprehensions have no filter clause.** `[x for x in xs if p(x)]` is a parse
  error today, so filtering goes through `List::ListFilter` and a conversion.
  Adding a filter clause would make the comprehension the single obvious way to
  build an array and is worth considering on its own merits.
