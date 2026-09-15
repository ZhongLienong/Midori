# Language Improvements

Written 2026-07-15, revised the same day after a design review of the language
surface. Revised again 2026-08-30 after checking the claims below against the
implementation; that audit added confirmed item 3 and coherence items 7-8, and
corrected one bullet under decisions taken. Companion to
`concurrency-backlog.md`, which covers scaling work.

## The organizing insight

The design tension in Midori is **not** mutable-versus-immutable. It is that the
**language surface is considerably more functional than the standard library and
the runtime.**

The surface says value-oriented: everything is an expression, Hindley-Milner
inference, ADTs with exhaustive matching, typeclasses with `deriving`, a pipe
operator, array comprehensions, a dedicated `TAIL_CALL` opcode, `Option` and
`Result` in the prelude, a `Never` bottom type, and a worker model built on
deep-copy value semantics.

The library and runtime say reference-oriented: `Append`/`Prepend`/`Extend`
mutate and return `Unit`, `Iterable::Next` is a mutating iterator protocol,
`Map`/`Set` are open-addressed, sixteen compound-assignment tokens exist, arrays
and structs alias, and failure terminates the process instead of producing a
value.

Demonstrated concretely: `arr |> ArrayUtil::Append(4)` fails to compile with
"Expected type 'Array<T0>' but got 'Unit'". The flagship functional feature
cannot reach the standard library.

**Coherence work is therefore mostly library work.** It does not require picking
a side on immutability. OCaml has `ref` and `mutable` and reads as completely
consistent, because its library is honest about which functions mutate.

## Decisions taken (do not re-litigate)

**Full immutability: declined.** Investigated in depth and rejected on the
merits. The language is expressively capable of it — generic folds, 100k-deep
tail recursion, 200k-element immutable lists, and comprehensions were all
verified working. But the benefits did not justify the cost for this project:

- Smaller language surface — the deciding pro, and it did not land: the current
  surface is not felt to be too large.
- Faster closures — real (capture-by-value removes a per-capture allocation and
  an indirection) but performance is not this project's objective.
- Aliasing safety — overstated. Reference semantics plus mutation is the
  mainstream default (Java, Python, JavaScript, C#, Go slices). A hazard, not a
  defect.
- Hash-key drift — real, and understated here as first written. Java and Python
  do not copy keys on insert; their strings are immutable, so the situation never
  arises. Confirmed item 3 closes the drift without reopening this decision.
- Interprocedural optimization — only pays if that optimizer work actually
  happens, and it was already a stretch goal.

Against that: weeks of standard-library rewriting (the `Iterable` protocol,
`Map`, `Set`, `List`, `ArrayUtil`, plus a persistent vector), deletion of a
tested feature (capture-by-reference closures), and one new language form
(`{ c with field = value }`) needed to keep struct updates bearable.

**Mutation stays.** The original motivation — a renderer, where linear algebra
matters — is sound in shape though not in detail: small linear algebra (`Vec3`,
`Mat4`) wants value semantics anyway, and the pixel loop must be native
regardless, where FFI already hands C a zero-copy raw pointer into array storage
(`array_arg.data = &array[0u]`). Mutation is retained because removing it buys
too little here, not because the renderer needs it in-language.

**`:=` for assignment: optional, on taste.** Its strongest justification did not
survive scrutiny. `if x = y` is a *type error* when the operands are not `Bool`,
so the C-style footgun is confined to two `Bool` operands — far narrower than in
C. The cheap fix is a diagnostic, not a syntax change. What remains for `:=` is
"mutation becomes greppable", a readability argument that does not justify a
corpus-wide migration unless wanted for its own sake.

## Confirmed work

**1. `join` returns `Result<T, E>` instead of terminating the joiner.**
Highest value-to-effort change available. Today a worker's failure propagates
into the joining VM and kills it, so the isolation the runtime pays for buys
nothing — you cannot observe a failed worker and carry on. Error codes already
survive the join boundary (commit `868c658`); the remaining work is turning
`JOIN_WORKER` into a value rather than a termination. Roughly an afternoon.

**2. Stop re-executing module initializers per spawn.** *Done 2026-09-14:
workers start from a copy of the spawning VM's globals.*
`InitializeWorkerGlobals` re-runs every non-entry module's initializer inside
each new worker VM, so a module whose top level has an observable effect
performs it again on every spawn. Treat as a correctness bug. Also removes the
spawn-cost entry from `concurrency-backlog.md`.

**3. Make `Text` immutable.**
`Text` is a mutable heap object — `TEXT_APPEND` in
`src/Interpreter/VirtualMachine/VirtualMachine.cpp` appends in place — and it is
also `Hashable` and the primary `Map`/`Set` key. `MapInsert` stores the key by
reference, so mutating a key after insertion strands its entry: it is reachable
under neither the old key nor the new one, while `MapCount` still counts it.
A later rehash re-derives the bucket from the current key and the entry
reappears, so whether a lookup succeeds depends on the map's growth history.
Reproduced end to end.

The precedent cited under decisions taken argues for this change rather than
against it. Java, Python, JavaScript, C#, and Go all pair reference semantics
with an immutable string type, which is why none of them needs a key-copying
rule. Midori is one step past that default, not on it.

Independently, the compiler already assumes `Text` is immutable.
`MidoriAnalysis::ConstantValue` admits `std::string`, so `LocalConstantPropagation`
treats a Text-literal-initialised local as a value constant and rewrites each use
back into a fresh literal. The observable result is that `def t: Text = "hello"`
is not mutable while `def t: Text = Concat("hel", "lo")` is — same declaration
form, same type, different semantics. Details in `text-mutability-soundness.md`.

Cost: delete the `Appendable<Text, Text>` instance, the `MIDORI_FFI_TextAppend`
registry entry, and the `TEXT_APPEND` opcode. The rest of the Text API is already
pure — every `TextUtil` function returns a new `Text`, and so does
`Concatenable::Concat`. Outside `MidoriPrelude/Appendable.mdr` itself,
`TextAppend` appears in three files: `benchmark/comprehensive_benchmarks.mdr`,
`benchmark/text_operations.mdr`, and `test/ffi/success/builtin_text_ffi.mdr`. No
library code uses it. If the benchmarks need amortised appends, add a
`TextBuilder` that is not `Hashable`.

This sits in confirmed work rather than coherence work because it closes a
correctness hole, not an ergonomic one. It is not immutability by default and
does not reopen that decision; it makes one type honest about which of the two
semantics it has.

## Coherence work (library-level, cheap, no language change)

**4. Make the array function names tell the truth.** `Append`, `Prepend`, and
`Extend` mutate and return `Unit`, which is why they cannot be piped. Follow the
Rust/OCaml convention: the plain name returns a new array and composes with
`|>`; an explicitly named in-place variant mutates. This removes the sharpest
visible contradiction in the language.

**5. Write down the worker boundary rule.** `Transferable` currently covers two
opposite semantics with nothing distinguishing them — verified in a single
`spawn` call where an `Array<Int>` argument was snapshotted while a
`Channel<Int>` argument was shared by handle. Both behaviours are correct; only
the vocabulary is missing. Document in `multicore-runtime.md`:

- *Snapshotted at the boundary:* primitives, `Text`, `Array`, `Tuple`, structs,
  unions. The worker receives its own copy.
- *Shared by handle:* `Channel<T>`. Both sides address the same queue. This is
  the deliberate exception, and it is what makes channels the communication
  primitive.
- *Cannot cross:* `Worker<T>`, closures, cells.

Frame workers as processes rather than threads — sending a value to a worker is
a snapshot, like sending it over a socket. That makes the copy obviously correct
instead of surprising.

**6. Add an assignment-in-condition diagnostic.** A sixth static-analyzer pass
alongside unused locals, shadowing, unreachable code, integer overflow, and
capture escape. Catches `if flag = other` where `==` was meant — the residual
hazard after the type checker rejects every non-`Bool` case. This is what GCC
and Clang do, and it costs no language change and no migration.

**7. Guard iterator invalidation on `Map` and `Set`.**
`MapIter` and `SetIter` hold a live reference to their container and re-read
`capacity` and `buckets` on every `Next`, so an insert that trips the 0.7 load
factor rehashes the buckets underneath a running iterator. Measured: iterating an
eight-entry `Map<Int, Int>` while inserting visited 35 entries in a map that ended
holding 28 — duplicates, silently, with no error and no diagnostic. Java, Python,
and C# all detect this and raise. Fix: a `version: Int` field on `Map` and `Set`,
bumped by insert, remove, clear, and rehash, snapshotted into the iterator at
construction and checked in `Next`. Both containers, about an hour. This is the
mutation cost the language actually pays, and it is cheap to make visible.

**8. Decide what `Equatable` means for `Float`.**
`Equatable<Float>` follows IEEE, which is right for `==` and wrong for key
identity, and `Hashable<Float>` then admits `Float` as a `Map`/`Set` key. A map
keyed on NaN accepts unlimited duplicate inserts and retrieves none of them:
three inserts of the same NaN give `MapCount == 3` and `MapContains == false`.
One typeclass is carrying two contracts. Options, cheapest first: normalise NaN
inside `Hashable<Float>` and `Equatable<Float>`; or separate key identity from
`==` the way Rust separates `Eq` from `PartialEq`; or drop `Hashable<Float>` and
make callers key on bit patterns. Unrelated to mutability — item 3 does not touch
it.

## Still open

**9. Decide the error model.** Partially addressed by item 1. Recommended shape,
unchanged: keep panics for programmer errors, add value-returning variants for
expected failures, add an early-return propagation operator for `Result`, and do
not add exceptions. Lacking higher-kinded types is not an obstacle — monomorphic
`Result` combinators work at rank-1.

**10. Make concurrency composable.** `ParallelMap` and friends are listed as
future work in `multicore-runtime.md` but cannot be written in Midori, because
`spawn` requires a named top-level function and closures are never
`Transferable`. A non-capturing function is
`MidoriClosure{ m_cell_values: empty, m_proc_index: N }`, so transferring one
means transferring an integer that the target VM rebuilds with
`MakeFunctionValue(N)` — roughly fifteen lines in `ValueTransfer`. The design
question is the type system: `FunctionType` carries no capture information,
though it already carries `m_is_foreign` as precedent for such a flag. Smaller
first step: permit transfer only of syntactically known top-level function
names, the rule `spawn` already enforces.

## Trigger-gated

- **Module identity separate from file path.** Trigger: the package system and
  path-based imports coming into visible conflict.
- **Bytecode verifier and a written trust model.** Trigger: wanting to
  distribute `.mbc` artifacts. Until then, document that compiled artifacts are
  not safe to run across a trust boundary.

## Anti-goals

- Exceptions and stack unwinding.
- Higher-kinded types.
- Immutability by default, and full immutability — considered and declined
  above. Confirmed item 3 is not a step toward either: it removes a single
  mutating instance from a type whose API is otherwise already pure.
- A borrow checker. Without one, binding-level `mut` conveys nothing at a
  function boundary, which is why mutability would have to live in types if it
  were ever marked at all.
- Green threads or a work-stealing scheduler before a written runtime model.
