# Text Mutability: Two Defects

Written 2026-08-30. Supporting note for confirmed item 3 of
`language-improvements.md`. Both defects share one root cause: `Text` is a
mutable heap object, and two other parts of the system are built on the
assumption that it is not.

## Root cause

`Appendable<Text, Text>` in `MidoriPrelude/Appendable.mdr` maps to
`MIDORI_FFI_TextAppend`, which the code generator lowers to a dedicated
`TEXT_APPEND` opcode. The handler in
`src/Interpreter/VirtualMachine/VirtualMachine.cpp` mutates the target in place:

    container.GetPointer()->GetTraceable<MidoriText>().Append(...)

Nothing else in the Text surface mutates. Every `TextUtil` function returns a
new `Text`, and `Concatenable::Concat` returns a new `Text`. `Appendable` is the
sole exception.

## Defect 1: mutated keys strand Map and Set entries

`MapInsert` stores the key by reference (`MidoriPrelude/Collections/Map.mdr`);
there is no copy. Mutating a key after insertion leaves the entry in the bucket
derived from the old hash while holding the new key value, so it matches neither
probe.

    def key : Text = "Ali";
    def m : Map::Map<Text, Int> = Map::MapNew();
    Map::MapInsert(m, key, 25);
    Appendable::Append(key, "ce");

    Map::MapContains(m, "Ali")     -> false
    Map::MapContains(m, "Alice")   -> false
    Map::MapCount(m)               -> 1

The slot is not reclaimable and the count is wrong. It is also not stable:
`MapRehash` re-derives every bucket from the current key, so after enough inserts
to trip the 0.7 load factor the entry becomes findable again.

    (twelve further inserts, capacity grows past 16)
    Map::MapContains(m, "Alice")   -> true

Whether a lookup succeeds therefore depends on the map's growth history rather
than on the program's own sequence of operations.

`Set` has the identical structure — same `OpenAddressing` probing, same
store-by-reference, same rehash-on-insert — and the same defect.

## Defect 2: constant propagation erases Text mutation

`MidoriAnalysis::ConstantValue` in `src/Compiler/Analysis/SemanticFacts.h`
includes `std::string` in its variant, and `LiteralForm` includes `Text`. So
`LocalConstantPropagation::TryCreateReplacement` accepts a Text literal as a
value constant, and `MaterializeReplacement` rewrites every subsequent use of
that local into a freshly constructed literal expression. Identity is discarded,
and with it any in-place mutation.

The pass is sound for `Int`, `Float`, `Byte`, `Word`, `Bool`, and `Unit`, which
are genuinely values. It is unsound for `Text` alone.

Observable consequence — four functions, one type, two semantics:

    def t : Text = "hello";              Append(t, "!")  ->  "hello"
    def t : Text = Concat("hel", "lo");  Append(t, "!")  ->  "hello!"
    def t : Text = s;   (s a parameter)  Append(t, "!")  ->  "hello!"

Aliasing is erased too: given `def a : Text = "hello"; def b : Text = a;`,
appending to `a` leaves `b` reading `"hello"`.

## Fix

Making `Text` immutable resolves both at once and needs no compensating work:

1. Delete the `Appendable<Text, Text>` instance from `MidoriPrelude/Appendable.mdr`.
2. Delete the `MIDORI_FFI_TextAppend` entry from `MidoriFFIRegistry.h` and its
   `TEXT_APPEND` lowering in `CodeGenerator.cpp`.
3. Delete the `TEXT_APPEND` opcode, its VM handler, and its disassembler case.
4. Update the three call sites: `benchmark/comprehensive_benchmarks.mdr`,
   `benchmark/text_operations.mdr`, `test/ffi/success/builtin_text_ffi.mdr`.
5. If the benchmarks need amortised appends, add a `TextBuilder` with a
   `Build() -> Text`. It must not have a `Hashable` instance.

After that, `ConstantValue` admitting `std::string` becomes correct rather than
accidental, and the `Map`/`Set` key contract holds for every `Hashable` type
except `Float`, which is a separate problem tracked as coherence item 8 in
`language-improvements.md`.

## Rejected alternatives

- **Copy Text keys on insert.** Costs an allocation per insert, does not fix
  `Set` without duplicating the change, does not fix defect 2, and does not help
  any other mutable-and-hashable type. It is also not what Java or Python do.
- **Forbid mutable types from instancing `Hashable`.** `Text` is the essential
  key type, so this is only viable once `Text` is immutable, at which point it is
  redundant.
- **Make `LocalConstantPropagation` skip `Text`.** Fixes defect 2 only, and
  fixes it by preserving the behaviour that causes defect 1.
