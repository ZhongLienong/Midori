# Nominal Newtypes — `type Meters = Int`

**Goal:** `type X = <type>;` introduces a type distinct from `<type>`, with no runtime
representation of its own. `Meters` is an unboxed `Int` at runtime and a separate type
to the type checker, the instance table and the name mangler.

This is the `type` half of spec §3 for the non-record, non-sum case. The `alias` half
(transparent abbreviation) landed in `d53fed1`/`a37ec49` and is not touched here.

---

## Premises corrected before designing

The brief for this work was written against an older tree. Three of its premises are
false on `v2-expression-oriented` at `a37ec49`, all verified by probe:

| Brief asserted | Measured |
|---|---|
| `type X = Y` is transparent, so a raw `Int` passes as `Meters` | `type Meters = Int;` **does not compile**: `Expected ';' after union body`. `ParseTypeDeclaration` (`Parser.cpp:3376`) dispatches `{`→record, else→`ParseUnionBody`. No alias branch remains. |
| `ParseTypeAliasDeclaration` at `Parser.cpp:4015` stores a resolved type under a second name | Renamed `ParseAliasDeclaration` at `:3407`, reached by the `alias` keyword. `alias Meters = Int;` compiles and is transparent — correctly. |
| Baseline 304/304 and 821 assertions / 147 cases | **311/311** and **885 assertions / 153 cases**. Both green. |

The transparency this work was meant to remove has already moved onto `alias`. What is
missing is not nominality-versus-transparency but an **unboxed** nominal type.

## A third route existed, and it decides the design

The brief named two routes. A third already works today:

```
type Meters = Meters(Int);          // single-variant sum
```

Probed, this already satisfies every semantic requirement in the brief:

- rejects the representation — `TakesMeters(raw)` → `Expected type 'Meters' but got 'Int'`
- rejects in the reverse direction
- carries typeclass instances nominally — `instance Describable<Meters>` compiles, and
  the same call on a raw `Int` fails with `no matching concrete instance`. This is
  exactly the `Hashable<Text>` ≠ `Hashable<Array<Byte>>` property spec §5 needs.
- converts both ways through `as` with user `Convertable<Int, Meters>` /
  `Convertable<Meters, Int>` instances

Its only defect is representation, measured rather than assumed: `CONSTRUCT_UNION` is
opcode 159; the wrapper program emits exactly one, the alias program zero. In the VM
that opcode is `AllocateTraceable(MidoriUnion())` plus a `MidoriTuple`
(`VirtualMachine.cpp:2898`) — one GC allocation per value.

So nominality is not the hard part. **Erasure is the whole feature.**

Note this also revises the brief's objection to route 2. `Text` wraps `Array<Byte>`,
which is already heap-allocated, so a struct-based newtype would cost one extra header
per string, not "boxing every string". What disqualifies boxing is the general case: a
`type Meters = Int` that silently allocates is a bad language feature.

## Decision

**An erasing newtype: a new `MidoriTypeUnion` variant, nominal in the front end and
erased in the back end.**

Two facts make this cheap, both verified:

1. **Nominality is already string-keyed.** `InstanceKey` is
   `{class_name, vector<string> concrete_types}` (`TypeChecker.h:60`), and codegen's
   `MangleInstanceMethodName` mangles from `type_arg->ToString()`. A newtype whose
   `ToString` returns its own name gets an independent instance slot and an independent
   mangled method name with no further work. This is also why a transparent `alias` can
   never carry its own instance — it renders as `Int`.

2. **Erasure is safe.** The VM never dispatches on user-level type identity. The only
   runtime tag is `DebugTypeTag`, debug-only, with a single use site in the codebase,
   tagging at primitive granularity (`INT`/`POINTER`), never by user type name. Opcode
   selection is entirely static.

### Blast radius, corrected

The brief estimated "three `std::visit` visitors". Actual: **6** `std::visit` sites over
`MidoriTypeUnion` (3 in `Type.cpp`, 3 in `CodeGenerator.cpp`) and **377** `IsType<>` call
sites — 219 TypeChecker, 126 CodeGenerator, 28 Parser, 3 `Type.cpp`, 1 diagnostic.

That is less alarming than it looks, but the codegen half needs stating precisely rather
than tidily. Of the 126 codegen tests:

| Receiver | Uses | Declarations in the file |
|---|---|---|
| `operand_type` | 45 | 4 |
| `from_type` | 25 | 1 |
| `target_type` | 9 | 1 |
| `type` | 10 | 1 |
| `iter_type` | 1 | 2 |
| `m_concrete_type` / `pattern` / `concrete` | 24 | 0 — members and parameters |
| miscellaneous one-offs | ~12 | — |

The 24 member/parameter receivers were the one part of the estimate not pinned by
measurement. Tracing them **inverted the naive design rule**, so it is recorded here
rather than discovered during implementation:

- `pattern` / `concrete` (15 uses) are the two parameters of a single function,
  `MatchInstanceTypeArg` (`CodeGenerator.cpp:915`) — **instance selection**.
- `m_concrete_type` (9 uses) is one member of one visitor, `DeduceGenericVisitor`
  (`:5579`), constructed at `:5724` — **generic specialisation keying**.

Both must stay **nominal**. Erasing there would make `instance Foo<Int>` match `Meters`,
destroying the entire feature. The `type` local at `:6157` is likewise a recursive
"contains a generic?" helper that must *recurse into* a newtype's representation rather
than erase it.

So the rule is not "nominal in the front end, erased in the back end". It is finer:

> **Opcode selection erases. Dispatch stays nominal.**

Erasure sites are therefore named and individually justified, never applied as a blanket
rule — and notably `GetConcreteTypeForExpression` (`CodeGenerator.h:359`) is *not* a safe
chokepoint despite feeding all four `operand_type` declarations, because its result at
`:2504` also feeds `Convertable` instance lookup, which must stay nominal.

The 219 TypeChecker sites mostly want to *stay* nominal, so they need an audit pass
rather than edits.

---

## Design

### Type representation

A new variant in `MidoriTypeUnion`, shaped like `StructType`/`UnionType` so generics
behave identically across all declaration forms:

```cpp
struct NewType
{
    std::string m_name;
    std::shared_ptr<MidoriType> m_representation;
    std::vector<std::string> m_generic_params;
    std::vector<std::shared_ptr<MidoriType>> m_type_arguments;
    std::vector<ClassConstraint> m_constraints;
    bool m_is_generic_instantiation = false;
};
```

Three visitors in `Type.cpp`:

- **`ToStringVisitor`** → the name plus rendered type arguments. Never the
  representation. This single line is what delivers nominal instances and nominal
  mangling.
- **`TypeEqualityVisitor`** → name equality and type-argument equality. Never compares
  representations. Guarded by the same `thread_local` visiting set `StructType` and
  `UnionType` use, since a newtype can recurse through its representation.
- **`SubstitutionVisitor`** → substitutes into the representation and the type
  arguments, then rebuilds. Must not take an empty-map shortcut (see Traps).

### Surface syntax and disambiguation

`ParseTypeDeclaration` gains a third branch. After `=`:

| RHS begins | Form |
|---|---|
| `{` | record (unchanged) |
| a depth-0 `\|` anywhere before the terminating `;` | sum (unchanged) |
| otherwise | **newtype** — parse the RHS with `ParseType` |

A single-variant sum must therefore carry an explicit leading bar:

```
type Meters  = Int;                  // newtype over Int
type Unit    = | Nothing;            // single nullary variant
type Wrapper = | Some(Int);          // single variant with a payload
type Option<T> = Some(T) | None;     // unchanged, has a depth-0 bar
```

The `|` scan is a bounded lookahead to the depth-0 `;`, tracking `()`, `<>`, `{}` and
`[]` depth — the established `ProbeArrayComprehension` pattern. Leading-bar sums are new
syntax; `SINGLE_BAR` is currently only a separator (`Parser.cpp:3288`).

**Migration cost: zero.** All 8 top-level `type` declarations in `test/` and
`MidoriPrelude/` today have either a `{` or a depth-0 `|`. Measured, not assumed.

**A collision to pin, not to fix.** `type Item = Int;` inside a `class` or `instance`
body is an associated-type binding and is the *identical* syntax to a top-level newtype.
There are 9 such bindings today. They are parsed by separate loops (`Parser.cpp:3544`
class body, `:3820` instance body) that never reach top-level statement dispatch
(`:5534`), so nothing breaks — but the two readings of one syntax now sit side by side,
so a test pins the boundary.

### Statement node — reused, not added

A newtype declaration reuses `MidoriStatement::TypeAlias` unchanged. Verified: every
consumer already does exactly what a newtype wants — `ShadowingPolicyDiagnostic` calls
`DefineType(name)` and pushes generic params, `SemanticFacts::Visit` is empty, and
`CodeGenerator::operator()` returns without emitting. The declaration's only real work
happens in the parser, which registers a `NewType` in the type table instead of the bare
right-hand side.

This is what keeps trap 2 inapplicable: no new AST variant, so no silent `if constexpr`
fall-through in `SemanticFacts.cpp` or `SharedAnalysis.cpp`. The node's name becomes
mildly misleading, which spec §4 already plans to fix by collapsing `Struct`, `Union` and
`TypeAlias` into one `TypeDefinition`.

### Type checking — nominal

A newtype unifies only with itself. It does **not** unify with its representation in
either direction; that rejection is the feature. Constraint solving, instance lookup and
associated-type resolution all see the newtype's own name, so they need no changes —
they already key on `ToString`.

The audit over the 219 TypeChecker `IsType<>` sites asks one question per site: does this
site want the nominal type or the representation? The default and overwhelmingly common
answer is nominal, which is the behaviour a new variant gets for free by not matching.
Sites that genuinely need the representation are the exception and get an explicit
unwrap.

### Code generation — erased

A newtype has no representation of its own. Construction and projection emit **no
instructions**. `CodeGenerator` strips newtypes only at the named opcode-selection sites
identified above — the four `operand_type` declarations (`:2843`, `:2882`, `:3012`,
`:3518`), the `as` built-in-cast path (`:2490`–`:2491`) after `Convertable` lookup has
already been attempted nominally, and `iter_type` — via one helper:

```cpp
const std::shared_ptr<MidoriType>& RepresentationOf(const std::shared_ptr<MidoriType>& type);
```

which walks nested newtypes to a fixed point. Every downstream `IsType<>` test then sees
the representation and selects the same opcode it would for the bare type. A
`type Meters = Int` therefore compiles to the identical bytecode as an `Int`, which is
the acceptance criterion for the feature.

Monomorphisation is unaffected: specialisation keys on mangled names built from
`ToString`, so `Meters` and `Int` produce distinct specialisations even though they share
a representation.

### Conversion

`as` is the vehicle; no new syntax. It already works in both directions with
hand-written `Convertable` instances on a nominal wrapper (probed). For an erasing
newtype both directions are no-ops, so writing them by hand is pure ceremony:
`Convertable<Rep, New>` and `Convertable<New, Rep>` are **derived automatically** at
declaration, each lowering to an identity conversion that emits no instructions.

This keeps one conversion concept in the language rather than adding newtype-specific
construction and projection forms.

---

## Testing

Four `.mdr` suites with `.expected` snapshots, each snapshot verified to bite by
corrupting it, observing the failure, and restoring:

1. **Nominal rejection, forward** — a raw `Int` passed where `Meters` is expected.
2. **Nominal rejection, reverse** — a `Meters` passed where `Int` is expected.
3. **Explicit conversion** — `7 as Meters` and back, round-tripping to the same value.
4. **Typeclass independence** — an instance on `Meters` that does not apply to `Int`,
   mirroring the probed `Describable` case.

Plus two guarding the design decisions rather than the feature:

5. **Associated-type boundary** — `type Item = Int;` in an instance body still binds an
   associated type and does not declare a newtype.
6. **Erasure** — the `--format json` disassembly of a newtype program contains no
   `CONSTRUCT_UNION` (opcode 159) and matches the bare-representation program
   instruction for instruction. This is the only test that proves the feature's point,
   so it is the one that must not be skipped.

Baseline to hold: **311/311** and **885 assertions / 153 cases**.

---

## Out of scope

- Deleting `struct`, `union`, or migrating the prelude's 131 declaration sites.
- Collapsing `Struct`, `Union` and `TypeAlias` into one `TypeDefinition` node (spec §4).
- Making `Text` an actual newtype over `Array<Byte>` (spec §5). This work is the
  mechanism that unblocks it, not the migration itself.
- Newtype-specific deriving beyond the two auto-derived `Convertable` instances.

## Traps

Established in this codebase; do not rediscover.

1. **`SubstituteTypeParams` with an empty map is not the identity** — since `e730762` it
   rebuilds a `StructType` with `m_generic_params` cleared. `NewType` substitution must
   not copy that shortcut.
2. **`Analysis/SemanticFacts.cpp` falls through silently** on unhandled variants rather
   than failing to compile (57 `if constexpr` arms). Read it; do not wait for a compile
   error. `Analysis/SharedAnalysis.cpp` visits AST nodes rather than types, so it is only
   at risk if a statement node is added — this design adds none.
3. **`src/Utility/Formatter/Formatter.cpp`** has three switches ending in `default:` with
   the same hazard. The new syntax must round-trip through `midori fmt`.
4. **Measure, do not reason.** Several plans here asserted premises that survived only
   until someone traced them — including an opcode sequence approved without being
   executed that turned out wrong. Three of this brief's own premises were false.

## Build and test

PowerShell, **not** Git Bash — Git Bash mangles the vcvars64 quoting and hangs.

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Do **not** use `build/out/Midori.exe`, a stale April build. This checkout is shared with
other sessions: run `git status` first and stage by explicit pathspec, never `git add -A`.
