# Expression-Oriented Midori — Design

Written 2026-09-01. Supersedes the immutability sections of
`docs/plan/language-improvements.md`; companion to
`docs/plan/text-mutability-soundness.md`.

**Status:** design settled, ready for an implementation plan.

**Delivery:** breaking v2 on branch `v2-expression-oriented`, cut from
`gc-overhaul` rather than `main` — the immutable design allocates far more than
the current one, so the generational collector is the correct base for measuring
it.

## 1. Goal

Midori should be **expression-oriented and piping-oriented in character**. That
is the plan's own organizing insight, and it is the target — not immutability.
Immutability is instrumental: it is what falls out once no construct exists
purely for effect.

The diagnostic symptom is the unit literal. `ArrayUtil::Slice` spends sixteen
lines and eight `()` values on four clamps, because assignment produces nothing
worth having.

## 2. Constraints

1. **Expression-orientation.** Every construct evaluates to something worth
   having; no function returns `Unit` outside the IO edge.
2. **Semantic uniqueness.** One concept, one form. Delete duplication that is
   *mechanical* — same lowering, same reading. Keep distinctions that carry
   different intent: `if` versus `match` survives, `defun` versus `def = fn`
   does not.
3. **Minimal built-ins.** Every operator desugars to exactly one typeclass
   method. The compiler's knowledge of specific library entities is one declared
   table, not scattered special cases.
4. **C-family surface.** Midori was designed with JavaScript and TypeScript in
   mind. A construct that is semantically right but reads wrong in a C-family
   grammar is wrong. This settled the iteration question against an otherwise
   defensible Clojure-style `loop`.

## 3. Concepts and forms

| Concept | Form |
|---|---|
| Value binding, functions included | `def x = e` |
| Anonymous function | `fn<T>(x: T) -> R where C<T> => e` |
| Introduce a distinct record | `type Point = { x: Int, y: Int }` |
| Introduce a distinct sum | `type Option<T> = Some(T) \| None` |
| Introduce a distinct type over another | `type Meters = Int` |
| Abbreviate a type expression | `alias IntMap<V> = Map<Int, V>` |
| Record update | `{ s with f = v, g = w }` |
| Consume an iterable | `for x in it { }` |
| Transform a sequence | `Iter::*` through the pipe operator |
| Materialize a sequence | `[e for x in it]` |
| Stateful loop | a named recursive helper |
| Failure propagation | `Result::AndThen` |
| Size | `#x` resolving to `Countable` |
| Index | `x[i]` resolving to `Indexable` |
| Concatenation | `a ++ b` resolving to `Concatenable` |
| Cast | `x as T` resolving to `Convertable` |

### Separators — one job each

| Separator | Its one job |
|---|---|
| `:` | ascribe a type to a **name** |
| `->` | a function's **result** |
| `=>` | a function's **body** |
| `=` | bind a **name** to a value or type |

A definition is therefore its own type with names and a body added:

```
def Append = fn<T>(array: Array<T>, value: T) -> Array<T> => array ++ [value];
```

## 4. Grammar delta

### Removed

- `defun` — an immutable `def` can be committed to a proc index, which is
  precisely what mutability prevented.
- `return` — tail position duplicates `=> e`; early exit is covered by branch
  structure, combinators, and `AndThen`.
- Assignment and compound assignment — 11 tokens, 4 AST nodes.
- `loop`, `break`, `continue` — a stateful loop is a named recursive helper,
  which is the idiomatic JS/TS spelling and an expression naturally.
- `struct`, `union` — folded into `type`.
- `new` — constructors are ordinary functions.
- `default` — a `_` wildcard pattern, which also nests.
- `spawn`, `join`, `channel` — ordinary functions over library structs.
- `true`, `false` — union constructors, subject to section 8.
- The `#` name-suffix dispatch in `CodeGenerator.cpp:3330-3370`.
- Six redundant prelude names: `MapSize`, `SetSize`, `ArrayUtil::Length`,
  `TextUtil::Length`, and the `ArrayUtil::Append` / `Prepend` / `Extend`
  forwarders.

### Added

- Type parameters and `where` clauses on `fn`.
- `->` in every return position.
- `{ s with f = v, g = w }` — simultaneous, so right-hand sides evaluate against
  the original record; duplicate fields are an error; no nested paths.
- `alias` for transparent type abbreviation.
- Constrained instances, `instance C<T> where D<T>` — **not parsed today**;
  `where` exists for functions, structs and unions only.
- `Indexable`, and operator binding at the class declaration:
  `infixl 5 (++) Concat : fn(T, T) -> T`.
- Pattern guards — `case Ok(n) if n > 100 =>`. Not sugar for a nested `if`: a
  failed guard falls through to the next case, which a nested `if` cannot do.
- `Iterable::Next : fn(Iter) -> Option<(Item, Iter)>`.

### Counts

| | before | target | **actual, 2026-09-06** |
|---|---|---|---|
| Expression nodes | 41 | 18 | **42** |
| Statement nodes | 11 | 5 | **11** |
| Built-in type kinds | 17 | 8 | **17** |
| Reserved words | 37 | ~25 | **34** |
| Callable concepts | 7 | 1 | **7** |

Statements remaining: `ExpressionStatement`, `VariableDefinition`,
`TypeDefinition`, `Class`, `Instance`. Only the first two appear inside a block.

**Measured status — read this before trusting the target column.** Five keyword
deletions have landed and are verified green (`defun` `4065330`, `struct`/`union`
`565beae`, `new` `cba4b3d`, `:` in return position `ca0572e`). Exactly one row
moved: reserved words, 37 → 34, and that is net of `alias` being *added*.

Every structural row is untouched, and the expression count went **up** by one
when `RecordUpdate` landed. This is not a shortfall in execution — the deletions
removed *spellings*, which is all a keyword deletion can remove. The nodes behind
them survive: `struct` and `union` are gone as syntax while `Struct`, `Union` and
`TypeAlias` remain three distinct statement nodes, and `new` is gone as syntax
while `Construct` remains a distinct expression node.

Two rows also need their targets corrected rather than merely being unmet:

- **Callable concepts 7 → 1 is not reachable under the design as chosen**, and
  §7b already records why without drawing the conclusion. A construction cannot
  appear in a pipeline and a bare constructor name is a deliberate diagnostic,
  because a constructor is monomorphised per site and has no single procedure to
  pass around. So constructors are unified with functions *at the surface* —
  one spelling, `Name(args)` — and remain a separate concept underneath. The
  honest target is **1 surface form, 2 concepts**. Restoring the literal 7 → 1
  would require erasure or reified type arguments, which §8 has not opened.
- **Built-in type kinds 17 → 8** is gated on the library rewrite, which is gated
  on the §13 `Iter` decision. It cannot move before that.

Statement nodes 11 → 5 is the one structural row that is both reachable and
unblocked today, and it is a pure refactor with no user-visible effect.

`def` is not itself an expression and should not be — a binding's value is the
scope it opens, not the thing bound. The containing block is the expression.

## 5. Types

Immutable throughout. `Array<T>` stays flat for O(1) indexing and cheap FFI
reads. `Map` and `Set` become HAMTs with their own node type rather than being
rebuilt on `Array`.

`Text` is immutable — a nominal newtype over `Array<Byte>`, not a transparent
alias, so `Hashable<Text>` does not collapse into `Hashable<Array<Byte>>` and
make every byte array a valid map key.

**The mechanism exists as of 2026-09-02.** This section previously named the
requirement without giving it a form — "newtype" appeared once in the whole
document and never as a declaration. It is `type X = Y`, the same keyword as the
record and sum forms, on the principle that `type` always introduces a distinct
type. Verified nominal (`Expected type 'Meters' but got 'Int'`), erased at opcode
selection so it costs nothing at runtime, and carrying typeclass instances
independently of its representation — which is what `Text` actually needs.

There is **no mutable type**. Building is a runtime concern behind comprehensions
and folds; FFI mutation lives outside the language behind opaque foreign handles,
the same pattern `Channel<T>` and `Worker<T>` already use.

`Cell<T>` is designed but **deliberately not shipped**. Build the language and
rewrite the prelude without it. If the prelude never needs one, the language does
not; if it needs three, those three define its shape.

Reducible to library types: `Range`, `Never` as a zero-variant union, `Unit` as
the 0-tuple, `Worker`, and `Channel`.

## 6. Lang items

The compiler's knowledge of the library is one declared table, not scattered
special cases:

| Item | Needed by |
|---|---|
| `Bool`, `True`, `False` | `if` |
| `Int`, `Float`, `Byte`, `Word`, `Text`, `Array` | literal types |
| `Iterable` | `for … in`, comprehensions |
| `Equatable`, `Hashable`, `Transferable` | `deriving` |

Operators need no lang items once symbols bind at class declarations.

## 7. Irreducible semantics

Four categories remain:

1. **Binding** — `def` introduces a name; not a call.
2. **Function** — everything now called constructor, operator, method, cast,
   index, or concurrency primitive.
3. **Pattern** — destructuring is not application. A constructor name spans
   categories 2 and 3: a call in expression position, a destructure in pattern
   position. One concept from both directions, as in ML, Haskell and Rust.
4. **Lazy control flow** — `if`, with `&&` and `||` as desugarings over it
   (`a && b` is `if a then b else False`). Function application evaluates its
   arguments, so these cannot be library functions.

---

## 7b. Additive grammar — complete 2026-09-02

Every form the redesign adds is in, and every deletion it enables is now unblocked.
Nothing has been removed yet; `defun`, `new`, `struct`, `union`, assignment and the
`:` return separator all still work.

| Form | Status |
|---|---|
| `fn<T>(x: T) -> R where C<T> => e` | in — top-level `def`, capture-free, matching `defun`'s envelope |
| `{ s with f = v, g = w }` | in — simultaneous, duplicate fields an error, no nested paths |
| `type` records, sums and newtypes; `alias` | in — newtypes nominal and erased at opcode selection |
| `case P if cond => e` | in — a guarded arm does not count toward exhaustiveness |
| `->` in return position | in — 831 sites migrated across 260 files |
| `Point(1, 2)` without `new` | in — same `Construct` node, byte-identical bytecode |
| `def m = fn(...) -> R => e` in an instance body | in — same `FunctionDefinition` node, byte-identical bytecode |

**Suite 350/350, unit tests 982 assertions / 174 cases.**

### Instance bodies, 2026-09-04

The instance body was the last place `defun` was the only spelling, so it was the
last thing blocking `defun`'s deletion. `defun m(...)` and `def m = fn(...)` now
share one parse: the dispatch consumes either keyword, then the method name, then
`= fn` for the binding form, and both continue into the same code, so the two
spellings build the identical `FunctionDefinition`. An instance method is marked
by name mangling (`MangleInstanceMethodName`), not by a flag on the node, and the
mangling happens after the loop over collected methods — so the type checker's
signature match against the class declaration, the `where`-constraint append from
`736af40`, and code generation are all reached unchanged.

**Generic instance methods are not reachable in either spelling.** `fn<U>(...)`
parses, then the type checker rejects it with "instance methods cannot declare
generic parameters" — the same error `defun m<U>(...)` has always produced. This
is not a gap the binding form introduces, and it does not block migration: of the
instance-method `defun`s in tracked `.mdr` files, **none** declares a type
parameter.

### Two things this settled that the design had left open

**Newtypes had no declaration form.** Section 5 required `Text` to be a nominal
newtype over `Array<Byte>` but never said how one is written — "newtype" appeared
once in the whole document, as a consequence. It is `type X = Y`, on the principle
that `type` always introduces a distinct type.

**`new X<T>(...)` was the only explicit type-argument syntax in the language.**
`Id<Int>(3)` on an ordinary call is a parse error. So deleting `new` would have
removed the only escape hatch for a case that needed one — building a generic
struct inside a generic function. That is why the over-strict `HasTypeVariables`
guard was relaxed first (`7008c5e`): the escape hatch is no longer needed, and
`new` became deletable as a result. The ordering mattered.

### Known gaps, tracked separately

- **Constructions cannot appear in pipelines.** `x |> Point(1)` fails with an arity
  error, and `x |> new Point(1)` fails identically, so this is pre-existing rather
  than new. `ParsePipe` prepends the piped value only when its right-hand side is a
  `Call`, and a construction is a `Construct`.
- **A bare constructor name is a diagnostic, deliberately.** `Apply(Point, 1, 2)`
  reports that a constructor is monomorphised at each construction site and has no
  single procedure to pass around. Same wall as generic functions as values.
- Parameterised aliases, generic newtypes as values, and an expected-type leak
  through `MemberAccess` into `Construct` each have their own task.

---

## 7c. Corpus migration — 2026-09-02

Four mechanical rewrites landed, each its own commit, each green on both suites,
with no compiler source touched:

| Commit | Rewrite | Sites |
|---|---|---|
| `0b924b3` | `: R =>` becomes `-> R =>` | 831 across 260 files |
| `b43de3d` | `defun Name(...)` becomes `def Name = fn(...)` | 802 |
| `30831f6` | construction without `new` | 616 |
| `463af7c` | the remaining `defun`s, unblocked by `84f6e81` | 48 |
| `9e45aa2` | `struct`/`union` become `type` | 172 |

### `defun` cannot be deleted yet — the two forms are not equivalent

The migration was expected to be mechanical. It found three places where
`defun Name(...)` and `def Name = fn(...)` genuinely diverge in the compiler:

1. **A segfault.** `test/concurrency/success/worker_join_index_regression.mdr`
   prints `ok` and exits 0. Change only its `defun Run() -> Unit` to
   `def Run = fn() -> Unit` and it exits **139** with no output. Confirmed on the
   real file. A hand-minimised three-line version does **not** reproduce, so the
   trigger is something the full file has and the reduction lost.
2. **Missing tail-call optimisation.** A self-recursive tail call runs to 10,000
   frames under `defun` and overflows at roughly 2,499 under `def = fn`. Tail
   calls are load-bearing and verified elsewhere to a million frames, so the
   migration would silently change complexity.
3. **A type-checker gap.** A block whose tail statement is unreachable satisfies
   the return type under `defun` and does not under `def = fn`. This one may become
   moot when `return` is deleted.

All three smell like one root cause — the `def = fn` path missing a treatment the
`FunctionDefinition` path gets. `ClosureLifting` rewrites capture-free lambdas into
global `FunctionDefinition`s but deliberately skips generic ones, so *whether a
lambda is lifted* is the likeliest discriminator.

Four `defun` sites in three files are deliberately left unmigrated as live
reproductions. Tracked separately.

### Deliberately pinned, not oversights

Fourteen `defun` and thirteen `struct`/`union` occurrences remain in tests that
exist to pin an old form still parsing, each paired with its new-form equivalent.
Two `.expected` snapshots needed caret columns shifted, since `type` is one
character wider than `union` and two wider than `struct`.

---

## 7d. Deletions — 2026-09-02

| Keyword | Status | Commit |
|---|---|---|
| `defun` | **deleted** | `4065330` |
| `struct` | **deleted** | `565beae` |
| `union` | **deleted** | `565beae` |
| `new` | **deleted** | `cba4b3d` — the inference gap below was closed first by `a284fd3` |
| `:` in return position | **deleted** | `c2077d4` (corpus), this commit (parser) |
| assignment | not started | — |

Suite **367/367**, unit tests **1024 assertions / 176 cases**. Each removed keyword
now gives a diagnostic naming its replacement rather than falling through to a bare
`Undefined name.` — which is what `defun` did on first attempt, and is worth
building in from the start for the remaining deletions.

### `new` was blocked on inference, not on migration

Fifty-two construction sites could not migrate, several in the prelude
(`Collections/Map.mdr:47`, `Set.mdr:38`, `Prelude/List.mdr:54`). Every one is the
same shape — a generic construction in **argument** position:

```
Appendable::Append(buckets, Slot::Empty())
   -> could not infer all type arguments for 'Slot'
```

Written `new Slot::Empty<K, V>()` it compiles. Since `new X<T>(...)` is the
language's only explicit-type-argument syntax — `Id<Int>(3)` on an ordinary call is
a parse error — deleting `new` removes the escape hatch these sites depend on.

The information is available: `buckets : Array<Slot<K,V>>` is known and
`Append`'s second parameter is its element type. The expected type simply does not
reach the `Construct` checker. A likely complication is that `Append` is a
typeclass method, so its parameter types may only become concrete *after* the
argument is checked — an ordering problem rather than a missing-information one.
Tracked separately.

### Three divergences found by migrating, not by design

The `defun` migration was expected to be mechanical. It surfaced three places where
`defun` and `def = fn` behaved differently, all since fixed:

- a **segfault** whose root cause was tokens moving in memory when `>>` is split
  during generic parsing (`d228563`) — nothing to do with lambdas, which is why a
  hand-minimised repro did not reproduce
- **missing tail-call optimisation** in a `def`-bound lambda: 2,499 frames instead
  of unbounded (`0ad4e26`)
- `return` validated against the wrong enclosing scope (`2b92f96`)

### Two things worth carrying forward

**Keyword deletion reaches further than the compiler.** `defun` lived in
`ProjectManifest.cpp`, where `midori init` scaffolds a new project — a newly
created project would have contained a keyword the compiler rejects. Grep, do not
work from a list.

**The installed prelude is a separate copy.** At
`%LOCALAPPDATA%\Midori\MidoriPrelude`, it was six months stale and twenty
system-import tests failed the moment `defun` died. It is current now, but nothing
surfaced the drift until a deletion forced it.

## 8. Open calls — pin while writing, not blockers

- **`Bool` as a library union.** Deletes four things for the price of one
  hardcoded type reference in `if`. Recommended.
- **Arithmetic and bitwise via typeclasses.** Safe only with hard literal
  defaulting — integer literals always `Int`, float literals always `Float` —
  since the type checker has no generalization.
- **Annotation policy.** Recommended: required on module-level `def`s, optional
  inside. **Evidence from the implementation, 2026-09-02:** an exported
  `def N : Int = 42` imports correctly while `def N = 42` fails with
  `variable not found`. The compiler already effectively requires the annotation
  for exports; making it a stated rule turns a confusing lookup failure into a
  clear diagnostic. Not a blocker for the prelude migration — the prelude exports
  only functions.
- **Operator precedence.** Declared (`infixl 5`) versus requiring parentheses for
  mixed operators. The second deletes a feature and a class of bugs.
- **Named arguments.** Positional-only construction is awkward at five or six
  fields. Either named arguments on calls generally, or lean on
  `{ Config::Default with … }`.

## 9. Declined

- **Monads.** `Bind` abstracts over a type constructor applied to different
  arguments — higher-kinded by definition. Per-type `AndThen` is what Rust ships.
- **The `?` operator.** `Result::ResultBind` already exists in the prelude and
  wins on every stated goal. Revisit only if writing a parser produces the
  nesting pyramid in practice — the compiler is a parser, so this gets tested
  early.
- **ML-style functors.** A second abstraction mechanism beside typeclasses.
- **Let-generalization.** Immutability makes it safe, but top-level annotations
  are required anyway, so it buys little.
- **Exceptions, higher-kinded types, a borrow checker, laziness.** Unchanged
  anti-goals.

## 10. Prerequisites that survive unchanged

- **Plan item 1** — `join` returning `Result<T, E>` rather than killing the
  joiner. Highest value-to-effort change available.
- **Plan item 2** — module initializers re-running inside every spawned VM. A
  correctness bug.

Neither touches the grammar; both can land independently.

## 11. Migration scope

| | files | lines |
|---|---|---|
| Prelude | 24 | 1,218 |
| Tests and benchmarks | 286 | 9,594 |

- **Mechanical, about 2,075 sites:** 1,339 `defun`, 635 `new`, 97
  `struct` / `union`, 4 `return`, and every `): T =>` becoming `) -> T =>`.
- **Needs thought:** 498 assignment statements and 52 `loop` blocks. Most are a
  counter, an accumulator, or an append-in-a-loop, each mapping to a
  comprehension, a fold, or a recursive helper.
- **Genuine redesign, about 6 files:** `Map.mdr`, `Set.mdr` and
  `OpenAddressing.mdr` to HAMTs; the `Iterable.mdr` signature; `ArrayUtil.mdr`
  and `List.mdr` to value-returning; and a new `Iter.mdr`.

## 12. Verification

The rewrite is done when:

1. The prelude compiles with no `defun`, `return`, `loop`, `break`, `continue`,
   `new`, `struct`, `union`, assignment, or compound assignment.
2. `arr |> ArrayUtil::Append(4) |> ArrayUtil::Reverse` compiles.
3. The audited defects are inexpressible: a mutated `Text` key and iterator
   invalidation. `Map<Float, _>` NaN keys are addressed separately, since
   immutability does not make NaN reflexive.
4. `#` resolves only through `Countable`, and `HasNameSuffix` is gone from
   `CodeGenerator.cpp`.
5. `ParallelMap` is writable in-language.
6. The existing test suite passes, migrated.

---

---

## 12b. Constraint discovered 2026-09-02: one-parameter classes need homogeneous methods

`CodeGenerator::ResolveConcreteTypeclassMethodName` (`CodeGenerator.cpp:5778-5788`)
special-cases a class with exactly one type argument by matching that argument
against **every** actual argument:

```cpp
if (candidate_args.size() == 1u)
{
    for (const std::shared_ptr<MidoriType>& actual_arg_type : actual_arg_types)
    {
        if (!MatchInstanceTypeArg(candidate_args[0u], actual_arg_type, substitutions, visited))
```

So a one-parameter class whose method takes arguments of *different* types cannot
dispatch. `Get(container: C, index: Int)` offers `[Bag<Int>, Int]`; `Bag<T>` matches
the first position and fails the second, and no candidate survives.

Every existing one-parameter class dodges this because its methods are homogeneous:
`Countable::Count : fn(T) -> Int` takes one argument, and `Concatenable::Concat`,
`Equatable::Equals` and `Orderable` all take arguments of the same type. Isolated by
probe: a one-parameter class with a heterogeneous method fails, and the identical
class with a homogeneous method works — so it is the arity mismatch, not the
associated type.

**Consequence for the operator work.** `Indexable` must be `class Indexable<C, I>`
rather than `class Indexable<C>`. That is independently the better design — it keeps
`Map<K,V>` open to `m[key]`, and `Array` open to a future `Range` index — but it is
worth knowing the one-parameter form is not merely worse, it does not compile.
Two-parameter classes with an associated type are supported on every path:
`FindMatchingInstance` and `ResolveInstanceNameForTypeArgs` are both N-ary, and
`Convertable<From, To>` has fifteen instances dispatching daily.

This is **not** section 13's functional-dependency hazard. There, the second
parameter is supposed to be determined by the first and nothing enforces it.
`Indexable<C, I>` is genuinely two-dimensional, so there is no dependency to
violate.

**Latent, dormant:** the tiebreaker at `CodeGenerator.cpp:5917-5931` treats
`m_second_type_name` as the *return* type. Correct for `Convertable<From, To>`,
meaningless for `Indexable<C, I>` where the second parameter is the index. It only
fires when more than one candidate matches, which the type checker rejects first
with a clean ambiguity error — so it is unreachable today, but it is a wrong
assumption sitting under new code.


## 13. Blocker discovered 2026-09-01: `Iter` needs associated-type equality constraints

Building the constrained-instance machinery surfaced a gap that blocks the
library rewrite in section 11, and it is a **language** gap, not a compiler bug.

A combinator that *transforms* element types cannot be written. Given:

```
class Stepper<S> {
    type Item;
    Step: fn(state: S) -> Option<Item>;
};

instance Stepper<Doubled<S>> where Stepper<S> {
    type Item = Int;
    defun Step(state: Doubled<S>) : Option<Int> =>
        match Stepper::Step(state.inner) with
            case Option::Some(v) => new Option::Some<Int>(v * 2)
            ...
};
```

the inner call returns `Option<Stepper::Item<S>>` with `S` abstract. `type Item = Int`
declares the *wrapper's* item type and says nothing about the inner one, so `v * 2`
requires `Stepper::Item<S> ~ Int` — an equality constraint the grammar cannot
express. `Parser::ParseClassConstraints` (`Parser.cpp:5434`) accepts only
`Identifier '<' Type,… '>'`. Haskell rejects the identical program without
`Item s ~ Int`.

Established by four probes, so this is measured rather than inferred:

| Probe | Shape | Result |
|---|---|---|
| P1 | constrained instance + associated type, wrapper body returns a constant | passes |
| P2 | constrained instance + inner `Step` call, no associated type | passes |
| P3 | full shape, `type Item = Stepper::Item<S>`, pass-through body | passes |
| P4 | the failing test with the use site deleted | one error, not two |

P4 shows the second error is a cascade. P3 shows `FindMatchingInstance`
(`TypeChecker.cpp:1191`) handles constrained instances with associated types
correctly, including a wrapper whose `Item` is itself a projection. **The
associated-type path has no gap** — the program was under-constrained.

`Iter::Map` and `Iter::Filter` are exactly this shape. There are **three**
options, not two, and the choice must be made **before** the library is written.

**A. Add equality constraints** — `where Stepper::Item<S> ~ Int`. A production in
`ParseClassConstraints` (`Parser.cpp:5434`, currently `Identifier '<' Type,… '>'`
only), a widened `ClassConstraint`, and seeding the equations so a projection
resolves while `S` is still abstract. The *resolution* half already exists:
`ResolveAssociatedType` (`TypeChecker.cpp:1221`) already reduces
`Stepper::Item<S>` once `S` is known. Keeps `Item` a function of `S`, which is
what lets inference work at use sites, and leaves dispatch untouched.
**Recommended.**

**B. Promote `Item` onto the wrapper struct** — `Map<S, A, B>` with
`type Item = B`. **This does not work.** It fixes only the *output* side. The body
still calls `Stepper::Step(state.inner)`, typed `Option<Stepper::Item<S>>` with
`S` abstract, and feeds it to `f : fn(A) -> B` — which needs
`Stepper::Item<S> ~ A`, the identical equality constraint under a different name.
The failure is on the *input* side, and a wrapper's own parameters can never
describe it, because the inner `Item` belongs to `S`, not to the wrapper.

**C. Promote the element type onto the class** — `class Stepper<S, A>`, no
associated type. Expressible today with zero language work: inside
`instance Stepper<Map<S,A,B>, B> where Stepper<S, A>` the inner call resolves to
`Option<A>` through the active constraint, no projection needed. Costs an extra
parameter on every signature mentioning a stepper, and silently relies on a
functional dependency `S -> A` that nothing enforces — dispatch only appears to
honour it because it matches on the first type argument. Two instances differing
only in `A` would become ambiguous with a confusing message. If this route is
taken, that fundep must be documented before anyone writes a second instance.

### The boundary, stated sharply

Promotion suffices only for combinators that **produce** elements and never
consume an inner stepper's: `Repeat`, `Empty`, `FromArray`. Every combinator the
library actually needs — `Map`, `Filter`, `Take`, `Zip`, `Enumerate`, `Fold` —
consumes them and hits the same wall.

`test/typeclass/success/constrained_instance_assoc_type.mdr` sits exactly on that
boundary: a pure pass-through, where the wrapper's `Item` is literally
`Stepper::Item<S>` and nothing is consumed at a known type, compiles and runs
today. Add `* 2` to the body and it does not.

This does not affect the settled decisions in sections 1–10. It sits squarely on
section 11's library rewrite and on the `Iterable` signature change in section 4.

### Resolved — option A implemented 2026-09-06, `62c1b5a`..`8033dd2`

`where Stepper::Item<S> ~ Int` parses, constrains and discharges. The program
above — the one with `* 2` — compiles and runs, and `Map` composed over `Filter`
resolves the constraint through two layers. Suite **371/371**, unit tests
**1024 assertions / 176 cases**. Plan and full findings:
`docs/superpowers/plans/2026-09-06-equality-constraints.md`.

Three things the design work did not anticipate, all found by probe:

- **Reduction belongs in `ResolveAssociatedType`, not `Unify`.** `Unify` was the
  obvious site and had no `AssociatedType` case at all, which made it look like
  the gap. But it only serves consumers that unify: `v * 2` checks numeric-ness
  directly and still failed. `ResolveAssociatedType` is the chokepoint
  `ApplySubstitution` already calls, so reducing there reaches every consumer. The
  `Unify` branch was written first, then measured to be dead and removed.
- **Widening `ClassConstraint` in place cost more than the site count suggested.**
  The plan predicted equality constraints reaching code that assumes a class name.
  What it missed is that **eighteen** sites *rebuild* a constraint — four
  substitutions, seven copies, six field-by-field reconstructions in `Freshen` and
  `ApplySubstitution`, and one arity check — each silently producing a class-kind
  constraint with an empty name. The failure mode was not a wrong answer but
  `m_classes.at("")` throwing `std::out_of_range`, which MSVC turns into
  `__fastfail` and Windows reports as `STATUS_STACK_BUFFER_OVERRUN` (`0xC0000409`).
  That reads exactly like a stack overflow and is not one.
- **A non-projection left-hand side cannot get a specific message.**
  `ParseDelimitedZeroOrMoreUnlimited` swallows a failed element and returns the
  accumulated list, which is inherent to zero-or-more. `where T ~ Int` is still
  rejected, by the caller's generic message. Surfacing the specific one needs
  sticky parser state that `TryParser` does not restore.

**What this unblocks:** section 11's library rewrite, and behind it the removal of
assignment — the last deletion and the only semantic one.

**Adjacent gap found, not fixed.** A state-threading stepper wants
`Step : fn(S) -> Option<(Item, S)>`, but `Option::Some((a, b))` currently collapses
to a two-argument call, so a tuple payload cannot be constructed. Pre-existing and
unrelated to constraints; the `Map`/`Filter` test indexes instead of threading
state to avoid it. This needs fixing before the real `Iterable::Next` signature in
section 4 can be written.
