# Nominal Newtypes Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `type Meters = Int;` declares a type distinct from `Int` that compiles to the identical bytecode as `Int`.

**Architecture:** A new `NewType` variant in `MidoriTypeUnion` carrying a name and a representation. It is nominal everywhere by default — `ToString` returns its own name, which is enough to give it independent typeclass instances, because instance lookup and method mangling are already keyed on `ToString`. Erasure is applied only at named opcode-selection sites in the code generator. No new AST statement node: the declaration reuses `MidoriStatement::TypeAlias`.

**Tech Stack:** C++23, CMake + Ninja presets, Catch2 unit tests, `.mdr` integration suites run by the Midori CLI.

**Spec:** `docs/superpowers/specs/2026-09-03-nominal-newtypes-design.md`

---

## Before you start

**Read the spec first.** Three premises in the original brief for this work were false, and the spec records what is actually true. In particular: `type X = Y` does not currently parse, the transparent form lives on `alias`, and a nominal-but-boxed newtype already exists as a single-variant sum. Erasure is the feature; nominality is nearly free.

**Build — PowerShell, NOT Git Bash.** Git Bash mangles the vcvars64 quoting and hangs.

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

**Baseline to hold: 311/311 integration, 885 assertions / 153 cases.** Do not regress either. Do **not** use `build/out/Midori.exe`, a stale April build.

**This checkout is shared with other sessions.** Run `git status` first and stage by explicit pathspec, never `git add -A`. There is at least one other live worktree under `.claude/worktrees/`.

**Traps, established the hard way:**

1. `SubstituteTypeParams` with an empty map is **not** the identity — since `e730762` it rebuilds a `StructType` with `m_generic_params` cleared. Do not copy that shortcut into the `NewType` arm.
2. `Analysis/SemanticFacts.cpp` uses `if constexpr` chains (57 arms) that **fall through silently** rather than failing to compile. This plan adds no AST node, so it should not bite — but read it before assuming so.
3. `src/Utility/Formatter/Formatter.cpp` has three switches ending in `default:` with the same hazard. Task 9 covers it.
4. **Measure, do not reason.** Several plans here asserted premises that survived only until someone traced them.
5. **`CodeGenerator.cpp` has three *exhaustive* `std::visit` visitors** — `IsGenericType`'s `GenericTypeVisitor` (`:5542`), `DeduceGenericVisitor` (`:5734`) and `SubstituteGenericTypes`'s visitor (`:6410`). They enumerate every alternative by hand with **no fallback**, so adding a variant to `MidoriTypeUnion` is a hard compile error there, not a silent fall-through. This is the *opposite* of trap 2 and it is good news — codegen cannot silently ignore a newtype. Discovered while executing Task 1, which had to add the three arms to keep the build green. Files listed per task below do not include this; expect it.

   The `DeduceGenericVisitor` arm added there guards on `m_concrete_type->IsType<MidoriType::NewType>()`, so a `Meters` concrete against an `Int` pattern deduces nothing. That guard is what keeps dispatch nominal — **do not remove it in Task 8.**

---

## File Structure

| File | Responsibility | Task |
|---|---|---|
| `src/Compiler/AbstractSyntaxTree/Type.h` | `NewType` variant, `MakeNewType` factory | 1 |
| `src/Compiler/AbstractSyntaxTree/Type.cpp` | `ToString`, equality, substitution arms | 1, 2 |
| `tests/unit/compiler/TypeTests.cpp` | **new** — nominal identity at the type layer | 1, 2 |
| `src/Compiler/Parser/Parser.h` | `ParseNewTypeBody` declaration | 3 |
| `src/Compiler/Parser/Parser.cpp` | RHS dispatch, leading-bar sums | 3 |
| `tests/unit/parser/ParserTests.cpp` | dispatch + associated-type boundary pins | 4 |
| `src/Compiler/TypeChecker/TypeChecker.cpp` | nominal rejection, derived `Convertable` | 5, 7 |
| `src/Compiler/CodeGenerator/CodeGenerator.h/.cpp` | `RepresentationOf`, erasure sites | 8 |
| `src/Utility/Formatter/Formatter.cpp` | round-trip the new syntax | 9 |
| `test/newtype/**` | `.mdr` integration suites | 5–8 |

New test files are picked up automatically — `tests/CMakeLists.txt` uses `GLOB_RECURSE CONFIGURE_DEPENDS`.

---

## Task 1: The `NewType` variant and its nominal identity

This is the task that delivers the feature's leverage: once `ToString` returns the newtype's own name, independent typeclass instances follow with no further work, because `InstanceKey` (`TypeChecker.h:60`) and `MangleInstanceMethodName` (`Type.cpp:707`) both key on `ToString`.

**Files:**
- Modify: `src/Compiler/AbstractSyntaxTree/Type.h`
- Modify: `src/Compiler/AbstractSyntaxTree/Type.cpp`
- Test: `tests/unit/compiler/TypeTests.cpp` (create)

- [ ] **Step 1: Write the failing test**

Create `tests/unit/compiler/TypeTests.cpp`:

```cpp
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
```

- [ ] **Step 2: Run the test to verify it fails**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
```

Expected: **compile error**, `MakeNewType` is not a member of `MidoriType`. That is the correct failure — the test cannot link before the variant exists.

- [ ] **Step 3: Add the variant and factory declaration**

In `src/Compiler/AbstractSyntaxTree/Type.h`, add the struct beside `UnionType` (after the `UnionType` definition, before `using MidoriTypeUnion`):

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

Add `NewType,` to the `MidoriTypeUnion` variant list, immediately after `UnionType,`.

Declare the factory beside `MakeUnionType`:

```cpp
	static std::shared_ptr<MidoriType> MakeNewType(const std::string& name, const std::shared_ptr<MidoriType>& representation, std::vector<std::string>&& generic_params = {});
```

- [ ] **Step 4: Implement the factory**

In `src/Compiler/AbstractSyntaxTree/Type.cpp`, beside `MakeUnionType` (around `:621`):

```cpp
std::shared_ptr<MidoriType> MidoriType::MakeNewType(const std::string& name, const std::shared_ptr<MidoriType>& representation, std::vector<std::string>&& generic_params)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(NewType{.m_name = name, .m_representation = representation, .m_generic_params = std::move(generic_params), .m_type_arguments = {}, .m_constraints = {}}));
}
```

- [ ] **Step 5: Add the `ToString` arm**

In `ToStringVisitor::operator()`, after the `UnionType` arm, add:

```cpp
			else if constexpr (std::is_same_v<Type, MidoriType::NewType>)
			{
				// Never renders its representation. InstanceKey and
				// MangleInstanceMethodName key on this string, so rendering the
				// representation here would collapse Hashable<Meters> into
				// Hashable<Int>.
				if (!type_variant.m_generic_params.empty())
				{
					return type_variant.m_name + "<"s + std::accumulate(std::next(type_variant.m_generic_params.begin()), type_variant.m_generic_params.end(), type_variant.m_generic_params.front(), join_with_comma) + ">"s;
				}

				if (type_variant.m_is_generic_instantiation && !type_variant.m_type_arguments.empty())
				{
					return StringifyTypeArguments(type_variant.m_name, type_variant.m_type_arguments);
				}

				return type_variant.m_name;
			}
```

- [ ] **Step 6: Add the equality arm**

In `TypeEqualityVisitor::operator()`, after the `UnionType` arm, add:

```cpp
		else if constexpr (std::is_same_v<TypeA, MidoriType::NewType>)
		{
			// Name and type arguments only. Comparing representations would make
			// Meters equal to Int, which is the whole thing this prevents.
			thread_local std::unordered_set<std::pair<const void*, const void*>, TypeConstPairHash> s_visiting;
			std::pair<const void*, const void*> key{&a, &b};
			if (s_visiting.contains(key))
			{
				return true;
			}
			s_visiting.insert(key);
			bool result = a.m_name == b.m_name
				&& a.m_type_arguments.size() == b.m_type_arguments.size()
				&& std::ranges::equal
				(
					a.m_type_arguments,
					b.m_type_arguments,
					[](const std::shared_ptr<MidoriType>& t1, const std::shared_ptr<MidoriType>& t2)
					{
						return *t1 == *t2;
					}
				);
			s_visiting.erase(key);
			return result;
		}
```

The `thread_local` visiting set mirrors `StructType` and `UnionType`; a newtype can recurse through its representation, so the guard is required, not decorative.

- [ ] **Step 7: Run the tests to verify they pass**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/MidoriUnitTests.exe "[type]"
```

Expected: 4 cases pass.

- [ ] **Step 8: Verify the tests bite**

Temporarily change the `ToString` arm's final `return type_variant.m_name;` to `return stringify(*type_variant.m_representation);`. Rebuild and rerun.

Expected: **exactly one case fails** — "A newtype renders as its own name". Restore the line.

(Corrected after execution. An earlier draft of this plan predicted two failures. Only one fails, and that is correct: `TypeEqualityVisitor`'s generic arm opens with `if constexpr (!std::is_same_v<TypeA, TypeB>) return false;`, so `NewType` versus `IntegerType` is decided on the variant alternative before `ToString` is ever called. The equality cases therefore exercise a genuinely independent path and are rightly unaffected by a `ToString` mutation.)

This is the corrupt-and-restore check the spec requires. Do not skip it — a test that renders the name by accident proves nothing.

- [ ] **Step 9: Run the full suites**

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Expected: 311/311 integration, and unit assertions **above** the 885 baseline with 0 failures.

- [ ] **Step 10: Commit**

```bash
git add src/Compiler/AbstractSyntaxTree/Type.h src/Compiler/AbstractSyntaxTree/Type.cpp tests/unit/compiler/TypeTests.cpp
git commit -m "feat(type): add a nominal NewType variant

ToString returns the newtype's own name and equality compares names rather
than representations. Since InstanceKey and MangleInstanceMethodName are both
keyed on ToString, this is what gives a newtype typeclass instances
independent of its representation.

Mutation-checked: rendering the representation instead of the name fails 2 of
the 4 new cases.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 2: Substitution, so generics behave as they do for records and sums

**Files:**
- Modify: `src/Compiler/AbstractSyntaxTree/Type.cpp`
- Test: `tests/unit/compiler/TypeTests.cpp`

- [ ] **Step 1: Write the failing test**

Append to `tests/unit/compiler/TypeTests.cpp`:

```cpp
TEST_CASE("Substituting a newtype's parameter rewrites its representation", "[type]")
{
	const std::shared_ptr<MidoriType> element = MidoriType::MakeGenericType("T");
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeArrayType(element);
	const std::shared_ptr<MidoriType> boxed = MidoriType::MakeNewType("Boxed", representation, {"T"});

	std::unordered_map<std::string, std::shared_ptr<MidoriType>> substitutions;
	substitutions["T"] = MidoriType::MakeLiteralType<MidoriType::IntegerType>();

	const std::shared_ptr<MidoriType> instantiated = MidoriType::SubstituteTypeParams(boxed, substitutions);

	REQUIRE(instantiated->IsType<MidoriType::NewType>());

	const MidoriType::NewType& result = instantiated->GetType<MidoriType::NewType>();
	REQUIRE(result.m_name == "Boxed");
	REQUIRE(result.m_representation->IsType<MidoriType::ArrayType>());
	REQUIRE(result.m_representation->GetType<MidoriType::ArrayType>().m_element_type->IsType<MidoriType::IntegerType>());
}

TEST_CASE("Substituting a newtype with an empty map preserves its parameters", "[type]")
{
	// Trap 1: since e730762, SubstituteTypeParams with an empty map is not the
	// identity for StructType - it rebuilds with m_generic_params cleared. The
	// NewType arm must not inherit that behaviour.
	const std::shared_ptr<MidoriType> element = MidoriType::MakeGenericType("T");
	const std::shared_ptr<MidoriType> representation = MidoriType::MakeArrayType(element);
	const std::shared_ptr<MidoriType> boxed = MidoriType::MakeNewType("Boxed", representation, {"T"});

	const std::unordered_map<std::string, std::shared_ptr<MidoriType>> empty;
	const std::shared_ptr<MidoriType> result = MidoriType::SubstituteTypeParams(boxed, empty);

	REQUIRE(result->IsType<MidoriType::NewType>());
	REQUIRE(result->GetType<MidoriType::NewType>().m_generic_params == std::vector<std::string>{"T"});
}
```

- [ ] **Step 2: Run to verify it fails**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/MidoriUnitTests.exe "[type]"
```

Expected: FAIL. Without a `NewType` arm, `SubstitutionVisitor`'s generic fallback returns a shallow copy, so `m_representation` still holds `T`.

- [ ] **Step 3: Add the substitution arm**

In `SubstitutionVisitor::operator()`, after the `UnionType` arm, add:

```cpp
			else if constexpr (std::is_same_v<T, MidoriType::NewType>)
			{
				std::vector<std::string> preserved_generic_params = type_variant.m_generic_params;
				TypePtr new_newtype = MidoriType::MakeNewType(type_variant.m_name, type_variant.m_representation, std::move(preserved_generic_params));
				cache[current_type.get()] = new_newtype;

				MidoriType::NewType& new_ref = new_newtype->GetType<MidoriType::NewType>();
				new_ref.m_representation = substitute(type_variant.m_representation);

				std::vector<MidoriType::ClassConstraint> new_constraints;
				new_constraints.reserve(type_variant.m_constraints.size());
				for (const MidoriType::ClassConstraint& constraint : type_variant.m_constraints)
				{
					std::vector<TypePtr> new_type_args;
					new_type_args.reserve(constraint.m_type_args.size());
					std::ranges::transform(constraint.m_type_args, std::back_inserter(new_type_args), substitute);
					new_constraints.emplace_back(constraint.m_class_name, std::move(new_type_args));
				}
				new_ref.m_constraints = std::move(new_constraints);

				if (!type_variant.m_generic_params.empty() || type_variant.m_is_generic_instantiation)
				{
					new_ref.m_is_generic_instantiation = true;
					new_ref.m_type_arguments = MidoriType::InstantiateTypeArguments(type_variant.m_generic_params, type_variant.m_type_arguments, substitute);
				}

				return new_newtype;
			}
```

Note the deliberate difference from the `StructType` arm: `m_generic_params` is **preserved**, not cleared. Trap 1.

> **Interaction discovered during execution — this bit, and it was a bug in this plan.**
>
> Preserving `m_generic_params` is correct, but it collides with the branch order of Task 1's `ToString` arm, which tests `!m_generic_params.empty()` *first* and renders the **parameter names**. A substituted `Boxed<Int>` therefore still carried `m_generic_params == {"T"}` and rendered `"Boxed<T>"` — and so did `Boxed<Text>`. Since `InstanceKey` and `MangleInstanceMethodName` are keyed on `ToString`, two distinct instantiations collapsed into one instance slot: a silent wrong-instance bug, in exactly the mechanism this feature exists to protect.
>
> `StructType` and `UnionType` never hit this because they *clear* `m_generic_params` on substitution — the very behaviour trap 1 says not to copy. Both halves of that advice were right individually and wrong together.
>
> **Fix:** reorder the `ToString` arm so the `m_is_generic_instantiation && !m_type_arguments.empty()` branch is tested *before* the `m_generic_params` branch. All three states then render correctly: an un-substituted declaration falls through to the names branch, an empty-map substitution renders `Boxed<T>`, and a real instantiation renders `Boxed<Int>`.
>
> **Lesson for the remaining tasks:** a test that only asserts on struct *fields* (as the two tests in this task originally did) will not catch a rendering bug. Where `ToString` is the load-bearing property, assert on `ToString` — and assert that two different instantiations render *differently*, not merely that one renders correctly.

- [ ] **Step 4: Run to verify it passes**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/MidoriUnitTests.exe "[type]"
```

Expected: 6 cases pass.

- [ ] **Step 5: Verify the trap-1 test bites**

Temporarily replace `std::vector<std::string> preserved_generic_params = type_variant.m_generic_params;` with `std::vector<std::string> preserved_generic_params;`. Rebuild, rerun.

Expected: "Substituting a newtype with an empty map preserves its parameters" **fails**. Restore.

- [ ] **Step 6: Run full suites and commit**

```powershell
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

```bash
git add src/Compiler/AbstractSyntaxTree/Type.cpp tests/unit/compiler/TypeTests.cpp
git commit -m "feat(type): substitute through a newtype's representation

Preserves m_generic_params rather than clearing them, unlike the StructType
arm - trap 1 in the plan. Mutation-checked: clearing them fails the empty-map
case.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 3: Parser — bare type name declares a newtype

**Files:**
- Modify: `src/Compiler/Parser/Parser.h` (declare `ParseNewTypeBody`, beside `ParseUnionBody` at `:642`)
- Modify: `src/Compiler/Parser/Parser.cpp` (`ParseTypeDeclaration` at `:3376`)

The dispatch after `=` becomes:

| RHS begins | Form |
|---|---|
| `{` | record — unchanged |
| a depth-0 `\|` before the terminating `;` | sum — unchanged, and now also accepts a leading `\|` |
| otherwise | newtype |

Migration cost is zero: all 8 top-level `type` declarations in `test/` and `MidoriPrelude/` today have either a `{` or a depth-0 `|`. Verified before this plan was written.

- [ ] **Step 1: Write the failing integration tests**

Create `test/newtype/success/declaration.mdr`:

```
module NewtypeDeclaration

import
{
	"../../../MidoriPrelude/Prelude/Panic.mdr",
}

type Meters = Int;

alias Feet = Int;

def distance: Meters = 5 as Meters;
def raw: Int = distance as Int;

if raw == 5 then () else Panic::Panic("newtype round-trip should preserve the value");
```

Create `test/newtype/success/leading_bar_sum.mdr`:

```
module NewtypeLeadingBarSum

import
{
	"../../../MidoriPrelude/Prelude/Panic.mdr",
}

// A single-variant sum now needs the explicit leading bar, which is what frees
// the bare-name spelling for newtypes.
type Solo = | Only(Int);

def value = new Solo::Only(3);
def unwrapped = match value with case Solo::Only(inner) => inner;

if unwrapped == 3 then () else Panic::Panic("leading-bar sum should still construct and match");
```

- [ ] **Step 2: Run to verify they fail**

```powershell
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

Expected: both FAIL. `declaration.mdr` reports `Expected ';' after union body` (the current behaviour, confirmed by probe); `leading_bar_sum.mdr` reports a parse error at the leading `|`, which is not accepted today — `SINGLE_BAR` is only a separator (`Parser.cpp:3288`).

- [ ] **Step 3: Add the lookahead helper**

In `src/Compiler/Parser/Parser.cpp`, above `ParseTypeDeclaration`:

```cpp
bool Parser::TypeBodyHasTopLevelBar() const
{
	// Bounded scan from the current token to the depth-0 ';' that ends the
	// declaration, following the ProbeArrayComprehension precedent. A depth-0
	// '|' means a sum; anything else is a newtype over a type expression.
	int paren_depth = 0;
	int angle_depth = 0;
	int brace_depth = 0;
	int bracket_depth = 0;

	for (int offset = 0; !Check(Token::Name::END_OF_FILE, offset); offset += 1)
	{
		const Token::Name token_name = Peek(offset).m_token_name;

		if (token_name == Token::Name::LEFT_PAREN) { paren_depth += 1; }
		else if (token_name == Token::Name::RIGHT_PAREN) { paren_depth -= 1; }
		else if (token_name == Token::Name::LEFT_ANGLE) { angle_depth += 1; }
		else if (token_name == Token::Name::RIGHT_ANGLE) { angle_depth -= 1; }
		else if (token_name == Token::Name::LEFT_BRACE) { brace_depth += 1; }
		else if (token_name == Token::Name::RIGHT_BRACE) { brace_depth -= 1; }
		else if (token_name == Token::Name::LEFT_BRACKET) { bracket_depth += 1; }
		else if (token_name == Token::Name::RIGHT_BRACKET) { bracket_depth -= 1; }

		const bool at_top_level = paren_depth == 0 && angle_depth == 0 && brace_depth == 0 && bracket_depth == 0;

		if (at_top_level && token_name == Token::Name::SINGLE_SEMICOLON)
		{
			return false;
		}

		if (at_top_level && token_name == Token::Name::SINGLE_BAR)
		{
			return true;
		}
	}

	return false;
}
```

Declare it in `src/Compiler/Parser/Parser.h` beside the other probe helpers:

```cpp
	bool TypeBodyHasTopLevelBar() const;
```

All nine token names used above are verified present in `src/Compiler/Token/Token.h`: `LEFT_PAREN` (`:18`), `RIGHT_PAREN`, `LEFT_BRACE`, `RIGHT_BRACE`, `LEFT_BRACKET`, `RIGHT_BRACKET` (`:23`), `RIGHT_ANGLE` (`:47`), `LEFT_ANGLE` (`:49`), `END_OF_FILE` (`:134`).

> **The scan above is incomplete as written, and it shipped a regression. Corrected during execution.**
>
> The lexer merges `>>` into a single `RIGHT_SHIFT` token (`Token.h:33`). Counting only `RIGHT_ANGLE` means `Array<Array<Int>>` never returns `angle_depth` to zero, so the scan misses its depth-0 `;`, runs to EOF, returns `false`, and misroutes the declaration to `ParseNewTypeBody`. The visible symptom was `type Result = Ok(Array<Array<Int>>) | Err(Text);` — **valid union syntax that parsed fine before this task** — failing with `Undefined struct or union`.
>
> The parser already had the answer: `ConsumeTypeRightAngle` (`Parser.cpp:995`) splits a `RIGHT_SHIFT` into two synthetic `RIGHT_ANGLE` tokens. A read-only probe must not mutate the token stream, so the fix is `angle_depth -= 2` on `RIGHT_SHIFT`. `LEFT_SHIFT` needs no symmetric arm — two `<` cannot lex adjacently in valid type syntax, and adding one would desync the probe from `ParseType`.
>
> Also add the `MAX_ARRAY_SIZE` cap and an early bail-out when any depth goes negative, so the comment's claim of a "bounded scan" is actually true — `ProbeArrayComprehension`, the cited precedent, has both guards and this did not.
>
> **Lesson: when a lookahead probe duplicates logic the real parser already has, find what the real parser does about edge cases first.** `ConsumeTypeRightAngle` existed precisely because someone already hit this.

- [ ] **Step 4: Add the newtype body parser**

In `src/Compiler/Parser/Parser.cpp`, after `ParseUnionBody`:

```cpp
MidoriResult::StatementResult Parser::ParseNewTypeBody(TypeDeclarationHeader&& header)
{
	MidoriResult::TypeResult representation_result = ParseType();
	if (!representation_result.has_value())
	{
		return std::unexpected(representation_result.error());
	}

	std::shared_ptr<MidoriType> representation = std::move(representation_result.value());

	MidoriResult::TokenResult semicolon_result = Consume(Token::Name::SINGLE_SEMICOLON, "Expected ';' after newtype definition.");
	if (!semicolon_result.has_value())
	{
		return std::unexpected(semicolon_result.error());
	}

	std::vector<std::string> generic_param_names;
	std::ranges::transform(header.m_generic_params, std::back_inserter(generic_param_names), [](const Token& generic_param) { return generic_param.m_lexeme; });

	std::shared_ptr<MidoriType> new_type = MidoriType::MakeNewType(header.m_name.m_lexeme, representation, std::move(generic_param_names));

	if (header.m_has_generic_params)
	{
		EndScope();
	}

	// Registered in the enclosing scope so uses of the name resolve to the
	// nominal type rather than to its representation. This is the line that makes
	// the declaration nominal rather than transparent.
	m_state.m_scopes.back().m_defined_types[header.m_name.m_lexeme] = new_type;

	return std::make_unique<MidoriStatement>(MidoriStatement::TypeAlias(std::move(header.m_name), std::move(header.m_generic_params), std::move(new_type)));
}
```

Declare in `Parser.h` beside `ParseUnionBody`:

```cpp
	MidoriResult::StatementResult ParseNewTypeBody(TypeDeclarationHeader&& header);
```

Reusing `MidoriStatement::TypeAlias` is deliberate and verified: `ShadowingPolicyDiagnostic` calls `DefineType(name)`, `SemanticFacts::Visit` is empty, and `CodeGenerator::operator()` emits nothing — exactly what a newtype declaration wants. No new AST variant means trap 2 does not apply.

- [ ] **Step 5: Wire the dispatch**

In `ParseTypeDeclaration` (`:3376`), replace the trailing dispatch — currently `if (Check(LEFT_BRACE, 0)) { return ParseStructBody(...); } return ParseUnionBody(...);` — with:

```cpp
	if (Check(Token::Name::LEFT_BRACE, 0))
	{
		return ParseStructBody(std::move(header_result.value()));
	}

	// A depth-0 '|' anywhere before the terminating ';' means a sum. Without one,
	// the right-hand side is a type expression and this declares a newtype. A
	// single-variant sum therefore needs an explicit leading bar.
	if (!TypeBodyHasTopLevelBar())
	{
		return ParseNewTypeBody(std::move(header_result.value()));
	}

	Match(Token::Name::SINGLE_BAR);
	return ParseUnionBody(std::move(header_result.value()));
```

The bare `Match` consumes an optional leading bar; `ParseUnionBody` then sees its usual first member.

- [ ] **Step 6: Run to verify the tests pass**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

Expected: both pass. `declaration.mdr` needs the derived `Convertable` instances from Task 7 for its `as` casts — if it fails on those and not on parsing, that is expected at this point; move its `as` lines into Task 7 and keep only the declaration here.

- [ ] **Step 7: Run the full suites**

Expected: 311/311 + the new cases, no regressions. If any existing suite breaks, the depth tracking in `TypeBodyHasTopLevelBar` is the first suspect — a generic sum such as `type Boxed<T> = Empty | Full(T)` has `<` and `>` before its bar.

- [ ] **Step 8: Commit**

```bash
git add src/Compiler/Parser/Parser.h src/Compiler/Parser/Parser.cpp test/newtype
git commit -m "feat(parser): a bare type name on the right of type declares a newtype

Dispatch after '=' is now brace -> record, depth-0 bar -> sum, otherwise
newtype. Single-variant sums take an explicit leading bar, which is what frees
the bare-name spelling. Zero migrations: all 8 existing top-level type
declarations already carry a brace or a depth-0 bar.

Reuses MidoriStatement::TypeAlias rather than adding an AST node, since every
consumer of that node already does what a newtype declaration wants.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 4: Pin the dispatch and the associated-type boundary

`type Item = Int;` inside a `class` or `instance` body is an associated-type binding and is byte-identical to a top-level newtype declaration. There are 9 such bindings today. They are parsed by separate loops (`Parser.cpp:3544` class body, `:3820` instance body) that never reach top-level dispatch (`:5534`), so nothing breaks — but two readings of one syntax now sit side by side, so pin the boundary.

**Files:**
- Modify: `tests/unit/parser/ParserTests.cpp`

- [ ] **Step 1: Write the failing tests**

Append to `tests/unit/parser/ParserTests.cpp`:

```cpp
TEST_CASE("Parser lowers a bare type name onto a nominal newtype", "[parser]")
{
	const std::string source_code =
		R"(module NewtypeLowering
type Meters = Int;
alias Feet = Int;
)";

	std::expected<MidoriTest::ParsedSnippet, CompilerError> parse_result = MidoriTest::ParseSnippet(source_code, "NewtypeLowering.mdr");
	if (!parse_result.has_value())
	{
		FAIL(std::string(parse_result.error().Rendered()));
	}

	const MidoriProgramTree& program = parse_result->m_program;
	REQUIRE(program.size() == 2u);

	REQUIRE(program[0u]->IsStatement<MidoriStatement::TypeAlias>());
	REQUIRE(program[1u]->IsStatement<MidoriStatement::TypeAlias>());

	// Same node, opposite semantics: the newtype carries a NewType, the alias
	// carries the bare representation. That difference is the feature.
	const MidoriStatement::TypeAlias& newtype = program[0u]->GetStatement<MidoriStatement::TypeAlias>();
	const MidoriStatement::TypeAlias& alias = program[1u]->GetStatement<MidoriStatement::TypeAlias>();

	REQUIRE(newtype.m_name.m_lexeme == "Meters");
	REQUIRE(newtype.m_aliased_type->IsType<MidoriType::NewType>());
	REQUIRE(newtype.m_aliased_type->GetType<MidoriType::NewType>().m_representation->IsType<MidoriType::IntegerType>());

	REQUIRE(alias.m_name.m_lexeme == "Feet");
	REQUIRE(alias.m_aliased_type->IsType<MidoriType::IntegerType>());
}

TEST_CASE("Parser still reads a leading-bar single-variant sum as a union", "[parser]")
{
	const std::string source_code =
		R"(module LeadingBarSum
type Solo = | Only(Int);
)";

	std::expected<MidoriTest::ParsedSnippet, CompilerError> parse_result = MidoriTest::ParseSnippet(source_code, "LeadingBarSum.mdr");
	if (!parse_result.has_value())
	{
		FAIL(std::string(parse_result.error().Rendered()));
	}

	const MidoriProgramTree& program = parse_result->m_program;
	REQUIRE(program.size() == 1u);
	REQUIRE(program[0u]->IsStatement<MidoriStatement::Union>());
	REQUIRE(program[0u]->GetStatement<MidoriStatement::Union>().m_name.m_lexeme == "Solo");
}

TEST_CASE("A type binding inside an instance body is still an associated type", "[parser]")
{
	// `type Item = Int;` here is byte-identical to a top-level newtype. The two
	// readings are kept apart by parse position, not by syntax, so this pins the
	// boundary rather than trusting it.
	const std::string source_code =
		R"(module AssociatedTypeBoundary
class Container<T> {
	type Item;
	First: fn(value: T) -> Item;
};
type Meters = Int;
)";

	std::expected<MidoriTest::ParsedSnippet, CompilerError> parse_result = MidoriTest::ParseSnippet(source_code, "AssociatedTypeBoundary.mdr");
	if (!parse_result.has_value())
	{
		FAIL(std::string(parse_result.error().Rendered()));
	}

	const MidoriProgramTree& program = parse_result->m_program;
	REQUIRE(program.size() == 2u);

	// The class body's `type Item` stayed inside the class and produced no
	// top-level declaration of its own.
	REQUIRE(program[0u]->IsStatement<MidoriStatement::Class>());
	REQUIRE(program[1u]->IsStatement<MidoriStatement::TypeAlias>());
	REQUIRE(program[1u]->GetStatement<MidoriStatement::TypeAlias>().m_aliased_type->IsType<MidoriType::NewType>());
}
```

Check the class-body associated-type spelling (`type Item;` versus `type Item = Int;`) against `Parser.cpp:3544` and match whichever the parser accepts in a `class` body.

- [ ] **Step 2: Restore the recovery-test case**

`8f0d754` changed the synchronization test's `type` starter from `type Alias = Int;` to `type Nominal = Empty | Full(Int);`, because at that point the bare spelling was no longer valid syntax. It is valid again now, and it is the more interesting case for that test since it exercises the new dispatch. In the `SyncCase` table (~`:397`), change:

```cpp
		{ "type", "type Nominal = Empty | Full(Int);\n", Token::Name::TYPE },
```

to:

```cpp
		{ "type", "type Nominal = Int;\n", Token::Name::TYPE },
```

Leave the `alias` row alone.

- [ ] **Step 3: Run to verify they pass**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/MidoriUnitTests.exe "[parser]"
```

- [ ] **Step 4: Verify they bite**

Temporarily invert the dispatch condition in `ParseTypeDeclaration` from `if (!TypeBodyHasTopLevelBar())` to `if (TypeBodyHasTopLevelBar())`. Rebuild, rerun.

Expected: the newtype-lowering and leading-bar cases **fail**. Restore.

- [ ] **Step 5: Commit**

```bash
git add tests/unit/parser/ParserTests.cpp
git commit -m "test: pin the newtype dispatch and the associated-type boundary

type Item = Int; inside a class or instance body is byte-identical to a
top-level newtype and is kept apart only by parse position, so the boundary is
asserted rather than trusted. Restores the bare spelling in the recovery test,
valid again now that it declares a newtype.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 5: Nominal rejection in both directions

**Files:**
- Test: `test/newtype/failure/rejects_representation.mdr` + `.expected` (create)
- Test: `test/newtype/failure/rejects_newtype.mdr` + `.expected` (create)
- Modify: `src/Compiler/TypeChecker/TypeChecker.cpp` only if the tests do not already pass

A newtype should reject its representation with no type-checker change at all, because unification compares types with `operator==`, which Task 1 already made nominal. Run the tests before writing any code.

- [ ] **Step 1: Write the failing tests**

`test/newtype/failure/rejects_representation.mdr`:

```
module NewtypeRejectsRepresentation

type Meters = Int;

defun TakesMeters(m: Meters) : Int => 0;

def raw: Int = 5;
def result: Int = TakesMeters(raw);
```

`test/newtype/failure/rejects_newtype.mdr`:

```
module NewtypeRejectsNewtype

type Meters = Int;

defun TakesInt(value: Int) : Int => value;

def distance: Meters = 5 as Meters;
def result: Int = TakesInt(distance);
```

- [ ] **Step 2: Run them and record the actual messages**

```powershell
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

Both must fail to compile. Capture each error verbatim — including the caret line — and write it into the matching `.expected` file. Follow the format of an existing failure snapshot, e.g. `test/type_alias/failure/`.

Do **not** hand-write the expected text from imagination. Run the compiler, read what it says, paste that.

- [ ] **Step 3: Verify each snapshot bites**

For each `.expected`, change one word of the message, rerun `Midori.exe test newtype`, confirm that suite now **fails**, then restore the file and confirm it passes again.

A snapshot that was never observed failing is not a test.

- [ ] **Step 4: If either direction wrongly compiles, fix unification**

Only if Step 2 showed a clean compile. In `TypeChecker.cpp`, find the unification entry point and add an early rejection before any structural comparison:

```cpp
	if (left->IsType<MidoriType::NewType>() != right->IsType<MidoriType::NewType>())
	{
		return false;
	}
```

Then rerun Steps 2 and 3. If Step 2 already failed correctly, skip this step entirely and say so in the commit message — a change that was not needed should not be invented.

- [ ] **Step 5: Run the full suites and commit**

```bash
git add test/newtype
git commit -m "test: a newtype rejects its representation in both directions

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 6: Typeclass instances attach to the newtype, not the representation

This is the property spec §5 needs: `Hashable<Text>` must not become `Hashable<Array<Byte>>`.

**Files:**
- Test: `test/newtype/success/typeclass_instance.mdr` (create)
- Test: `test/newtype/failure/instance_does_not_leak.mdr` + `.expected` (create)

- [ ] **Step 1: Write the passing case**

`test/newtype/success/typeclass_instance.mdr`:

```
module NewtypeTypeclassInstance

import
{
	"../../../MidoriPrelude/Prelude/Panic.mdr",
}

type Meters = Int;

class Describable<T> {
	Describe: fn(value: T) -> Int;
};

instance Describable<Meters> {
	defun Describe(value: Meters): Int => value as Int;
};

def distance: Meters = 7 as Meters;

if Describable::Describe(distance) == 7 then () else Panic::Panic("instance on the newtype should resolve");
```

- [ ] **Step 2: Write the failing case**

`test/newtype/failure/instance_does_not_leak.mdr`:

```
module NewtypeInstanceDoesNotLeak

type Meters = Int;

class Describable<T> {
	Describe: fn(value: T) -> Int;
};

instance Describable<Meters> {
	defun Describe(value: Meters): Int => value as Int;
};

// The instance is written for Meters. A raw Int must not find it.
def leaked: Int = Describable::Describe(7);
```

- [ ] **Step 3: Run both**

```powershell
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

The success case needs Task 7's derived `Convertable` for its `as` casts; if it fails only on those, complete Task 7 first and return here. The failure case should already report `no matching concrete instance for 'Describable::Describe'` — that behaviour was confirmed by probe on the equivalent single-variant sum, and it comes from `InstanceKey` being `ToString`-keyed.

Record the failure case's verbatim message in `instance_does_not_leak.expected`.

- [ ] **Step 4: Verify the snapshot bites**

Corrupt one word of the `.expected`, rerun, confirm failure, restore, confirm pass.

- [ ] **Step 5: Commit**

```bash
git add test/newtype
git commit -m "test: a typeclass instance on a newtype does not apply to its representation

This is the Hashable<Text> vs Hashable<Array<Byte>> property spec section 5
needs. It follows from InstanceKey being keyed on ToString.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 7: Derived `Convertable` instances for `as` in both directions

For an erasing newtype both conversions are identity, so requiring users to hand-write them is pure ceremony. Derive `Convertable<Rep, New>` and `Convertable<New, Rep>` when the declaration is checked.

**Files:**
- Modify: `src/Compiler/TypeChecker/TypeChecker.cpp` (`operator()(MidoriStatement::TypeAlias&)` at `:3863`)
- Test: `test/newtype/success/conversion.mdr` (create)

- [ ] **Step 1: Write the failing test**

`test/newtype/success/conversion.mdr`:

```
module NewtypeConversion

import
{
	"../../../MidoriPrelude/Prelude/Panic.mdr",
}

type Meters = Int;

def distance: Meters = 12 as Meters;
def raw: Int = distance as Int;
def round_tripped: Meters = raw as Meters;

if raw == 12 then () else Panic::Panic("newtype to representation should preserve the value");
if (round_tripped as Int) == 12 then () else Panic::Panic("round trip should preserve the value");
```

- [ ] **Step 2: Run to verify it fails**

```powershell
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

Expected: FAIL with `Define 'instance Convertable<Int, Meters>' to enable this conversion.` — that suggestion string is built at `TypeChecker.cpp:4553`.

- [ ] **Step 3: Register the derived instances**

In `TypeChecker::operator()(MidoriStatement::TypeAlias& type_alias)` (`:3863`), after the existing duplicate-generic-parameter check, add:

```cpp
	if (type_alias.m_aliased_type->IsType<MidoriType::NewType>())
	{
		const MidoriType::NewType& new_type = type_alias.m_aliased_type->GetType<MidoriType::NewType>();
		RegisterIdentityConversion(type_alias.m_aliased_type, new_type.m_representation);
		RegisterIdentityConversion(new_type.m_representation, type_alias.m_aliased_type);
	}
```

Add the helper beside the other instance-registration code, matching how `m_instances` is populated at `:3688`:

```cpp
void TypeChecker::RegisterIdentityConversion(const std::shared_ptr<MidoriType>& from_type, const std::shared_ptr<MidoriType>& to_type)
{
	InstanceKey conversion_key{std::string(CONVERTABLE_CLASS_NAME), {from_type->ToString(), to_type->ToString()}};
	if (m_instances.contains(conversion_key))
	{
		return;
	}

	std::vector<std::shared_ptr<MidoriType>> type_args{from_type, to_type};
	m_instances.emplace
	(
		std::move(conversion_key),
		InstanceInfo(std::string(CONVERTABLE_CLASS_NAME), std::move(type_args), {}, {}, {})
	);
}
```

Declare it in `TypeChecker.h` beside the other private helpers:

```cpp
	void RegisterIdentityConversion(const std::shared_ptr<MidoriType>& from_type, const std::shared_ptr<MidoriType>& to_type);
```

`CONVERTABLE_CLASS_NAME` is already available from `Common/Constant/Constant.h:77`.

The `contains` guard matters: a user may write their own `Convertable<Int, Meters>` with real behaviour, and a hand-written instance must win over the derived one.

- [ ] **Step 4: Run to verify it passes**

If codegen now reports `Convertable instance method '$Convert_Convertable_Int_Meters' not found` (`CodeGenerator.cpp:2594`), that is expected — a derived instance has no method body. Task 8 makes these conversions emit nothing, which is what resolves it. Note the failure and continue to Task 8, then return and confirm this test passes.

- [ ] **Step 5: Commit**

```bash
git add src/Compiler/TypeChecker/TypeChecker.h src/Compiler/TypeChecker/TypeChecker.cpp test/newtype
git commit -m "feat(typechecker): derive identity Convertable instances for newtypes

Both directions of a newtype conversion are identity once the newtype erases,
so requiring hand-written instances is ceremony. A user-written instance still
wins - the derived one is only registered if no instance exists for that key.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 8: Erasure — the task the feature exists for

**Read the spec's "Code generation — erased" section before starting.** The naive rule "nominal in the front end, erased in the back end" is **wrong** and tracing proved it. Erasing in the wrong place makes `instance Foo<Int>` match `Meters` and silently destroys everything Tasks 1–7 built.

> **Opcode selection erases. Dispatch stays nominal.**

**Erase at these sites only:**

| Site | Uses | Why it erases |
|---|---|---|
| `operand_type`, `CodeGenerator.cpp:2843`, `:2882`, `:3012`, `:3518` | 45 | arithmetic opcode selection |
| `from_type` / `target_type`, `:2490`–`:2491` | 34 | built-in cast selection, **after** `Convertable` lookup has been attempted nominally |
| `iter_type` | 1 | for-loop iteration opcode |

**Do NOT erase at these — they are dispatch:**

| Site | Uses | Why it stays nominal |
|---|---|---|
| `MatchInstanceTypeArg(pattern, concrete)`, `:915` | 15 | instance selection — erasing makes `instance Foo<Int>` match `Meters` |
| `DeduceGenericVisitor::m_concrete_type`, `:5579` | 9 | generic specialisation keying |
| the recursive `type` helper at `:18`–`:110` | 10 | must *recurse into* the representation to find generics, not erase it |

Note `GetConcreteTypeForExpression` (`CodeGenerator.h:359`) is **not** a safe blanket chokepoint despite feeding all four `operand_type` declarations, because its result at `:2504` also feeds `Convertable` lookup.

**Files:**
- Modify: `src/Compiler/CodeGenerator/CodeGenerator.h`, `src/Compiler/CodeGenerator/CodeGenerator.cpp`
- Test: `test/newtype/success/erasure.mdr` (create)

- [ ] **Step 1: Write the failing test**

`test/newtype/success/erasure.mdr`:

```
module NewtypeErasure

import
{
	"../../../MidoriPrelude/Prelude/Panic.mdr",
}

type Meters = Int;

def a: Meters = 3 as Meters;
def b: Meters = 4 as Meters;
def sum: Int = (a as Int) + (b as Int);

if sum == 7 then () else Panic::Panic("arithmetic through a newtype should work");
```

- [ ] **Step 2: Run to verify it fails**

Expected: a codegen error — either the missing `$Convert_Convertable_*` method from Task 7, or an opcode-selection failure because `Meters` is not an `IntegerType` at an arithmetic site.

- [ ] **Step 3: Add the erasure helper**

In `src/Compiler/CodeGenerator/CodeGenerator.cpp`, near the other type helpers:

```cpp
const std::shared_ptr<MidoriType>& CodeGenerator::RepresentationOf(const std::shared_ptr<MidoriType>& type)
{
	// Opcode selection only. Never call this from instance selection or generic
	// deduction - a newtype must stay nominal there, or instance Foo<Int> starts
	// matching Meters.
	const std::shared_ptr<MidoriType>* current = &type;
	while (*current != nullptr && (*current)->IsType<MidoriType::NewType>())
	{
		current = &(*current)->GetType<MidoriType::NewType>().m_representation;
	}

	return *current;
}
```

Declare in `CodeGenerator.h`:

```cpp
	static const std::shared_ptr<MidoriType>& RepresentationOf(const std::shared_ptr<MidoriType>& type);
```

- [ ] **Step 4: Apply it at the arithmetic sites**

At `:2843`, `:2882`, `:3012` change each

```cpp
	const std::shared_ptr<MidoriType>& operand_type = GetConcreteTypeForExpression(binary.m_left);
```

to

```cpp
	const std::shared_ptr<MidoriType>& operand_type = RepresentationOf(GetConcreteTypeForExpression(binary.m_left));
```

and at `:3518`

```cpp
	std::shared_ptr<MidoriType> operand_type = RepresentationOf(GetConcreteTypeForExpression(unary.m_expr));
```

`RepresentationOf` returns a reference into the type graph, so binding it to a `const&` is safe; the `:3518` site copies, which is also fine.

- [ ] **Step 5: Make newtype casts emit nothing**

In the `As` handler (`:2490` onwards), after the nominal `Convertable` lookup has been attempted and before built-in cast selection, add:

```cpp
	// A newtype and its representation share a runtime representation, so a
	// conversion between them is a no-op. This runs after the Convertable lookup
	// so a user-written instance with real behaviour still wins.
	if (RepresentationOf(from_type) == RepresentationOf(target_type)
		&& (from_type->IsType<MidoriType::NewType>() || target_type->IsType<MidoriType::NewType>()))
	{
		return;
	}
```

Place this precisely where a resolved `Convertable` instance would already have been emitted, so a hand-written instance takes priority. Read `:2490`–`:2600` before inserting rather than trusting the line numbers.

- [ ] **Step 6: Run to verify it passes**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

Both this and Task 7's `conversion.mdr` should now pass.

- [ ] **Step 7: Prove erasure — the test that makes the feature real**

Every other test in this plan would still pass if newtypes boxed. This one would not.

```powershell
./out/build/ninja/x64-development/out/Midori.exe build test/newtype/success/erasure.mdr --format json
```

Write an equivalent program using plain `Int` instead of `Meters`, build it the same way, and compare the `$main$` `opcodes` arrays. They must be **identical**.

Also assert the negative directly: the newtype build must contain **no opcode 159** (`CONSTRUCT_UNION`). Confirm 159 is still `CONSTRUCT_UNION` before relying on it, since adding opcodes would shift it:

```bash
sed -n '/enum class OpCode/,/^};/p' src/Common/Executable/Executable.h | grep -oE '^\s+[A-Z][A-Z0-9_]*,' | tr -d ' ,' | nl -v0 | grep CONSTRUCT_UNION
```

Record the comparison as `test/newtype/success/erasure.expected` if the harness supports output snapshots for it; otherwise record the two opcode arrays in the commit message so a future regression is bisectable.

- [ ] **Step 8: Verify the erasure check bites**

Temporarily make `RepresentationOf` return `type` unchanged. Rebuild. Confirm `erasure.mdr` fails. Restore.

- [ ] **Step 9: Confirm dispatch stayed nominal**

Rerun Task 6's suites specifically:

```powershell
./out/build/ninja/x64-development/out/Midori.exe test newtype
```

`instance_does_not_leak.mdr` must **still fail to compile**. If erasure leaked into instance selection, it will now wrongly succeed — that is the regression this step exists to catch.

- [ ] **Step 10: Run the full suites and commit**

```bash
git add src/Compiler/CodeGenerator/CodeGenerator.h src/Compiler/CodeGenerator/CodeGenerator.cpp test/newtype
git commit -m "feat(codegen): erase newtypes at opcode selection only

A newtype and its representation share a runtime representation, so arithmetic
and casts strip the wrapper and conversions emit nothing. Instance selection
and generic deduction deliberately do not strip it: erasing there would make
instance Foo<Int> match Meters.

Verified: type Meters = Int compiles to the same \$main\$ opcodes as Int, and
emits no CONSTRUCT_UNION.

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 9: Formatter round-trip

Trap 3: `src/Utility/Formatter/Formatter.cpp` has three switches ending in `default:`, so a new syntax can format wrongly without any compile error.

**Files:**
- Modify: `src/Utility/Formatter/Formatter.cpp`
- Test: `tests/unit/utility/FormatterTests.cpp`

- [ ] **Step 1: Check current behaviour**

```powershell
./out/build/ninja/x64-development/out/Midori.exe fmt test/newtype/success/declaration.mdr
```

Read the output. If `type Meters = Int;` and `type Solo = | Only(Int);` both survive unchanged, the arms may already be adequate — but still add the test in Step 2, because `default:` can produce correct output by accident and stop doing so later.

- [ ] **Step 2: Write the failing test**

Append to `tests/unit/utility/FormatterTests.cpp`, matching the `FormatOrFail` style already used there:

```cpp
TEST_CASE("Formatter round-trips newtype declarations and leading-bar sums", "[formatter]")
{
	// Formatter.cpp has three switches ending in default:, so a new syntax can
	// format wrongly with no compile error. This pins both new spellings.
	const std::string source_code =
		"module Main\n"
		"type Meters=Int;\n"
		"type Solo=|Only(Int);\n";

	const std::string formatted = FormatOrFail(source_code, "NewtypeFormat.mdr");

	const std::string expected =
		"module Main\n"
		"type Meters = Int;\n"
		"type Solo = | Only(Int);\n";

	CHECK(formatted == expected);
}

TEST_CASE("Formatter is idempotent for newtype declarations", "[formatter]")
{
	const std::string source_code =
		"module Main\n"
		"type Meters = Int;\n"
		"type Solo = | Only(Int);\n";

	const std::string once = FormatOrFail(source_code, "NewtypeIdempotent.mdr");
	const std::string twice = FormatOrFail(once, "NewtypeIdempotent.mdr");

	CHECK(once == twice);
}
```

If the formatter's chosen spacing for the leading bar differs from `= | Only(Int)`, adopt whatever it actually emits as `expected` — the point is that the output is stable and lossless, not that it matches a spelling guessed here. Run it, read it, pin it.

- [ ] **Step 3: Fix the three `default:` switches**

Read all three. Add explicit arms for the newtype declaration and the leading bar. Do not rely on `default:` — that is exactly the hazard.

- [ ] **Step 4: Run and commit**

```bash
git add src/Utility/Formatter/Formatter.cpp tests/unit/utility
git commit -m "fix(formatter): round-trip newtype declarations and leading-bar sums

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Task 10: Audit sweep and final verification

- [ ] **Step 1: Audit the TypeChecker `IsType` sites**

There are 219 in `TypeChecker.cpp`. The default — a `NewType` not matching and falling through — is correct for almost all of them, which is why this is an audit rather than an edit pass.

Grep for the sites that test for a *representation-like* type and ask whether a newtype over that type should also be accepted:

```bash
grep -nE "IsType<MidoriType::(IntegerType|FloatType|TextType|ArrayType|BoolType|ByteType|WordType)>" src/Compiler/TypeChecker/TypeChecker.cpp
```

For each hit, decide and record: nominal (leave alone) or representation (add an explicit unwrap). Bias hard toward leaving alone — a newtype that is wrongly accepted as its representation is a silent correctness bug, whereas one wrongly rejected is a loud, fixable error.

Write the findings into the commit message. If any site needs an unwrap, add a `.mdr` test for it first.

- [ ] **Step 2: Read `Analysis/SemanticFacts.cpp`**

Trap 2. This plan adds no AST node, so the 57 `if constexpr` arms should not need changes. Confirm by reading rather than by absence of a compile error, which is precisely what this file will not give you.

- [ ] **Step 3: Full verification**

```powershell
cmd /c '"C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat" >nul && cmake --build --preset x64-development'
./out/build/ninja/x64-development/out/Midori.exe test
./out/build/ninja/x64-development/out/MidoriUnitTests.exe
```

Required: **all** integration tests pass at 311 + the new newtype cases, and unit assertions above 885 with zero failures. Paste the actual summary lines into the commit message — not a claim that they passed.

- [ ] **Step 4: Confirm the tree is clean and staged by pathspec**

```bash
git status --short
```

Stage only your own files. Another session has a live worktree under `.claude/worktrees/`.

- [ ] **Step 5: Commit**

```bash
git commit -m "chore: audit type-checker IsType sites for newtype handling

Co-Authored-By: Claude Opus 5 <noreply@anthropic.com>"
```

---

## Out of scope

- Deleting `struct`/`union` or migrating the prelude's 131 declaration sites.
- Collapsing `Struct`, `Union` and `TypeAlias` into one `TypeDefinition` (spec §4).
- Making `Text` a newtype over `Array<Byte>` (spec §5). This plan is the mechanism that unblocks it.
- Newtype deriving beyond the two identity `Convertable` instances.
