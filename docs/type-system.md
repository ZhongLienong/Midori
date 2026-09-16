# Midori Type System

Midori uses a Hindley-Milner based, nominal type system with bidirectional inference. The implementation follows Algorithm W and extends it with algebraic data types, type classes, associated types, deriving, type-definition constraints, and exhaustive pattern matching.

Unless a snippet explicitly shows a full file, the examples below focus on the type-system surface only. Complete `.mdr` source files still need an explicit `module` declaration.

## Overview

The type system provides:

- **Full local inference** for most expressions
- **Bidirectional inference** for constructors and lambdas when an expected type is available
- **Parametric polymorphism** for functions, structs, unions, and aliases
- **Ad-hoc polymorphism** through type classes and instances
- **Associated types** for type classes whose output types are determined by an input type
- **Nominal algebraic data types** through structs and unions
- **Deriving** for common structural and container operations
- **Constraint propagation** from both function signatures and type definitions
- **Compile-time exhaustiveness checking** for `match` on unions and `Bool`

## Primitive Types

| Type | Description |
|------|-------------|
| `Int` | 64-bit signed integer |
| `Float` | 64-bit IEEE 754 double |
| `Byte` | 8-bit unsigned integer |
| `Word` | 64-bit unsigned integer |
| `Bool` | `true` or `false` |
| `Text` | UTF-8 string |
| `Unit` | Unit value type |

## Composite Types

### Arrays

Homogeneous arrays use `Array<T>`:

```midori
Array<Int>
Array<Array<Text>>
```

Empty array literals require context:

```midori
def values : Array<Int> = [];
```

Use `#` to get the length:

```midori
def arr = [1, 2, 3, 4, 5];
def len = #arr;
```

Arrays are iterable:

```midori
for name in ["Alice", "Bob", "Charlie"] {
    IO::PrintLine(name);
};
```

### Tuples

Tuples are fixed-size heterogeneous values:

```midori
(Int, Text)
(Bool, Int, Float)
```

### Functions

Function types are first-class:

```midori
fn(Int) -> Bool
fn(Int, Int) -> Int
fn() -> Unit
fn(fn(Int) -> Int) -> Int
```

### Concurrency Types

`Worker<T>` and `Channel<T>` are opaque handle types for the concurrency system:

```midori
Worker<Int>       // handle to a worker that returns Int
Channel<Text>     // handle to a channel carrying Text values
```

Both are internally represented as `Int` handles but carry compile-time type parameters. `Worker<T>` is produced by `Concurrency::Spawn` and consumed by `Concurrency::Join`. `Channel<T>` is produced by `Concurrency::MakeChannel(capacity)`, which takes `T` from context, and used with `->` (send) and `<-` (receive).

### Ranges

Ranges are used directly in `for` loops:

```midori
for i in 0..1..10 {
    IO::PrintLine(i as Text);
};

for i in 10..-1..0 {
    IO::PrintLine(i as Text);
};
```

Custom iteration is expressed through the `Iterable` type class and its associated `Item` type.

## Operator Grouping

Operators that do not bind equally cannot be mixed in one expression without
parentheses. `a + b * c` is an error; write `a + (b * c)`. A chain of operators
that bind equally needs none, so `a + b - c` and `x && y && z` are unchanged.

The classes are `*` `/` `%`; `+` `-`; `++`; `<<` `>>`; `<` `<=` `>` `>=`;
`==` `!=`; `&`; `^`; `|`; `&&`; `||`. Concatenation is its own class, so
`"n = " ++ (count as Text)` stays explicit about what is concatenated and what is
arithmetic.

## Algebraic Data Types

### Records

Records are nominal product types:

```midori
type Point =
{
    x: Int,
    y: Int
};

type Box<T> =
{
    value: T
};
```

### Unions

Unions are nominal tagged sums:

```midori
type Option<T> = None | Some(T);
type Result<T, E> = Ok(T) | Err(E);
```

### Constructor Type Argument Inference

A generic constructor takes its type arguments from the constructor arguments and the
surrounding expected type. There is no form for stating them, so every type parameter has
to be reachable from one of those two:

```midori
def some_int = Option::Some(42);
def empty_int : Option<Int> = Option::None();

def Wrap = fn<T>(value: T) -> Option<T> => Option::Some(value);
```

A zero-arity variant has no argument to infer from, so it needs an expected type. An
annotation on the binding is the usual way to supply one, as `empty_int` does above.

Inference must resolve every omitted type parameter. If no argument or expected type pins it down, construction fails:

```midori-test name=type-system/unresolved_none kind=failure path=.doc_examples/type_system/unresolved_none.mdr module=TypeSystemUnresolvedNone
type Option<T> = None | Some(T);
def unresolved = Option::None();  // error: missing type context
```

### Deriving

Midori supports `deriving` on structs and unions for a focused set of generated operations.

Structural deriving:

```midori
type Point =
{
    x: Int,
    y: Int
} deriving (Equatable, Hashable);
```

Container deriving on unions:

```midori
type OptionBox<T> = Empty | Full(T) deriving (Map, Bind, Unwrap);

def mapped = OptionBoxMap(OptionBox::Full(5), fn(x) => { x + 1 });
```

Transferable deriving for concurrency:

```midori
type Point =
{
    x: Float,
    y: Float
} deriving (Transferable);

type Result = Ok(Int) | Err(Text) deriving (Transferable);
```

`Transferable` generates field-by-field serialization for structs and tag+payload serialization for unions. All fields/variants must themselves satisfy `Transferable`. Types that cannot be transferable (closures, ranges, `Worker<T>`) produce a compile-time constraint-failure error.

Current support is intentionally narrow:

- `Equatable`, `Hashable`, and `Transferable` are supported structural derives
- `Map`, `Bind`, and `Unwrap` are supported container derives
- Structural deriving is limited to non-generic, non-recursive structs and unions for `Equatable` and `Hashable`
- `Transferable` deriving works on structs and unions including recursive union shapes
- Container deriving maps only the first type parameter and supports pass-through variants, single-value variants, and recursive self fields

## Type Aliases

Type aliases are transparent compile-time names for existing types:

```midori
type UserId = Int;
type Name = Text;

type IntPair = Pair<Int, Int>;
type IntArray = Array<Int>;
```

Aliases can be imported and exported like other symbols. They are fully interchangeable with the underlying type and have no runtime cost.

## Generics and Constraints

Functions, structs, unions, and aliases can all be parameterized:

```midori
def identity = fn<T>(value: T) -> T => value;

type Pair<A, B> =
{
    first: A,
    second: B
};
```

Functions can require type class constraints:

```midori
class Show<T> {
    show: fn(value: T) -> Text;
};

def Display = fn<T>(value: T) -> Text where Show<T> => Show::show(value);
```

Type definitions can also carry constraints:

```midori
type Box<T> where Show<T> = {
    value: T
};

def ShowBox = fn<T>(box: Box<T>) -> Text => Show::show(box.value);
```

Constraints attached to a struct or union are checked when the type is instantiated and automatically propagate when a function accepts that type.

## Type Classes

Midori supports both single-parameter and multi-parameter type classes.

### Class Definitions and Instances

```midori
class Show<T> {
    show: fn(value: T) -> Text;
};

instance Show<Int> {
    def show = fn(value: Int) -> Text => value as Text;
};
```

Methods are accessed through qualified syntax:

```midori
def text = Show::show(42);
```

### Associated Types

Associated types let a class determine a related type from its instance head:

```midori
class Iterable<Iter>
{
    type Item;
    Next: fn(iter: Iter) -> Option<Item>;
};
```

Instances bind the associated type:

```midori
type Counter =
{
    current: Int,
    end: Int
};

instance Iterable<Counter>
{
    type Item = Int;

    def Next = fn(counter: Counter) -> Option<Int> => {
        if counter.current >= counter.end
        then Option::None()
        else {
            def value = counter.current;
            counter.current = counter.current + 1;
            Option::Some(value)
        }
    };
};
```

Use projection syntax to refer to an associated type in other signatures:

```midori
def NextValue = fn<Iter>(iter: Iter) -> Option<Iterable::Item<Iter>>
    where Iterable<Iter> => Iterable::Next(iter);
```

Associated types are especially useful when one type parameter logically determines another, while multi-parameter type classes remain available for other cases.

### Operator-Backed Type Classes

Several operators are wired into the type checker and code generator so they can dispatch through type classes when the operands are not handled entirely as builtins.

- `as` can use `Convertable<From, To>`
- `++` can use `Concatenable<T>`
- `#` can use `Countable<T>`
- `==` and `!=` can use `Equatable<T>`
- `<`, `<=`, `>`, and `>=` can use `Orderable<T>`

Examples:

```midori
def ConvertIt = fn<From, To>(value: From) -> To
    where Convertable<From, To> => {
    value as To
};

def Join = fn<T>(left: T, right: T) -> T
    where Concatenable<T> => {
    left ++ right
};
```

The current implementation mixes direct builtin lowering with these hooks:

- `as` prefers direct builtin casts for concrete primitive conversions, but generic and constrained code can lower through `Convertable`
- `++` is builtin for `Text` and `Array<T>`, and can also lower through `Concatenable`
- `#` is builtin for arrays, has specialized lowering for prelude `List`, `Map`, and `Set` shapes, and otherwise falls back to `Countable`
- equality and ordering use builtin lowering for the primitive cases and typeclass dispatch for user-defined cases

The shipped prelude provides the following related modules:

- `Convertable`
- `Concatenable`
- `Countable`
- `Equatable`
- `Orderable`

The concrete coverage is intentionally uneven today. For example, `Orderable` is provided as a class surface, but most interesting instances are still expected to come from user code rather than the prelude.

### Compound Assignment Surface

In addition to `+=`, `-=`, `*=`, `/=`, and `%=`, the current language surface also includes:

- `&=`
- `|=`
- `^=`
- `<<=`
- `>>=`

These are currently defined for integer-style numeric types (`Int`, `Byte`, and `Word`) where appropriate.

## Pattern Matching

Pattern matching is expression-oriented:

```midori
def Unwrap = fn(option: Option<Int>) -> Int =>
    match option with
        case Option::Some(value) => value
        case Option::None() => 0;
```

### Exhaustiveness Rules

`match` expressions are checked for exhaustiveness:

- **Union scrutinees** must cover every variant unless a catch-all arm is present
- **`Bool` scrutinees** must cover both `true` and `false` unless a catch-all arm is present
- **Other scrutinee types** require a catch-all arm

A catch-all arm is an unguarded `case` whose pattern always matches: `case _`, a
bare binding such as `case n`, or a tuple of those. There is no `default` keyword.

Coverage is currently tracked at the top-level pattern. For unions, matching a variant counts as covering that variant even if nested sub-patterns are not themselves exhaustive.

### Pipe Into Match

The pipe operator can feed directly into a `match`:

```midori
def pipeline_result =
    5
    |> fn(x) => { x + 1 }
    |> Transform
    |> match with
        case Result::Ok(v) => v
        case Result::Err(_) => 0;
```

This is equivalent to matching on the result of the previous pipeline stage.
In pattern position, `_` is a wildcard that ignores the matched value and does not bind a local. Outside patterns, `_` remains a valid identifier.

## Type Inference

Midori uses Hindley-Milner inference with unification and an occurs check.

### Core Inference Flow

1. Fresh type variables are created for unknown types.
2. Constraints are collected while traversing the AST.
3. Unification solves those constraints.
4. Substitutions are applied to produce the final types.

### Bidirectional Inference

Expected-type context is used in two especially common places:

- **Constructors** infer omitted generic arguments from arguments and surrounding type context
- **Lambdas** may omit parameter annotations and return types when a function type is already expected

Examples:

```midori
def doubler : fn(Int) -> Int = fn(x) => { x * 2 };
def mapped = OptionMap(Option::Some(1), fn(x) => { x + 1 });
```

Lambdas still need a surrounding function type when annotations are omitted:

```midori-test name=type-system/lambda_missing_context kind=failure path=.doc_examples/type_system/lambda_missing_context.mdr module=TypeSystemLambdaMissingContext
def identity = fn(x) => { x };  // error: no expected function type
```

The fully explicit lambda syntax remains valid:

```midori
fn(x: Int) -> Int => { x + 1 }
```

## Special Types

### Never

`Never` is the bottom type for computations that do not produce a normal value, such as non-returning control-flow paths.

### Undecided Type

During parsing, omitted annotations can temporarily be represented as undecided type slots. These become inference variables during type checking.

## Implementation Architecture

The type system is implemented primarily in `src/Compiler/TypeChecker/` and the shared type representation in `src/Common/`.

### Type Representation

Types include:

- Primitive types
- Type variables
- Generic parameters
- Arrays, tuples, functions, structs, and unions
- Type class constraints and associated type projections

### Type Checker Responsibilities

The type checker handles:

- Scoped type environments
- Unification and substitution
- Freshening of polymorphic types
- Constraint solving for type classes
- Associated type resolution
- Exhaustiveness checking for `match`

### Key Runtime-Independent Registries

- **Type environment stack** for scoped bindings
- **Type substitution map** for inference variables
- **Class registry** for type class declarations and associated types
- **Instance registry** for concrete instance resolution
- **Active constraint set** for function and type-definition constraints

## Limitations

Current limitations include:

1. **No higher-kinded types**: abstractions like `Functor` and `Monad` cannot be expressed directly.
2. **Nominal ADTs**: structs and unions compare by name rather than by structure.
3. **No GADTs or general type-level functions**: associated types are supported, but richer type-level computation is not.
4. **No existential types**: type parameters cannot be hidden inside values.
5. **No rank-N polymorphism**: polymorphic types cannot appear in arbitrary positions.
6. **Top-level exhaustiveness only**: nested-pattern exhaustiveness is not fully checked yet.
7. **Focused deriving support**: only the currently supported targets and shapes are derivable.

## Future Considerations

Likely extensions include:

- Richer unification error messages
- Deeper nested-pattern exhaustiveness analysis
- Broader deriving support
- Default method implementations for type classes
