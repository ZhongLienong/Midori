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

## Algebraic Data Types

### Structs

Structs are nominal product types:

```midori
struct Point
{
    x: Int,
    y: Int
};

struct Box<T>
{
    value: T
};
```

### Unions

Unions are nominal tagged sums:

```midori
union Option<T> = None | Some(T);
union Result<T, E> = Ok(T) | Err(E);
```

### Constructor Type Argument Inference

Generic constructors accept explicit type arguments, but Midori can infer omitted arguments from constructor arguments and the surrounding expected type:

```midori
def some_int = new Option::Some(42);
def empty_int : Option<Int> = new Option::None();

defun Wrap<T>(value: T) : Option<T> => new Option::Some(value);
```

The explicit form remains available:

```midori
def exact = new Option::Some<Int>(42);
```

Inference must resolve every omitted type parameter. If no argument or expected type pins it down, construction fails:

```midori
def unresolved = new Option::None();  // error: missing type context
```

### Deriving

Midori supports `deriving` on structs and unions for a focused set of generated operations.

Structural deriving:

```midori
struct Point
{
    x: Int,
    y: Int
} deriving (Equatable, Hashable);
```

Container deriving on unions:

```midori
union OptionBox<T> = Empty | Full(T) deriving (Map, Bind, Unwrap);

def mapped = OptionBoxMap(new OptionBox::Full(5), fn(x) => { x + 1 });
```

Current support is intentionally narrow:

- `Equatable` and `Hashable` are supported structural derives
- `Map`, `Bind`, and `Unwrap` are supported container derives
- Structural deriving is limited to non-generic, non-recursive structs and unions
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
defun identity<T>(value: T) : T => value;

struct Pair<A, B>
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

defun Display<T>(value: T) : Text where Show<T> => {
    return Show::show(value);
};
```

Type definitions can also carry constraints:

```midori
struct Box<T> where Show<T> {
    value: T
};

defun ShowBox<T>(box: Box<T>) : Text => {
    return Show::show(box.value);
};
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
    defun show(value: Int) : Text => {
        return value as Text;
    };
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
struct Counter
{
    current: Int,
    end: Int
};

instance Iterable<Counter>
{
    type Item = Int;

    defun Next(counter: Counter) : Option<Int> => {
        if counter.current >= counter.end
        then new Option::None()
        else {
            def value = counter.current;
            counter.current = counter.current + 1;
            new Option::Some(value)
        }
    };
};
```

Use projection syntax to refer to an associated type in other signatures:

```midori
defun NextValue<Iter>(iter: Iter) : Option<Iterable::Item<Iter>>
    where Iterable<Iter> => {
    return Iterable::Next(iter);
};
```

Associated types are especially useful when one type parameter logically determines another, while multi-parameter type classes remain available for other cases.

## Pattern Matching

Pattern matching is expression-oriented:

```midori
defun Unwrap(option: Option<Int>) : Int => {
    return match option with
        case Option::Some(value) => value
        case Option::None() => 0
    ;
};
```

### Exhaustiveness Rules

`match` expressions are checked for exhaustiveness:

- **Union scrutinees** must cover every variant unless a `default` arm is present
- **`Bool` scrutinees** must cover both `true` and `false` unless a `default` arm is present
- **Other scrutinee types** require `default`

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
def mapped = OptionMap(new Option::Some(1), fn(x) => { x + 1 });
```

Lambdas still need a surrounding function type when annotations are omitted:

```midori
def identity = fn(x) => { x };  // error: no expected function type
```

The fully explicit lambda syntax remains valid:

```midori
fn(x: Int) : Int => { x + 1 }
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
