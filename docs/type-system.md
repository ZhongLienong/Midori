# Midori Type System

Midori implements a **Hindley-Milner (HM) based type system** with extensions for type classes, algebraic data types, and parametric polymorphism. The implementation follows **Algorithm W**, the classic inference algorithm for ML-family languages.

## Overview

The type system provides:

- **Full type inference** - Type annotations are optional in most cases
- **Parametric polymorphism** - Generic functions and data types
- **Ad-hoc polymorphism** - Haskell-style type classes
- **Algebraic data types** - Structs (product types) and unions (sum types)
- **Type aliases** - Alternative names for existing types
- **Static type safety** - All type errors are caught at compile time

## Primitive Types

| Type | Description |
|------|-------------|
| `Int` | 64-bit signed integer |
| `Float` | 64-bit floating point (IEEE 754 double) |
| `Byte` | 8-bit unsigned integer |
| `Word` | 64-bit unsigned integer |
| `Bool` | Boolean (`true` or `false`) |
| `Text` | UTF-8 string |
| `Unit` | Unit type (similar to `void`) |

## Composite Types

### Arrays

Homogeneous, dynamically-sized collections:

```
[Int]           -- Array of integers
[[Text]]        -- Array of arrays of text
```

**Length Operator**: Use `#` to get array length:
```
def arr = [1, 2, 3, 4, 5];
def len = #arr;  -- 5
```

**Array Iteration**: Arrays can be used directly in for-loops:
```
def names = ["Alice", "Bob", "Charlie"];
for name in names {
    PrintLine(name);  -- Iterates over elements
};
```

### Tuples

Fixed-size, heterogeneous collections:

```
(Int, Text)           -- Pair of integer and text
(Bool, Int, Float)    -- Triple
```

### Functions

First-class function types:

```
fn(Int) -> Bool              -- Function taking Int, returning Bool
fn(Int, Int) -> Int          -- Function taking two Ints
fn() -> Unit                 -- Thunk (no parameters)
fn(fn(Int) -> Int) -> Int    -- Higher-order function
```

### Ranges

Iterator types for numeric sequences:

```
Int..Int      -- Integer range
Float..Float  -- Float range
```

Ranges are created with the `start..step..end` syntax and used in for-loops:
```
for i in 0..1..10 {    -- 0, 1, 2, ..., 9
    PrintLine(i as Text);
};

for i in 10..-1..0 {   -- 10, 9, 8, ..., 1 (backward)
    PrintLine(i as Text);
};
```

Both ranges and arrays are valid iterables for `for` expressions.

### Futures

`Future<T>` represents a value that will be available asynchronously:

```
Future<Int>     -- Future resolving to an integer
Future<Text>    -- Future resolving to text
Future<Array<Int>>  -- Future resolving to an array
```

Futures are created with `async` and consumed with `await`:

```
def task : Future<Int> = async expensive_computation();
def result : Int = await task;
```

**Key Properties:**
- `async expr` spawns a concurrent task and returns `Future<T>` where `T` is the type of `expr`
- `await future` blocks until the future completes and returns the unwrapped value of type `T`
- Multiple futures can execute concurrently
- Each async task runs in an isolated VM with its own stack and garbage collector

See [Async/Await](async-await.md) for detailed documentation on concurrent execution.

## Algebraic Data Types

### Structs (Product Types)

Named record types with labeled fields:

```
struct Point {
    x: Int,
    y: Int
}

struct Person {
    name: Text,
    age: Int
}
```

### Unions (Sum Types)

Tagged union types (discriminated unions):

```
union Option<T> {
    Some(T),
    None
}

union Result<T, E> {
    Ok(T),
    Err(E)
}
```

## Type Aliases

Type aliases create alternative names for existing types, improving code readability and maintainability:

### Basic Type Aliases

```
type UserId = Int;
type Name = Text;
type Score = Float;

def user: UserId = 42;
def name: Name = "Alice";
```

### Struct Type Aliases

```
struct Point {
    x: Float,
    y: Float
}

type Position = Point;
type Coordinate = Point;

def pos: Position = new Point(10.0, 20.0);
```

### Generic Type Aliases

Type aliases can include generic parameters:

```
struct Pair<A, B> {
    first: A,
    second: B
}

type IntPair = Pair<Int, Int>;
type StringPair = Pair<Text, Text>;

def coords: IntPair = new Pair(10, 20);
```

### Type Aliases in Modules

Type aliases can be exported and imported like other symbols:

```
// In library.mdr
module MyLib
public export { UserId, Name, createUser }

type UserId = Int;
type Name = Text;

defun createUser(id: UserId, name: Name) : (UserId, Name) => (id, name);
```

```
// In main.mdr
import { "./library.mdr" }
use MyLib.{UserId, Name, createUser}

def id: UserId = 42;
def name: Name = "Bob";

// Qualified access also works
def id2: MyLib::UserId = 100;
```

### Type Alias Semantics

- Type aliases are **transparent** - they are fully interchangeable with their underlying type
- Type aliases are resolved at **compile time** - no runtime overhead
- Type aliases can reference other type aliases
- Type aliases cannot be recursive (no `type List = (Int, List)`)

## Generics (Parametric Polymorphism)

Functions, structs, and unions can be parameterized over types:

```
fn identity<T>(x: T) -> T {
    x
}

fn map<A, B>(arr: [A], f: (A) -> B) -> [B] {
    -- implementation
}

struct Pair<A, B> {
    first: A,
    second: B
}
```

Generic type parameters are instantiated at call sites through type inference.

## Type Classes (Ad-hoc Polymorphism)

Type classes enable overloading and constrained polymorphism, similar to Haskell or Rust traits.

### Defining a Type Class

```
class Eq<T> {
    fn eq(self: T, other: T) -> Bool;
}

class Ord<T> : Eq<T> {
    fn lt(self: T, other: T) -> Bool;
    fn gt(self: T, other: T) -> Bool;
}
```

### Implementing Instances

```
instance Eq<Int> {
    fn eq(self: Int, other: Int) -> Bool {
        -- built-in integer equality
    }
}

instance Eq<Point> {
    fn eq(self: Point, other: Point) -> Bool {
        self.x == other.x && self.y == other.y
    }
}
```

### Constrained Polymorphism

Functions can require type class constraints:

```
fn max<T : Ord>(a: T, b: T) -> T {
    if a.gt(b) { a } else { b }
}
```

## Type Inference

Midori uses the Hindley-Milner type inference algorithm, which guarantees:

1. **Principal types** - The most general type is always inferred
2. **Decidability** - Type inference always terminates
3. **Completeness** - If a valid typing exists, it will be found

### How It Works

The inference process consists of several phases:

#### 1. Fresh Type Variables

When encountering an expression with unknown type, a fresh type variable is generated:

```
let x = [];   -- x : [?0] where ?0 is a fresh type variable
```

#### 2. Constraint Generation

As expressions are analyzed, equality constraints are collected:

```
let x = [1, 2, 3];  -- Generates constraint: ?0 = Int
```

#### 3. Unification

Constraints are solved through unification, which finds substitutions that make types equal:

```
Unify([?0], [Int])  -->  ?0 := Int
```

#### 4. Substitution Application

After solving, substitutions are applied to produce final types:

```
x : [Int]  -- After applying ?0 := Int
```

### Occurs Check

The type system prevents infinite types through the occurs check:

```
-- This would be rejected:
fn f(x) { f }  -- Would require: ?0 = (?0) -> ?1 (infinite type)
```

## Special Types

### Never Type

The `Never` type represents computations that don't return (bottom type):

- `return` expressions in non-tail position
- Infinite loops

### Undecided Type

Internal representation for type holes during parsing, converted to type variables during type checking.

## Implementation Architecture

The type system is implemented across several components:

### Type Representation (`Type.h`)

Types are represented as a variant (tagged union) of possible type forms:

- Primitive types (Int, Float, Bool, etc.)
- Type variables (for inference)
- Generic parameters (for polymorphism)
- Composite types (Array, Future, Function, Struct, Union, Tuple)
- Class constraints (for type classes)

### Type Checker (`TypeChecker.cpp`)

The type checker implements:

- **Environment management** - Scoped symbol tables mapping names to types
- **Unification** - Structural type equality with substitution
- **Freshening** - Instantiation of polymorphic types
- **Constraint solving** - Type class instance resolution

### Key Data Structures

- **Type Environment Stack** - Scoped mappings from variable names to types
- **Type Substitution** - Mapping from type variable IDs to their resolved types
- **Class Registry** - Registered type classes and their methods
- **Instance Registry** - Type class instances indexed by class name and concrete types
- **Active Constraints** - Type class constraints in the current scope

## Comparison with Other Systems

| Feature | Midori | Haskell | OCaml | Rust |
|---------|--------|---------|-------|------|
| HM Inference | Yes | Yes | Yes | Partial |
| Type Classes | Yes | Yes | No* | Yes (traits) |
| Higher-Kinded Types | No | Yes | Yes | No |
| Row Polymorphism | No | No | Yes | No |
| Subtyping | No | No | No | Yes (lifetimes) |
| GADTs | No | Yes | Yes | No |

*OCaml has modular implicits as an alternative

## Limitations

Current limitations of the type system:

1. **No higher-kinded types** - Cannot abstract over type constructors (no `Functor`, `Monad`)
2. **Nominal typing for ADTs** - Structs and unions are compared by name, not structure
3. **No type-level computation** - No type families or associated types
4. **No existential types** - Cannot hide type parameters in data types
5. **No rank-N polymorphism** - Polymorphic types cannot appear in arbitrary positions

## Future Considerations

Potential extensions that could be added:

- **Associated types** in type classes
- **Default method implementations**
- **Multi-parameter type classes**
- **Functional dependencies**
