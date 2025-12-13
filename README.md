# ミドリ (Midori)

A statically-typed functional programming language featuring algebraic data types, pattern matching, typeclasses, and a module system. Compiles to bytecode for the Midori Virtual Machine with garbage-collected memory management.

## Key Features

- **Static Type System** - Strong static typing with type inference and generics
- **Pattern Matching** - Exhaustive pattern matching on algebraic data types
- **Algebraic Data Types** - Structs (product types) and unions (sum types)
- **Typeclasses** - Haskell-style constrained generics for polymorphism
- **Module System** - Explicit imports/exports with privacy enforcement
- **Pipe Operator** - Functional composition with `|>` operator
- **Ranges** - Elegant `start..step..end` syntax for loops
- **Closures** - First-class functions with lexical scoping
- **Expression-Oriented** - Everything is an expression with a value

## Quick Start

### Hello World
```midori
import { "../MidoriPrelude/IO.mdr" }

IO::PrintLine("Hello, Midori!");
```

### Basic Types & Variables
```midori
def number : Int = 42;
def pi : Float = 3.14159;
def message : Text = "Hello";
def flag : Bool = true;
def items : Array<Int> = [1, 2, 3, 4, 5];
```

### Functions
```midori
// Simple function
defun square(x: Int) : Int => {
    return x * x;
};

// Function with type inference
defun add(a: Int, b: Int) : Int => a + b;

// Generic function
defun identity<T>(value: T) : T => value;

// Higher-order function
defun apply<T, R>(fn: fn(T) -> R, value: T) : R => {
    return fn(value);
};
```

### Control Flow
```midori
// If-else expression
def result = if x > 0 then "positive" else "non-positive";

// For loop with ranges
for i in 0..1..10 {
    IO::PrintLine(i as Text);
};

// Loop with break
def sum = loop {
    if count >= 10 then break total else ();
    total = total + count;
    count = count + 1;
};
```

### Structs (Product Types)
```midori
struct Point {
    x: Float,
    y: Float,
};

struct Box<T> {
    value: T,
};

def origin = new Point(0.0, 0.0);
def boxed = new Box(42);
def x_coord = origin.x;
```

### Unions (Sum Types)
```midori
union Option<T> = Some(T) | None;

union List<T> = Cons(T, List<T>) | Nil;

def maybe_value = new Option::Some(42);
def empty_list = new List::Nil();
```

### Pattern Matching
```midori
union Result<T, E> = Ok(T) | Err(E);

defun handle_result<T>(result: Result<T, Text>) : Text => {
    return match result with
        case Result::Ok(value) => "Success: " ++ (value as Text)
        case Result::Err(msg) => "Error: " ++ msg
        default => "Unknown"
    ;
};
```

### Classes
```midori
// Define a class
class Show<T> {
    show: fn(value: T) -> Text;
};

// Implement for Int
instance Show<Int> {
    defun show(value: Int) : Text => {
        return value as Text;
    };
};

// Use with constraints
defun display<T>(value: T) : Text where Show<T> => {
    return Show::show(value);
};

def message = display(42);  // "42"
```

### Module System
```midori
// Define a module (MyModule.mdr)
module MyModule
public export { add, multiply }

defun add(a: Int, b: Int) : Int => a + b;
defun multiply(a: Int, b: Int) : Int => a * b;
defun internal() : Int => 100;  // Not exported

// Use in another file
import { "./MyModule.mdr" }

def result = MyModule::add(5, 3);
```

### Pipe Operator
```midori
defun double(x: Int) : Int => x * 2;
defun add_ten(x: Int) : Int => x + 10;

def result = 5
    |> double
    |> add_ten
    |> double;  // ((5 * 2) + 10) * 2 = 40
```

### Closures
```midori
defun make_counter() : fn() -> Int => {
    def count = 0;
    return fn() : Int => {
        count = count + 1;
        return count;
    };
};

def counter = make_counter();
def first = counter();   // 1
def second = counter();  // 2
```

## Language Features

### Type System
- **Primitive Types**: `Int`, `Float`, `Bool`, `Text`, `Unit`
- **Composite Types**: `Array<T>`, structs, unions
- **Function Types**: `fn(T1, T2) -> R`
- **Generic Parameters**: Single and multiple type parameters
- **Type Constraints**: Class constraints with `where`
- **Type Inference**: Automatic type deduction at instantiation

### Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logical**: `&&`, `||`, `!`
- **Bitwise**: `&`, `|`, `^`, `<<`, `>>`
- **String**: `++` (concatenation)
- **Pipe**: `|>` (function composition)
- **Compound Assignment**: `+=`, `-=`, `*=`, `/=`, `%=`, `++=`

### Advanced Features
- **Recursive Data Types**: Self-referential unions for lists, trees
- **Exhaustive Matching**: Compiler-enforced pattern coverage
- **Range Expressions**: `start..step..end` with positive/negative steps
- **Float Ranges**: Support for decimal step values
- **Nested Generics**: Complex generic type compositions
- **Cross-Module Classes**: Import and use classes across modules

## Standard Library

The `MidoriPrelude` directory contains standard modules:

- **IO.mdr** - Input/output operations (`IO::PrintLine`)
- **Math.mdr** - Mathematical functions
- **Array.mdr** - Array utilities
- **DateTime.mdr** - Timing and date operations

## Development

### Building Midori

Midori uses a three-tier build configuration:

**Debug** - Full diagnostics with AST dumps and bytecode disassembly:
```bash
cmake --build build --config Debug
```

**Development** - Optimized build with compilation progress:
```bash
cmake --build build --config Development
```

**Release** - Maximum performance with minimal output:
```bash
cmake --build build --config Release
```

### Running Programs

```bash
# Run a Midori program
./build/Midori.exe path/to/program.mdr

# With debug output
./build/Debug/Midori.exe path/to/program.mdr
```

### Running Tests

Run all tests:
```bash
python scripts/run_tests.py
```

Run specific tests:
```bash
python scripts/run_tests.py --test closure/simple.mdr
python scripts/run_tests.py --category typeclass
python scripts/run_tests.py --pattern recursive
```

Create new tests:
```bash
python scripts/new_test.py closure/my_test
python scripts/new_test.py expression/failure/syntax_error --should-fail
```

## Example Programs

### Recursive Fibonacci
```midori
defun fib(n: Int) : Int => {
    return if n <= 1 then n else fib(n - 1) + fib(n - 2);
};
```

### Binary Tree
```midori
union Tree<T> = Leaf(T) | Node(Tree<T>, Tree<T>);

defun height<T>(tree: Tree<T>) : Int => {
    return match tree with
        case Tree::Leaf(value) => 1
        case Tree::Node(left, right) => {
            def left_height = height(left);
            def right_height = height(right);
            return 1 + (if left_height > right_height
                        then left_height
                        else right_height);
        }
        default => 0
    ;
};
```

### Generic Linked List
```midori
union List<T> = Cons(T, List<T>) | Nil;

defun length<T>(list: List<T>) : Int => {
    return match list with
        case List::Cons(head, tail) => 1 + length(tail)
        case List::Nil => 0
        default => 0
    ;
};

defun map<A, B>(list: List<A>, f: fn(A) -> B) : List<B> => {
    return match list with
        case List::Cons(head, tail) =>
            new List::Cons(f(head), map(tail, f))
        case List::Nil =>
            new List::Nil()
        default =>
            new List::Nil()
    ;
};
```

### Quicksort
```midori
defun quicksort(arr: Array<Int>) : Array<Int> => {
    if arr == [] then return [];

    def pivot = arr[0];
    def less = [];
    def greater = [];

    for i in 1..1..((arr |> length) - 1) {
        if arr[i] < pivot then
            less = less ++ [arr[i]]
        else
            greater = greater ++ [arr[i]];
    };

    return quicksort(less) ++ [pivot] ++ quicksort(greater);
};
```

## Architecture

- **Frontend**: Lexer → Parser → Type Checker
- **Optimizer**: Constant folding, tail call optimization, strength reduction
- **Backend**: Bytecode generator → Linker
- **Runtime**: Stack-based VM with mark-and-sweep GC
