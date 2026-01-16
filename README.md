# ミドリ (Midori)

A statically-typed functional programming language featuring algebraic data types, pattern matching, typeclasses, and a module system. Compiles to bytecode for the Midori Virtual Machine with garbage-collected memory management.

## Key Features

- **Static Type System** - Strong static typing with type inference and generics
- **Pattern Matching** - Exhaustive pattern matching on algebraic data types
- **Algebraic Data Types** - Structs (product types) and unions (sum types)
- **Type Aliases** - Create readable names for complex types
- **Typeclasses** - Haskell-style constrained generics for polymorphism
- **Module System** - Explicit imports/exports with privacy enforcement
- **Pipe Operator** - Functional composition with `|>` operator
- **Ranges** - Elegant `start..step..end` syntax for loops
- **Closures** - First-class functions with lexical scoping
- **Async/Await** - Concurrent execution with `Future<T>` types
- **Expression-Oriented** - Everything is an expression with a value

## Quick Start

## Installation (Windows)
```powershell
# From the repo root (after building Midori.exe):
python .\scripts\install.py --copy-binaries

# This prefers a Release build if present (out/build/x64-release),
# otherwise falls back to Development/Debug. Use --preset to force:
# python .\scripts\install.py --copy-binaries --preset x64-release

# Uninstall:
# python .\scripts\uninstall.py
```

### Hello World
```midori
// Path import (relative or absolute)
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

// For loop with arrays
def names = ["Alice", "Bob", "Charlie"];
for name in names {
    IO::PrintLine(name);
};

// Array length operator
def arr = [1, 2, 3, 4, 5];
def len = #arr;  // 5

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

### Type Aliases
```midori
// Basic type aliases
type UserId = Int;
type Name = Text;

def user_id: UserId = 42;
def user_name: Name = "Alice";

// Type alias for struct
struct Point { x: Float, y: Float };
type Position = Point;

def pos: Position = new Point(10.0, 20.0);

// Generic type alias
struct Pair<A, B> { first: A, second: B };
type IntPair = Pair<Int, Int>;

def coords: IntPair = new Pair(1, 2);
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
// Path import (relative or absolute)
import { "./MyModule.mdr" }

// Or search in MIDORI_PATH (semicolon-separated on Windows, colon-separated on Unix)
// import { <MyModule> }

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

### Async/Await
```midori
import { <IO> }

defun compute(x: Int) : Int => x * x;

// Spawn concurrent tasks (run in separate VMs with their own heaps)
def task1 : Future<Int> = async compute(10);
def task2 : Future<Int> = async compute(20);

// Await results (blocks until complete, deep-copies return values)
def r1 : Int = await task1;
def r2 : Int = await task2;

IO::PrintLine("Results: " ++ (r1 as Text) ++ ", " ++ (r2 as Text));
```

> **Warning**: Concurrent mutation of captured mutable references (e.g., arrays) is undefined behavior. See [Async/Await docs](docs/async-await.md) for details.

## Language Features

### Type System
- **Primitive Types**: `Int`, `Float`, `Bool`, `Text`, `Unit`
- **Composite Types**: `Array<T>`, `Future<T>`, structs, unions
- **Function Types**: `fn(T1, T2) -> R`
- **Type Aliases**: `type UserId = Int;` for readable type names
- **Generic Parameters**: Single and multiple type parameters
- **Type Constraints**: Class constraints with `where`
- **Type Inference**: Automatic type deduction at instantiation

#### Numeric Limits

**Integer (Int)** - 64-bit signed integer:
- Maximum value: `9223372036854775807` (2^63 - 1)
- Minimum value: `-9223372036854775807` (practical limit)
- **Note**: The literal `-9223372036854775808` (INT64_MIN) causes a parser overflow and should be avoided

**Float** - Double-precision floating point:
- Range: Approximately ±1.7E+308
- Precision: ~15-17 decimal digits

**Unicode Text Support**:
- Full UTF-8 encoding support
- String length returns code point count (not byte count)
- Supports multi-byte characters from all Unicode planes

### Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Comparison**: `==`, `!=`, `<`, `>`, `<=`, `>=`
- **Logical**: `&&`, `||`, `!`
- **Bitwise**: `&`, `|`, `^`, `<<`, `>>`
- **String**: `++` (concatenation)
- **Pipe**: `|>` (function composition)
- **Length**: `#` (array length)
- **Compound Assignment**: `+=`, `-=`, `*=`, `/=`, `%=`, `++=`

### Advanced Features
- **Recursive Data Types**: Self-referential unions for lists, trees
- **Exhaustive Matching**: Compiler-enforced pattern coverage
- **Range Expressions**: `start..step..end` with positive/negative steps
- **Array Iteration**: `for x in array` iterates over elements
- **Float Ranges**: Support for decimal step values
- **Nested Generics**: Complex generic type compositions
- **Cross-Module Classes**: Import and use classes across modules
- **Async/Await**: Spawn concurrent tasks with `async`, retrieve results with `await`

## Standard Library

The `MidoriPrelude` directory contains standard modules:

- **IO.mdr** - Input/output operations (`IO::PrintLine`)
- **Math.mdr** - Mathematical functions
- **DateTime.mdr** - Timing and date operations

## Foreign Function Interface (FFI)

Midori supports calling external C/C++ functions through its Foreign Function Interface, enabling integration with native libraries and system APIs.

### Declaring Foreign Functions

Use the `foreign` keyword to declare external functions:

```midori
// Declare a foreign function
foreign "MIDORI_FFI_Print" Print : fn(Text) -> Unit;

// With multiple parameters
foreign "MIDORI_FFI_WriteFile" WriteFile : fn(Text, Text) -> Bool;

// Returning complex types
foreign "MIDORI_FFI_ReadBinaryFile" ReadBinaryFile : fn(Text) -> Array<Byte>;
```

### Supported Types

**Primitive Types** (passed by value):
- `Int` (64-bit signed integer)
- `Float` (64-bit double)
- `Bool` (boolean)
- `Byte` (8-bit unsigned integer)
- `Word` (64-bit unsigned integer)
- `Unit` (empty/void)

**Heap Types** (automatically marshalled):
- `Text` - VM passes C-string pointer, FFI returns `malloc`'d C-string
- `Array<T>` - VM passes struct pointer, FFI returns struct pointer

### FFI Function Signature

All FFI functions must follow this signature:

```cpp
extern "C" {
    MIDORI_STDLIB_API void MIDORI_FFI_FunctionName(void** args, void* ret) noexcept;
}
```

**Parameters:**
- `args`: Array of pointers to arguments (indexed by parameter position)
- `ret`: Pointer to 8-byte return value buffer

### Type Marshalling

#### Primitive Types

**Receiving Arguments:**
```cpp
// Int, Float, Byte, Word
int64_t value;
std::memcpy(&value, args[0], sizeof(int64_t));

// Bool
bool flag;
std::memcpy(&flag, args[0], sizeof(bool));
```

**Returning Values:**
```cpp
// Int
int64_t result = 42;
std::memcpy(ret, &result, sizeof(int64_t));

// Bool
bool success = true;
std::memcpy(ret, &success, sizeof(bool));

// Unit (void)
std::memset(ret, 0, sizeof(double));
```

#### Text Type

**Receiving Text Arguments:**
```cpp
// VM passes const char* directly
const char* str = reinterpret_cast<const char*>(args[0]);
```

**Returning Text:**
```cpp
// Allocate with malloc (NOT new)
char* result = static_cast<char*>(std::malloc(size));
std::memcpy(result, data, size);

// Return pointer as int64_t
const int64_t ptr = reinterpret_cast<int64_t>(result);
std::memcpy(ret, &ptr, sizeof(int64_t));

// VM will copy to GC memory and free() the result
```

#### Array Type

**Receiving Array Arguments:**
```cpp
struct ArrayArgument {
    void* data;    // Pointer to array of MidoriValue (8 bytes each)
    int length;    // Number of elements
};

ArrayArgument* array = reinterpret_cast<ArrayArgument*>(args[0]);
double* elements = reinterpret_cast<double*>(array->data);

// Access elements
for (int i = 0; i < array->length; i++) {
    int64_t value;
    std::memcpy(&value, &elements[i], sizeof(double));
    // Use value...
}
```

**Returning Arrays:**
```cpp
struct FFIArray {
    void* data;    // Pointer to array of doubles (8 bytes each)
    int length;
};

// Allocate array data
double* array_data = static_cast<double*>(std::malloc(length * sizeof(double)));

// Fill array
for (int i = 0; i < length; i++) {
    int64_t value = i * 10;
    std::memcpy(&array_data[i], &value, sizeof(double));
}

// Allocate return struct
FFIArray* result = static_cast<FFIArray*>(std::malloc(sizeof(FFIArray)));
result->data = array_data;
result->length = length;

// Return pointer
const int64_t ptr = reinterpret_cast<int64_t>(result);
std::memcpy(ret, &ptr, sizeof(int64_t));

// VM will copy to GC memory and free both struct and data
```

### Memory Management Rules

**Critical Rules:**

1. **Use `malloc`/`free`, NOT `new`/`delete`**: FFI allocations are freed by the VM using `std::free()`

2. **Heap Returns are Copied**: VM copies FFI-allocated Text/Array data into GC-managed memory, then immediately frees FFI allocation

3. **No GC Access**: FFI functions cannot access the VM's garbage collector or internal types

4. **8-Byte Limit**: All return values must fit in 8 bytes (`sizeof(double)`)

**Memory Flow:**
```
FFI: malloc() → return pointer
 ↓
VM: copy to GC memory → free() FFI allocation
 ↓
GC: manage lifetime
```

### Complete Example

**Midori Declaration:**
```midori
// In your module
foreign "MIDORI_FFI_ReadBinaryFile" ReadBinaryFile : fn(Text) -> Array<Byte>;

// Usage
def data = ReadBinaryFile("file.bin");
IO::Print((data[0] as Int) as Text);
```

**C++ Implementation:**
```cpp
#include "Library/MidoriStdLibExports.h"
#include <fstream>
#include <vector>

extern "C" {
    MIDORI_STDLIB_API void MIDORI_FFI_ReadBinaryFile(void** args, void* ret) noexcept
    {
        struct FFIArray {
            void* data;
            int length;
        };

        const char* file_path = reinterpret_cast<const char*>(args[0]);

        std::ifstream file(file_path, std::ios::binary);
        if (!file.is_open()) {
            const int64_t null_ptr = 0;
            std::memcpy(ret, &null_ptr, sizeof(int64_t));
            return;
        }

        // Read file
        file.seekg(0, std::ios::end);
        const std::streamsize size = file.tellg();
        file.seekg(0, std::ios::beg);

        std::vector<char> buffer(size);
        file.read(buffer.data(), size);

        // Allocate array (MidoriValue = 8 bytes each)
        double* array_data = static_cast<double*>(
            std::malloc(size * sizeof(double))
        );

        // Convert bytes to array elements
        for (std::streamsize i = 0; i < size; i++) {
            const int64_t byte = static_cast<uint8_t>(buffer[i]);
            std::memcpy(&array_data[i], &byte, sizeof(double));
        }

        // Create return struct
        FFIArray* result = static_cast<FFIArray*>(std::malloc(sizeof(FFIArray)));
        result->data = array_data;
        result->length = static_cast<int>(size);

        const int64_t ptr = reinterpret_cast<int64_t>(result);
        std::memcpy(ret, &ptr, sizeof(int64_t));
    }
}
```

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

## Architecture

- **Frontend**: Lexer → Parser → Type Checker
- **Optimizer**: Constant folding, tail call optimization, strength reduction
- **Backend**: Bytecode generator → Linker
- **Runtime**: Stack-based VM with per-VM mark-and-sweep GC

See [Runtime Architecture](docs/runtime-architecture.md) for details on the per-VM memory model.

## Documentation

See the [docs](docs/) folder for detailed technical documentation:

- [Type System](docs/type-system.md) - Type inference, type classes, and algebraic data types
- [Compilation Workflow](docs/compilation-workflow.md) - Complete pipeline from lexing to linking
- [Async/Await](docs/async-await.md) - Concurrent execution with per-VM isolation
- [Runtime Architecture](docs/runtime-architecture.md) - Memory model, garbage collection, and concurrency
