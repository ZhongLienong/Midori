# ミドリ (Midori)

A statically-typed functional programming language featuring algebraic data types, pattern matching, typeclasses, and a module system. Compiles to bytecode for the Midori Virtual Machine with garbage-collected memory management.

## Key Features

- **Static Type System** - Strong static typing with bidirectional inference, generics, and constraints
- **Pattern Matching** - Exhaustive pattern matching on unions and `Bool`
- **Algebraic Data Types** - Structs (product types) and unions (sum types)
- **Type Aliases** - Create readable names for complex types
- **Typeclasses** - Constrained generics with associated types and instance dispatch
- **Deriving** - Generate structural and container helpers from type declarations
- **Module System** - Explicit imports/exports with privacy enforcement
- **Package System** - Third-party packages with native FFI bindings
- **Pipe Operator** - Functional composition with `|>`, inferred lambdas, and `|> match with`
- **Ranges** - Elegant `start..step..end` syntax for loops
- **Closures** - First-class functions with lexical scoping
- **Expression-Oriented** - Everything is an expression with a value

## Quick Start

## Installation (Windows)
```powershell
# From the repo root (after building Midori.exe):
python .\scripts\install.py --copy-binaries

# This prefers a Release preset build if present,
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
union Option<T> = None | Some(T);

union List<T> = Cons(T, List<T>) | Nil;

def maybe_value = new Option::Some(42);
def empty_value : Option<Int> = new Option::None();
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

### Associated Types
```midori
union Option<T> = None | Some(T);

class Iterable<Iter> {
    type Item;
    Next: fn(iter: Iter) -> Option<Item>;
};

defun NextValue<Iter>(iter: Iter) : Option<Iterable::Item<Iter>>
    where Iterable<Iter> => {
    return Iterable::Next(iter);
};
```

### Deriving
```midori
struct Point {
    x: Int,
    y: Int
} deriving (Equatable, Hashable);

union OptionBox<T> = Empty | Full(T) deriving (Map, Bind, Unwrap);
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

### Package System

Midori supports third-party packages with native FFI bindings. Packages can provide native libraries (DLL/SO/DYLIB) that are loaded dynamically at runtime.

**Package structure:**
```
PackageName/
├── package.midori       # TOML manifest
├── PackageName.mdr      # Module file
└── lib/                 # Native libraries
    └── windows/x64/packagename.dll
```

**Using a package:**
```bash
# Set MIDORI_PATH to include package directory
export MIDORI_PATH="/path/to/packages/PackageName:/path/to/MidoriPrelude"
```

```midori
import { <PackageName> }

def result = PackageName::NativeFunction(arg1, arg2);
```

See [Package System](docs/package-system.md) for complete documentation on creating and using packages.

### Pipe Operator
```midori
defun double(x: Int) : Int => x * 2;
union Result<T, E> = Ok(T) | Err(E);

defun transform(value: Int) : Result<Int, Text> => {
    if value > 10
    then new Result::Ok(value + 1)
    else new Result::Err("too small")
};

def result =
    5
    |> double
    |> fn(x) => { x + 1 }
    |> transform
    |> match with
        case Result::Ok(value) => value
        case Result::Err(_) => 0;
```

In pattern position, `_` is a wildcard that ignores the matched value and does not bind a local. Outside patterns, `_` remains a normal identifier.

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
- **Primitive Types**: `Int`, `Float`, `Byte`, `Word`, `Bool`, `Text`, `Unit`
- **Composite Types**: `Array<T>`, structs, unions
- **Function Types**: `fn(T1, T2) -> R`
- **Type Aliases**: `type UserId = Int;` for readable type names
- **Generic Parameters**: Single and multiple type parameters
- **Type Constraints**: `where` constraints on functions, structs, and unions
- **Associated Types**: Projections such as `Iterable::Item<Iter>`
- **Deriving**: `Equatable`, `Hashable`, `Map`, `Bind`, and `Unwrap`
- **Type Inference**: Automatic type deduction for instantiation, constructors, and context-aware lambdas

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
- **Concatenation**: `++` for `Text` and `Array<T>`
- **Pipe**: `|>` (function composition)
- **Length**: `#` (array length)
- **Compound Assignment**: `+=`, `-=`, `*=`, `/=`, `%=`

Concatenation assignment is explicit rather than a dedicated operator:

```midori
items = items ++ [value];
text = text ++ suffix;
Prependable::Prepend(items, value);
Appendable::Append(items, value);
Extendable::Extend(items, other_items);
```

### Advanced Features
- **Recursive Data Types**: Self-referential unions for lists, trees
- **Exhaustive Matching**: Compiler-enforced pattern coverage
- **Constructor Inference**: Generic constructor arguments can be inferred from arguments and context
- **Pipe Into Match**: Pipelines can flow directly into `match with`
- **Range Expressions**: `start..step..end` with positive/negative steps
- **Array Iteration**: `for x in array` iterates over elements
- **Float Ranges**: Support for decimal step values
- **Nested Generics**: Complex generic type compositions
- **Cross-Module Classes**: Import and use classes, instances, and associated types across modules

## Standard Library

`MidoriPrelude` is the standard-library layer that ships with the repo. The main module groups are:

- **Core ADTs** - `Prelude/Option.mdr`, `Prelude/Result.mdr`, `Prelude/List.mdr`
- **Collections** - `Collections/Map.mdr`, `Collections/Set.mdr`
- **Effects** - `IO.mdr`, `System.mdr`, `DateTime.mdr`
- **Built-in helpers** - `TextUtil.mdr`, `ArrayUtil.mdr`, `Math.mdr`

The public IO and system surface now prefers typed wrappers over sentinel values. Common entry points include:

- `IO::TryReadFile`, `IO::TryWriteFile`, `IO::TryAppendToFile`, `IO::TryDeleteFile`, `IO::TryRenameFile`, and `IO::TryGetFileSize`
- `System::TryGetEnv`, `System::CurrentDirectory`, `System::TrySetCurrentDirectory`, `System::Run`, `System::CurrentPlatform`, and `System::CurrentProcessId`
- `DateTime::LocalNow`, `DateTime::UtcNow`, `DateTime::NowUnixMillis`, and `DateTime::FormatLocal`
- `TextUtil::Length`, `TextUtil::Split`, `TextUtil::Replace`, `TextUtil::Trim`, and `TextUtil::Reverse`
- `ArrayUtil::Append`, `ArrayUtil::Prepend`, `ArrayUtil::Extend`, `ArrayUtil::Slice`, and `ArrayUtil::Reverse`

`Prelude/Result.mdr` uses `Result::Ok` and `Result::Err`. The older `Result::OK` and `Result::Error` spellings are removed from the public prelude API.

See [Prelude](docs/prelude.md) for module-by-module notes and examples covering the typed `IO`, `System`, `DateTime`, `TextUtil`, and `ArrayUtil` APIs.

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

Midori uses CMake presets for native builds.

Configure and build a Development binary:
```bash
cmake --preset x64-development
cmake --build --preset x64-development --target Midori
```

Other common presets:
```bash
cmake --preset x64-debug
cmake --build --preset x64-debug --target Midori

cmake --preset x64-release
cmake --build --preset x64-release --target Midori
```

Native preset builds write the executable to `out/build/ninja/<preset>/out/Midori.exe`.

### Running Programs

```bash
# Run a Midori program built with the Development preset
.\out\build\ninja\x64-development\out\Midori.exe path\to\program.mdr
```

### Running Unit Tests

Debug and Development preset builds enable `MIDORI_BUILD_TESTS` by default. Release preset builds leave unit tests off unless you opt in with `-DMIDORI_BUILD_TESTS=ON`.

See [Testing Guide](docs/testing.md) for the `tests/` vs `test/` split, helper usage, and the full command matrix.

For a single entry point that configures, builds, and runs tests, use:
```bash
python scripts/test_project.py
```

Configure and build the unit test target:
```bash
cmake --preset x64-debug
cmake --build --preset x64-debug --target MidoriUnitTests
```

Run the registered Catch2 suites through CTest:
```bash
ctest --test-dir out/build/ninja/x64-debug --output-on-failure
```

You can also build and run the Development preset:
```bash
cmake --preset x64-development
cmake --build --preset x64-development --target MidoriUnitTests
ctest --test-dir out/build/ninja/x64-development --output-on-failure
```

### Running Regression Tests

See [Testing Guide](docs/testing.md) for when a new test should go in `tests/` instead of `test/`, plus filtering examples for both harnesses.

Run all file-based language regression tests:
```bash
python scripts/run_tests.py --build Development
```

Run specific regression tests:
```bash
python scripts/run_tests.py --test closure/simple.mdr --build Development
python scripts/run_tests.py --category typeclass --build Development
python scripts/run_tests.py --category static_analyzer --build Development
python scripts/run_tests.py --pattern recursive --build Development
```

Test fixtures are file-based:
```bash
# Add a new program test under test/<category>/<name>.mdr
# Put tests under a failure/ directory when they should fail compilation
# Add <name>.expected to assert stdout/stderr snapshots
# Add <name>.warnings.json to assert warning code, line, and message fragments
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
        case Tree::Leaf(_) => 1
        case Tree::Node(left, right) => {
            def left_height = height(left);
            def right_height = height(right);
            return 1 + (if left_height > right_height
                        then left_height
                        else right_height);
        }
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
    ;
};

defun map<A, B>(list: List<A>, f: fn(A) -> B) : List<B> => {
    return match list with
        case List::Cons(head, tail) =>
            new List::Cons(f(head), map(tail, f))
        case List::Nil =>
            new List::Nil()
    ;
};
```

## Architecture

- **Frontend**: Lexer → Parser → Type Checker
- **Optimizer**: Constant folding, tail call optimization, strength reduction
- **Backend**: Bytecode generator → Linker
- **Runtime**: Stack-based VM with mark-and-sweep garbage collection

See [Runtime Architecture](docs/runtime-architecture.md) for details on VM execution, closure capture, and memory management.

## Documentation

See the [docs](docs/) folder for detailed technical documentation:

- [Type System](docs/type-system.md) - Type inference, type classes, and algebraic data types
- [Prelude](docs/prelude.md) - Standard-library module map and typed wrapper examples
- [Compilation Workflow](docs/compilation-workflow.md) - Complete pipeline from lexing to linking
- [Package System](docs/package-system.md) - Creating and using packages with native FFI bindings
- [Project Standard](docs/project-standard.md) - Standard project layout and manifest
- [Runtime Architecture](docs/runtime-architecture.md) - VM execution, closure capture, and garbage collection
- [Testing Guide](docs/testing.md) - Choosing between implementation tests and regression tests, with helper and command references
