# Midori Compilation Workflow

This document describes the complete compilation pipeline from source code to executable bytecode.

## Overview

The Midori compiler follows a traditional multi-pass architecture:

```
Source Code (.mdr)
       │
       ▼
   ┌───────┐
   │ Lexer │  Tokenization
   └───┬───┘
       │ TokenStream
       ▼
┌──────────────┐
│ModuleManager │  Dependency Resolution
└──────┬───────┘
       │ BuildGraph
       ▼
   ┌────────┐
   │ Parser │  Syntax Analysis (per module, parallel)
   └───┬────┘
       │ AST (MidoriProgramTree)
       ▼
┌─────────────┐
│ TypeChecker │  Semantic Analysis
└──────┬──────┘
       │ Typed AST
       ▼
┌──────────────────┐
│ OptimizerManager │  AST Optimizations
└────────┬─────────┘
       │ Optimized AST
       ▼
┌───────────────┐
│ CodeGenerator │  Bytecode Emission
└───────┬───────┘
       │ BytecodeModule
       ▼
┌────────────────┐
│ BytecodeLinker │  Module Linking
└───────┬────────┘
       │ MidoriExecutable
       ▼
  Virtual Machine
```

## Phase 1: Lexical Analysis (Lexer)

**Source**: `src/Compiler/Lexer/`

The lexer converts raw source code into a stream of tokens.

### Input
- Source code as a string
- File name for error reporting

### Output
- `TokenStream` - Vector of `Token` objects

### Token Structure

Each token contains:
- **Name** - Token type (keyword, identifier, operator, literal, etc.)
- **Lexeme** - The actual text
- **Line** - Source line number
- **Column** - Column position for error reporting

### Processing Steps

1. **Character scanning** - Read characters one at a time
2. **Whitespace/comment skipping** - Ignore spaces, tabs, `//` and `/* */` comments
3. **Token recognition**:
   - Keywords (`def`, `defun`, `if`, `then`, `else`, `match`, `struct`, `union`, `class`, `instance`, etc.)
   - Identifiers (variable/function names)
   - Literals (integers, floats, strings, booleans)
   - Operators (`+`, `-`, `*`, `/`, `==`, `|>`, `++`, etc.)
   - Delimiters (`(`, `)`, `{`, `}`, `[`, `]`, `;`, `,`)
4. **Error recovery** - Report invalid characters, continue scanning

### Key Features

- **UTF-8 support** - Handles multi-byte characters in strings
- **Nested comments** - Block comments can be nested
- **Number formats** - Integers, floats, hex literals
- **Escape sequences** - `\n`, `\t`, `\\`, `\"` in strings

## Phase 2: Module Resolution (ModuleManager)

**Source**: `src/Compiler/ModuleManager/`

The module manager handles imports and builds a dependency graph for parallel compilation.

### Input
- `TokenStream` from the main file
- File path

### Output
- `BuildGraph` containing:
  - Dependency graph
  - Module declarations (name, exports)
  - Token streams for each module

### Processing Steps

1. **Statement scanning** - Find `module`, `import`, `export`, `use` statements
2. **Import resolution** - Locate imported files:
   - Path imports: `import { "./path/to/module.mdr" }`
   - Search path imports: `import { <ModuleName> }` (uses `MIDORI_PATH`)
3. **Dependency graph construction** - Build directed graph of module dependencies
4. **Cycle detection** - Check for circular imports (error if found)
5. **Compilation stream calculation** - Topological sort for parallel compilation
   - Modules with no dependencies compile first
   - Dependent modules wait for their dependencies

### Parallel Compilation

Modules are organized into "streams" for parallel execution:

```
Stream 1: [A, B, C]     (no dependencies, compile in parallel)
Stream 2: [D, E]        (depend on stream 1, wait then compile in parallel)
Stream 3: [F]           (depends on stream 2)
```

## Phase 3: Syntax Analysis (Parser)

**Source**: `src/Compiler/Parser/`

The parser converts tokens into an Abstract Syntax Tree (AST).

### Input
- `TokenStream`
- Imported symbol tables
- Module declaration

### Output
- `MidoriProgramTree` - List of statement nodes

### Grammar Structure

The parser implements a **recursive descent parser** with:
- Operator precedence climbing for expressions
- Backtracking for ambiguous constructs
- Error recovery with synchronization

### AST Node Types

**Statements** (`MidoriStatement`):
- `Define` - Variable definition
- `DefineTuple` - Tuple destructuring
- `DefineFunction` - Function definition
- `Struct` - Struct declaration
- `Union` - Union declaration
- `Class` - Type class declaration
- `Instance` - Type class instance
- `Foreign` - FFI function declaration
- `Simple` - Expression statement
- `Continue` - Loop continue

**Expressions** (`MidoriExpression`):
- Literals: `IntegerLiteral`, `FloatLiteral`, `TextLiteral`, `BoolLiteral`, `UnitLiteral`
- `Binary` - Binary operations
- `UnaryPrefix` / `UnarySuffix` - Unary operations
- `Call` - Function call
- `Construct` - Struct/union construction
- `BoundedName` - Variable reference
- `Bind` - Variable assignment
- `IfElse` - Conditional expression
- `Match` - Pattern matching
- `Block` - Block expression
- `Loop` / `For` - Loop expressions
- `Return` / `Break` - Control flow
- `Function` - Lambda expression
- `Array` / `ArrayGet` / `ArraySet` - Array operations
- `Get` / `Set` - Member access
- `Async` / `Await` - Concurrent execution

### Key Features

- **Expression-oriented** - Most constructs are expressions with values
- **Scoped name resolution** - Variables resolved to local/global/captured
- **Generic parameter parsing** - `<T, U>` syntax for generics
- **Constraint parsing** - `where Show<T>` for type class constraints

## Phase 4: Type Checking (TypeChecker)

**Source**: `src/Compiler/TypeChecker/`

See [Type System Documentation](type-system.md) for detailed information.

### Input
- Untyped AST (`MidoriProgramTree`)
- Imported type signatures
- Imported type class information

### Output
- Typed AST with type annotations on all nodes

### Processing Steps

1. **Environment setup** - Initialize type environment with primitives and imports
2. **Declaration processing** - Register structs, unions, classes, instances
3. **Type inference** - Hindley-Milner Algorithm W:
   - Generate fresh type variables
   - Collect constraints through AST traversal
   - Unify constraints
   - Apply substitution
4. **Type class resolution** - Resolve method calls to concrete instances
5. **Exhaustiveness checking** - Verify pattern matches cover all cases

### Key Features

- **Full type inference** - No annotations required in most cases
- **Polymorphism** - Parametric and ad-hoc (via type classes)
- **Occurs check** - Prevent infinite types
- **Constraint propagation** - Type class constraints flow through calls

## Phase 5: Optimization (OptimizerManager)

**Source**: `src/Compiler/OptimizerManager/`

The optimizer performs AST-level transformations to improve performance.

### Input
- Typed AST

### Output
- Optimized AST

### Available Optimizers

#### Constant Folding (`ConstantFolding`)

Evaluates constant expressions at compile time:

```midori
// Before
def x = 2 + 3 * 4;

// After
def x = 14;
```

Handles:
- Arithmetic operations
- Boolean logic
- String concatenation
- Comparison operators

#### Strength Reduction (`StrengthReduction`)

Replaces expensive operations with cheaper equivalents:

```midori
// Before
x * 2
x / 4
x % 8

// After
x << 1
x >> 2
x & 7
```

#### Tail Call Optimization (`TailCallOptimization`)

Converts tail-recursive calls to loops:

```midori
// Before: Regular call (grows stack)
defun factorial(n: Int, acc: Int) : Int => {
    if n <= 1 then acc else factorial(n - 1, n * acc)
};

// After: Tail call (reuses stack frame)
// Emits TAIL_CALL opcode instead of CALL_DEFINED
```

### Optimization Pipeline

Optimizers run in sequence, with multiple passes until no more optimizations apply:

```
repeat until stable:
    ConstantFolding.optimize()
    StrengthReduction.optimize()
    TailCallOptimization.optimize()
```

## Phase 6: Code Generation (CodeGenerator)

**Source**: `src/Compiler/CodeGenerator/`

The code generator emits bytecode from the optimized AST.

### Input
- Optimized, typed AST
- Module name and export list
- Imported type class information

### Output
- `BytecodeModule` containing:
  - Procedures (bytecode streams)
  - String pool
  - Global variables
  - Export/import tables

### Bytecode Format

Instructions are variable-length:
- **1 byte**: Opcode
- **1-8 bytes**: Operands (indices, constants, offsets)

### Key Opcodes

**Stack Operations**:
- `LOAD_*` - Push constants (integer, float, text, bool, unit)
- `POP` - Discard top of stack
- `DUP` - Duplicate top of stack

**Variables**:
- `GET_LOCAL` / `SET_LOCAL` - Local variable access
- `GET_GLOBAL` / `SET_GLOBAL` - Global variable access
- `GET_CELL` / `SET_CELL` - Closure variable access

**Arithmetic**:
- `ADD_INTEGER`, `SUB_INTEGER`, `MUL_INTEGER`, `DIV_INTEGER`, `MOD_INTEGER`
- `ADD_FLOAT`, `SUB_FLOAT`, `MUL_FLOAT`, `DIV_FLOAT`
- `NEGATE_INTEGER`, `NEGATE_FLOAT`

**Comparison**:
- `LESS_INTEGER`, `GREATER_INTEGER`, `EQUAL`, `NOT_EQUAL`
- Fused ops: `IF_INTEGER_LESS`, `IF_FLOAT_GREATER_EQUAL`, etc.

**Control Flow**:
- `JUMP` - Unconditional jump
- `JUMP_IF_FALSE` - Conditional jump
- `LOOP` - Backward jump (for loops)

**Functions**:
- `CALL_DEFINED` - Call user function
- `CALL_FOREIGN` - Call FFI function (dynamic lookup, fallback)
- `CALL_FOREIGN_INDEXED` - Call FFI function by table index (fast path)
- `TAIL_CALL` - Tail call optimization
- `RETURN` - Return from function
- `ALLOCATE_CLOSURE` - Create closure object
- `CONSTRUCT_CLOSURE` - Capture variables

**Async/Await**:
- `SPAWN_ASYNC` - Pop closure, spawn async task, push `Future<T>`
- `AWAIT_FUTURE` - Pop future, block until complete, push result
- `ASYNC_RETURN` - Return from async task (sets future result)

**Data Structures**:
- `NEW_ARRAY` - Create array
- `ARRAY_GET` / `ARRAY_SET` - Array access
- `NEW_STRUCT` - Create struct
- `GET_MEMBER` / `SET_MEMBER` - Struct field access
- `NEW_UNION` - Create union variant
- `GET_TAG` - Get union discriminant

### Generic Specialization

Generic functions are specialized at call sites:

```midori
defun identity<T>(x: T) : T => x;

identity(42);      // Generates identity_Int
identity("hello"); // Generates identity_Text
```

## Phase 7: Linking (BytecodeLinker)

**Source**: `src/Compiler/BytecodeLinker/`

The linker combines multiple bytecode modules into a single executable.

### Input
- List of `BytecodeModule` objects in dependency order
- Entry module name

### Output
- `MidoriExecutable` - Final executable bytecode

### Linking Steps

1. **Base offset assignment** - Calculate starting indices for each module's:
   - Procedures
   - Global variables
   - String pool entries

2. **Global symbol table** - Build unified symbol table:
   - Map exported symbols to global procedure indices
   - Detect duplicate symbols (error)

3. **Constant pool merging** - Deduplicate string constants across modules

4. **Import resolution** - For each module's imports:
   - Find symbol in exporting module
   - Patch bytecode with correct global index

5. **Bytecode concatenation** - Combine all procedures into single array

6. **Bootstrap procedure** - Generate entry point:
   - Initialize global variables
   - Call main module's top-level code
   - Emit `HALT`

### Symbol Resolution

Imports use placeholder indices during code generation:

```
Module A exports: add (procedure 0)
Module B imports: A::add

During codegen:  GET_GLOBAL [placeholder]
After linking:   GET_GLOBAL [actual_index]
```

## Executable Format

The final `MidoriExecutable` contains:

| Field | Description |
|-------|-------------|
| `m_procedures` | Vector of bytecode streams |
| `m_procedure_names` | Debug names for each procedure |
| `m_global_variable_names` | Debug names for globals |
| `m_string_pool` | Constant strings |
| `m_global_count` | Number of global variables |

## Error Handling

Each phase reports errors with:
- File name
- Line and column numbers
- Source code context
- Descriptive error message

Errors use the `std::expected` pattern for monadic error propagation:

```cpp
return lexer.Lex()
    .and_then([](TokenStream&& tokens) { return parser.Parse(tokens); })
    .and_then([](AST&& ast) { return typeChecker.Check(ast); })
    // ... continue pipeline
```

## Debug Output

With debug builds (`MIDORI_BUILD_DEBUG`):

- **AST Dump** - Pretty-printed syntax tree
- **Disassembly** - Human-readable bytecode listing
- **Stack Trace** - Runtime call stack on errors
- **Optimizer Stats** - Optimizations performed per pass

## FFI (Foreign Function Interface) System

**Source**: `src/Library/`

The FFI system provides a hybrid approach for calling native functions with optimal performance.

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    FFI Resolution                        │
├─────────────────────────────────────────────────────────┤
│  Static Builtins (index 0-N)                            │
│  ├─ Print, ReadLine, SquareRoot, etc.                   │
│  └─ Direct function pointer array in VM                 │
│  └─ Resolved at compile time via CALL_FOREIGN_INDEXED   │
├─────────────────────────────────────────────────────────┤
│  Dynamic Fallback (CALL_FOREIGN)                        │
│  ├─ For extensions not in the registry                  │
│  └─ Runtime lookup with caching                         │
└─────────────────────────────────────────────────────────┘
```

### FFI Registry

The `MidoriFFIRegistry` class maintains a static table of builtin functions:

```cpp
// MidoriFFIRegistry.h
using FFIFunction = void(*)(void** args, void* ret);

struct FFIEntry {
    const char* m_name;      // "MIDORI_FFI_Print"
    FFIFunction m_function;  // Function pointer
};
```

### Compile-Time Resolution

During code generation, foreign function declarations are checked against the registry:

1. **Registry lookup**: `MidoriFFIRegistry::FindIndex(foreign_name)`
2. **If found**: Store index in `m_ffi_indices` map, emit `CALL_FOREIGN_INDEXED`
3. **If not found**: Emit `CALL_FOREIGN` for runtime lookup

### Bytecode Format

**CALL_FOREIGN_INDEXED** (4 bytes):
```
[opcode][ffi_index][arity][return_type]
```

**CALL_FOREIGN** (3 bytes):
```
[opcode][arity][return_type]
```

### Performance

| Approach | Lookup Cost |
|----------|-------------|
| CALL_FOREIGN (fallback) | ~20-50ns (registry lookup) |
| CALL_FOREIGN_INDEXED | ~1-2ns (array access) |

### Available FFI Functions

| Category | Functions |
|----------|-----------|
| IO - Console | Print, PrintError, ReadInput, ReadLine |
| IO - File | ReadFile, WriteFile, AppendToFile, ReadBinaryFile, WriteBinaryFile, FileExists, DeleteFile, RenameFile, GetFileSize |
| Math | SquareRoot |
| DateTime | GetTime |
| System | Exit, GetEnv, Sleep, GetCurrentDirectory |
