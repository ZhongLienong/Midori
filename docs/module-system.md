# Module System

The Midori module system provides namespace isolation, dependency management, and controlled symbol visibility for organizing code across multiple files.

## Overview

Modules in Midori enable:
- Clean separation of concerns across files
- Controlled export/import of symbols
- Cross-module type checking and function calls
- Parallel compilation of independent modules
- System-wide package imports via `MIDORI_PATH`

## Architecture

### Core Components

#### 1. ModuleDeclaration
**Location**: `src/Compiler/Module/Module.h/cpp`

Represents module metadata:
```cpp
struct ModuleDeclaration {
    std::string m_module_name;                  // Module identifier
    std::string m_file_path;                    // Source file path
    std::vector<ModuleExport> m_exports;        // Exported symbols
    std::vector<std::string> m_imports;         // Imported module names
    bool m_has_module_declaration;              // Explicit "module X" declaration
};
```

#### 2. ModuleExport
**Location**: `src/Compiler/Module/Module.h`

Defines exported symbols with visibility:
```cpp
struct ModuleExport {
    std::string m_symbol_name;                  // Exported symbol name
    VisibilityLevel m_visibility;               // Public, Private, or Internal
};

enum class VisibilityLevel {
    Public,     // Accessible to all importers
    Private,    // Accessible only to same namespace modules
    Internal    // Module-internal only (not exported)
};
```

#### 3. ModuleManager
**Location**: `src/Compiler/ModuleManager/ModuleManager.h/cpp`

Orchestrates module compilation:
- Extracts module declarations and imports
- Builds dependency graph
- Detects circular dependencies
- Creates compilation streams for parallel processing

#### 4. ImportResolver
**Location**: `src/Compiler/ImportResolver/ImportResolver.h/cpp`

Resolves import paths:
- System imports: `<ModuleName>` via `MIDORI_PATH`
- Path imports: `"relative/path.mdr"` relative to current file
- Converts module names to file paths
- Platform-aware path handling

#### 5. BuildGraph
**Location**: `src/Compiler/Token/Token.h`

Tracks module dependencies and compilation order:
```cpp
struct BuildGraph {
    struct BuildNode {
        TokenStream m_tokens;                    // Module's tokens
        std::string m_file_name;                 // Source file path
        std::vector<std::string> m_dependencies; // Imported modules
        std::vector<UseImport> m_use_imports;    // 'use' statements
        int m_in_degree;                         // Dependency count
        bool m_processed;                        // Compilation flag
    };

    std::unordered_map<std::string, BuildNode> m_nodes;
    std::unordered_map<std::string, ModuleDeclaration> m_module_declarations;
    std::unordered_map<std::string, std::string> m_module_name_to_file;  // Duplicate detection
};
```

#### 6. CompiledModule
**Location**: `src/Compiler/Module/CompiledModule.h`

Stores per-module compilation results:
```cpp
struct CompiledModule {
    std::string m_module_name;
    std::filesystem::path m_file_path;

    struct SymbolTable {
        std::unordered_set<std::string> m_exports;
        std::unordered_map<std::string, VisibilityLevel> m_export_visibility;
    } m_symbols;

    TypeEnvironment m_type_signatures;           // Type info for exports
    std::optional<BytecodeModule> m_bytecode;    // Per-module bytecode
    TypeclassMetadataMap m_typeclass_metadata;   // Typeclass info
};
```

#### 7. BytecodeLinker
**Location**: `src/Compiler/BytecodeLinker/BytecodeLinker.h/cpp`

Links multiple modules into executable:
- Assigns global procedure/variable indices
- Builds global symbol table
- Resolves cross-module references
- Patches bytecode with correct addresses
- Creates bootstrap procedure

## Module Syntax

### Module Declaration

Declare a module with exports:

```midori
module Math.Vector
public export { add, multiply, dot }
private export { internal_helper }

defun add(v1: Vec2, v2: Vec2): Vec2 => {
    Vec2(v1.x + v2.x, v1.y + v2.y)
};

defun multiply(v: Vec2, scalar: Float): Vec2 => {
    Vec2(v.x * scalar, v.y * scalar)
};

defun dot(v1: Vec2, v2: Vec2): Float => {
    v1.x * v2.x + v1.y * v2.y
};

defun internal_helper(x: Float): Float => {
    x * 2.0
};
```

Key points:
- Module name can use dot notation for namespacing: `Math.Vector`
- `public export` makes symbols accessible to all importers
- `private export` restricts access to same namespace modules
- Unexported symbols are module-internal only

### Import Statements

Two types of imports:

**System imports** (via `MIDORI_PATH`):
```midori
import { <IO> }
import { <Math.Vector> }
```

**Path imports** (relative to current file):
```midori
import { "utils/helpers.mdr" }
import { "../lib/database.mdr" }
```

Multiple import blocks are allowed:
```midori
import
{
    <IO>
    <String>
}

import
{
    "local_helpers.mdr"
}
```

### Use Statements

Import specific symbols from modules:

```midori
import { <Math.Vector> }

use Math.Vector.{add, multiply}

def result = add(v1, v2);
def scaled = multiply(v, 3.0);
```

Without `use`, qualify access:
```midori
import { <Math.Vector> }

def result = Math.Vector::add(v1, v2);
```

## Module Resolution

### System Imports

**Location**: `src/Compiler/ImportResolver/ImportResolver.cpp:35-82`

For `import { <Math.Vector> }`:

1. Extract module name: `Math.Vector`
2. Convert to path: `Math/Vector.mdr`
3. Search through `MIDORI_PATH` directories (platform-specific separator: `;` on Windows, `:` on Unix)
4. Return first matching file's absolute path

**MIDORI_PATH** example:
```bash
export MIDORI_PATH="/usr/local/lib/midori:/home/user/midori_packages"
```

### Path Imports

**Location**: `src/Compiler/ImportResolver/ImportResolver.cpp:84-117`

For `import { "utils/helpers.mdr" }`:

1. Resolve relative to current file's directory
2. Convert to absolute path
3. Verify file exists
4. Return absolute path

### Explicit Module Declarations Required

**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp:41-48`

**All `.mdr` files must begin with an explicit `module` declaration.** Files without a module declaration are rejected:

```midori
// ❌ This file will be rejected
defun add(a: Int, b: Int): Int => a + b;
```

**Error:**
```
Module declaration required. All .mdr files must begin with an explicit 'module ModuleName' declaration.
```

**Correct approach:**
```midori
// ✅ Explicit module declaration
module MyHelper

defun add(a: Int, b: Int): Int => a + b;
```

This ensures clarity and prevents confusion about module identity.

## Cross-Module Access

### Qualified Access

Use `::` separator for cross-module access:

```midori
import { <IO> }

defun main(): Int => {
    IO::PrintLine("Hello, World!");
    0
};
```

**Name Mangling**: Symbols are internally mangled as `ModuleName::SymbolName`

### Unqualified Access via Use

```midori
import { <IO> }
use IO.{PrintLine}

defun main(): Int => {
    PrintLine("Hello, World!");
    0
};
```

### Symbol Resolution Order

**Location**: `src/Compiler/Parser/Parser.cpp:100-175`

1. Check local scope (current function/module)
2. Check `use` imported symbols
3. Check explicit module exports
4. For qualified access (`Module::Symbol`), directly resolve in target module
5. Check typeclass methods with qualified syntax

## Visibility and Privacy

### Access Control

**Public exports**:
```midori
module Utils
public export { helper, format }
```
- Accessible to all importers
- No restrictions

**Private exports**:
```midori
module Math.Internal
private export { debug_helper }
```
- Accessible only to same namespace modules
- `Math.Internal` and `Math.Vector` share `Math` namespace
- Checked via `SharesNamespace()` function

**Internal (not exported)**:
```midori
module Utils

defun internal_only(x: Int): Int => x * 2;
```
- Not in export list
- Module-internal only
- Cannot be accessed from other modules

### Privacy Checking

**Location**: `src/Compiler/Parser/Parser.cpp:143-153`

During parsing, visibility is enforced:
- Public: always accessible
- Private: requires `SharesNamespace(importer, exporter)`
- Internal: never accessible cross-module

### Type Export Enforcement

**Location**: `src/Compiler/Parser/Parser.cpp:3754-3762`

Types (structs, unions, type aliases) must be explicitly exported to be accessible via qualified access:

```midori
module MyLib
public export { PublicType, get_value }

struct PublicType { value: Int };
struct InternalType { data: Int };  // Not exported

defun get_value(): Int => 42;
```

Attempting to access an unexported type from another module:
```midori
import { "MyLib.mdr" }

// ❌ Error: Type 'InternalType' is not exported by module 'MyLib'
def data: MyLib::InternalType = ...;

// ✅ OK: PublicType is exported
def public_data: MyLib::PublicType = new MyLib::PublicType(10);
```

**Union Constructors:**
Union constructors are automatically exported when the union type is exported:
```midori
module DataTypes
public export { Option }  // Exports Option, Some, and None

union Option<T> = None | Some(T);
```

Both `Option::None()` and `Option::Some(value)` become accessible to importers.

## Compilation Pipeline

### Phase 1: Build Graph Generation

**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp:28-186`

```
Main File (TokenStream)
       │
       ▼
Extract Module Declaration
       │
       ▼
Extract Imports & Use Statements
       │
       ▼
For Each Import:
  │─ Resolve Path
  │─ Lex File
  │─ Recursive Module Manager
  │─ Add to Build Graph
       │
       ▼
Check Circular Dependencies
       │
       ▼
Calculate In-Degrees
       │
       ▼
Create Compilation Streams
```

Steps:
1. **Extract Module Info**: Parse `module`, `export`, `import`, `use` statements
2. **Resolve Imports**: Convert import specifiers to absolute paths
3. **Recursive Processing**: Process each imported module
4. **Dependency Graph**: Build `ModuleDependencyGraph` (module → dependencies)
5. **Cycle Detection**: Use DFS to detect circular dependencies
6. **Topological Sort**: Calculate in-degrees for compilation ordering
7. **Parallel Streams**: Group independent modules for parallel compilation

### Phase 2: Statement Extraction

**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp:476-587`

**Module Declaration Extraction**:
```cpp
std::tuple<std::string, std::vector<ModuleExport>>
ExtractModuleDeclaration(const TokenStream& tokens, const std::vector<StatementSpan>& spans)
```
- Scans for `module` keyword
- Parses module name (supports dot notation)
- Extracts `public export` and `private export` blocks

**Import Extraction**:
```cpp
std::vector<std::pair<std::string, int>>
ExtractImports(const TokenStream& tokens, const std::vector<StatementSpan>& spans)
```
- Finds `import { ... }` blocks
- Supports both `<System>` and `"path"` syntax
- Returns list of import specifiers with line numbers

**Use Statement Extraction**:
```cpp
std::vector<UseImport>
ExtractUseStatements(const TokenStream& tokens, const std::vector<StatementSpan>& spans)
```
- Finds `use Module.{Symbol1, Symbol2}` statements
- Parses module name and symbol list
- Returns structured `UseImport` objects

### Phase 3: Circular Dependency Detection

**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp:189-223`

Uses depth-first search:
```cpp
bool CheckCycle(
    const std::string& node,
    std::unordered_set<std::string>& visited,
    std::unordered_set<std::string>& recursion_stack
) const
```

Algorithm:
1. Mark current node as visited and in recursion stack
2. For each dependency:
   - If in recursion stack: cycle detected (return true)
   - If not visited: recursively check
3. Remove from recursion stack
4. Return false (no cycle)

### Phase 3.5: Duplicate Module Name Detection

**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp:50-71`

**Purpose**: Ensures each module name is unique across the entire project.

**Implementation:**
```cpp
if (build_graph.m_module_name_to_file.contains(module_name)) {
    const std::string& existing_file = build_graph.m_module_name_to_file.at(module_name);
    if (existing_file != m_main_file_name) {
        return std::unexpected(/* Duplicate module error */);
    }
}
build_graph.m_module_name_to_file[module_name] = m_main_file_name;
```

**Validates:**
- No two `.mdr` files can declare the same module name
- Prevents import map overwrites that cause symbol loss
- Enforces **1 file = 1 unique module** design principle

**Error Example:**
```
Duplicate module declaration: 'SharedModule' is declared in multiple files:
  First:  /path/to/file_a.mdr
  Second: /path/to/file_b.mdr
```

### Phase 4: Compilation Streams

**Location**: `src/Compiler/Token/Token.h:126-148`

Parallel compilation strategy:

```cpp
std::vector<std::vector<std::string>> GetCompilationStreams() const
```

Returns streams for parallel processing:
```
Stream 1: [ModuleA, ModuleB, ModuleC]  // No dependencies between them
Stream 2: [ModuleD]                     // Depends on Stream 1
Stream 3: [ModuleE, ModuleF]            // Depends on Stream 2
```

Modules within a stream compile in parallel. Streams are processed sequentially based on dependencies.

### Phase 5: Per-Module Compilation

For each module in dependency order:

1. **Parse**: `Parser` → `MidoriProgramTree` (AST)
2. **Extract Type Signatures**: `TypeChecker::ExtractTypeSignatures()` → Filtered type environment
   - **Location**: `src/Compiler/TypeChecker/TypeChecker.cpp:797-852`
   - Only includes types/functions that are explicitly exported
   - Union constructors automatically included when union type is exported
   - Prevents internal implementation types from leaking
3. **Type Check**: `TypeChecker` → Typed AST
4. **Optimize**: `OptimizerManager` → Optimized AST
5. **Code Generate**: `CodeGenerator` → `BytecodeModule`

Each module produces:
- `CompiledModule` with symbol table and **filtered** type signatures (exports only)
- `BytecodeModule` with local procedures and imports/exports

### Phase 6: Bytecode Linking

**Location**: `src/Compiler/BytecodeLinker/BytecodeLinker.cpp`

Links all modules into single executable:

```
Per-Module Bytecode
       │
       ▼
Assign Global Indices
       │
       ▼
Build Global Symbol Table
       │
       ▼
Merge Resources (strings, constants)
       │
       ▼
Resolve Imports & Patch Bytecode
       │
       ▼
Create Bootstrap Procedure
       │
       ▼
MidoriExecutable
```

**Steps**:

1. **Assign Base Offsets**:
   - Each module gets base procedure index
   - Each module gets base global variable index
   - Enables translation from local to global indices

2. **Build Global Symbol Table**:
   - Collect all exported symbols: `ModuleName::SymbolName`
   - Map to global procedure/variable index
   - Detect duplicate exports

3. **Merge Resources**:
   - Merge string pools from all modules
   - Merge constant pools
   - Merge function name tables

4. **Resolve and Patch**:
   - For each imported symbol, find in exporting module
   - Patch bytecode instructions with correct global indices
   - Handle opcodes: `DEFINE_GLOBAL`, `GET_GLOBAL`, `SET_GLOBAL`, `CALL_PROC`, `MAKE_CLOSURE`

5. **Create Bootstrap**:
   - Special startup procedure
   - Initializes all modules
   - Calls module-level initialization code

## BytecodeModule Structure

**Location**: `src/Compiler/BytecodeModule/BytecodeModule.h`

Per-module bytecode representation:

```cpp
struct BytecodeModule {
    struct ExportedSymbol {
        std::string m_name;
        size_t m_procedure_index;        // Local procedure index
        size_t m_global_index;           // Local global variable index
        SymbolType m_type;               // Function or Global
    };

    struct ImportedSymbol {
        std::string m_name;
        std::string m_from_module;
        std::vector<size_t> m_usage_locations;  // Bytecode offsets to patch
    };

    std::string m_module_name;
    std::vector<BytecodeStream> m_procedures;   // All functions in module
    std::vector<ExportedSymbol> m_exports;      // What this module exports
    std::vector<ImportedSymbol> m_imports;      // What this module imports
    std::vector<std::string> m_string_pool;     // Module-local strings
};
```

## Key Constants

**Location**: `src/Common/Constant/Constant.h`

```cpp
constexpr std::string_view NameSeparator = "::";           // Module::Symbol
constexpr char ModuleSeparator = '@';                      // Internal use
constexpr std::string_view MAIN_PROCEDURE_PREFIX = "$main$";
constexpr std::string_view MODULE_BOOTSTRAP_PREFIX = "$module_bootstrap$";
```

## Example Usage

### Simple Module and Import

**math_lib.mdr**:
```midori
module Math
public export { add, multiply }

defun add(a: Int, b: Int): Int => a + b;
defun multiply(a: Int, b: Int): Int => a * b;
```

**main.mdr**:
```midori
import { "math_lib.mdr" }

use Math.{add, multiply}

defun main(): Int => {
    def x = add(5, 3);
    def y = multiply(x, 2);
    y
};
```

### Qualified Access

**math_lib.mdr**:
```midori
module Math
public export { add, multiply }

defun add(a: Int, b: Int): Int => a + b;
defun multiply(a: Int, b: Int): Int => a * b;
```

**main.mdr**:
```midori
import { "math_lib.mdr" }

defun test_qualified(): Int => {
    Math::add(Math::multiply(5, 3), 2)
};

defun main(): Int => test_qualified();
```

### System Import

**Requires `MIDORI_PATH` to include IO module directory**:

```midori
import { <IO> }

defun main(): Int => {
    IO::PrintLine("Hello, Midori!");
    0
};
```

### Layered Dependencies

**layer_c.mdr**:
```midori
module LayerC
public export { base }

defun base(x: Int): Int => x * 2;
```

**layer_b.mdr**:
```midori
module LayerB
public export { transform }

import { "layer_c.mdr" }

defun transform(x: Int): Int => LayerC::base(x) + 10;
```

**layer_a.mdr**:
```midori
module LayerA
public export { process }

import { "layer_b.mdr" }

defun process(x: Int): Int => LayerB::transform(x);
```

**main.mdr**:
```midori
import { "layer_a.mdr" }

defun main(): Int => LayerA::process(5);  // Returns (5 * 2) + 10 = 20
```

### Private Exports

**math_internal.mdr**:
```midori
module Math.Internal
private export { debug_helper }
public export { compute }

defun debug_helper(x: Int): Int => {
    // Only accessible to Math.* namespace modules
    x * 2
};

defun compute(x: Int): Int => debug_helper(x) + 1;
```

**math_public.mdr**:
```midori
module Math.Public

import { "math_internal.mdr" }

defun test(): Int => {
    // Can access private export (same Math namespace)
    Math.Internal::debug_helper(5)
};
```

**main.mdr**:
```midori
import { "math_internal.mdr" }

defun main(): Int => {
    // ERROR: Cannot access private export from different namespace
    Math.Internal::debug_helper(5)
};
```

## Error Handling

### Module Resolution Errors

**File not found**:
```
Could not resolve import: <NonExistent>
At line 1 in main.mdr
```

**Circular dependency**:
```
Circular dependency detected: /path/to/module_a.mdr
At line 3 in module_b.mdr
```

**Missing module declaration**:
```
Module declaration required. All .mdr files must begin with an explicit 'module ModuleName' declaration.
At line 1 in my_file.mdr
```

**Duplicate module name**:
```
Duplicate module declaration: 'SharedModule' is declared in multiple files:
  First:  C:\path\to\file_a.mdr
  Second: C:\path\to\file_b.mdr
At line 1 in file_b.mdr
```

### Symbol Errors

**Using unexported symbol**:
```
Symbol 'internal_func' is not exported by module 'Utils'
```

**Private symbol access violation**:
```
Cannot access private symbol 'debug_helper' from module 'Math.Internal'
Private symbols only accessible to same namespace modules
```

### Type Errors

**Using unexported type**:
```
Type 'InternalData' is not exported by module 'MyLib'.
At line 6 in main.mdr
```

This occurs when attempting qualified type access on a type that isn't in the module's export list:
```midori
// MyLib only exports get_value, not InternalData
def data: MyLib::InternalData = ...;  // ❌ Error
```

### Duplicate Exports

**Multiple modules exporting same symbol**:
```
Duplicate export: 'compute' exported by both 'Math' and 'Calculator'
```

## Implementation Details

### Module Declaration Tracking

**Location**: `src/Compiler/Module/Module.h:44`

```cpp
bool m_has_module_declaration;
```

Tracks whether file has explicit `module` declaration. **As of the current implementation, this is always `true` for successfully compiled modules** since explicit module declarations are required.

Historical note: This field previously supported implicit module names (filename-based), but that feature was removed to ensure clarity and prevent bugs.

### Symbol Mangling

All cross-module symbols are mangled with `::` separator:
- Function: `ModuleName::function_name`
- Type: `ModuleName::TypeName`
- Typeclass instance: `TypeclassName_TypeName@ModuleName`

### Namespace Matching

**Location**: `src/Compiler/Parser/Parser.cpp`

```cpp
bool SharesNamespace(const std::string& module1, const std::string& module2)
```

Compares module name prefixes:
- `Math.Vector` and `Math.Internal` share `Math` namespace
- `Math.Vector` and `String.Utils` do not share namespace

### Thread Safety

Module compilation supports parallel execution:
- Independent modules compile in parallel via `AsyncThreadPool`
- Each module has isolated symbol table during compilation
- Linking phase is sequential (merges all modules)

### Memory Management

- BuildGraph stores all module nodes
- CompiledModule uses `std::optional<BytecodeModule>` for lazy bytecode
- String pools are merged during linking to eliminate duplicates

## Platform Differences

### Path Separators

**MIDORI_PATH** uses platform-specific separators:
- Windows: `;` (semicolon)
- Unix/macOS: `:` (colon)

**Example**:
```bash
# Windows
set MIDORI_PATH=C:\midori\lib;C:\midori\packages

# Unix/macOS
export MIDORI_PATH=/usr/local/lib/midori:/home/user/midori_packages
```

### File Path Resolution

Import resolver handles platform-specific path formats:
- Windows: Supports both `/` and `\` separators
- Unix/macOS: Only `/` separator
- All paths normalized to absolute paths internally

## Performance Considerations

### Parallel Compilation

BuildGraph enables parallel compilation:
- Modules with no dependencies compile simultaneously
- Reduces compilation time for large projects
- Uses `std::thread` via `AsyncThreadPool` (non-WASM)

### Import Caching

**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp:113-116`

Modules are cached in BuildGraph:
```cpp
if (build_graph.m_nodes.contains(include_absolute_path_str)) {
    continue;  // Already processed
}
```

Prevents redundant compilation of shared dependencies.

### String Pool Merging

**Location**: `src/Compiler/BytecodeLinker/BytecodeLinker.cpp`

During linking, string pools from all modules are merged:
- Eliminates duplicate strings across modules
- Reduces executable size
- Improves cache locality