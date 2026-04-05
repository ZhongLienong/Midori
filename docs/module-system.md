# Module System

Midori modules provide namespace isolation, explicit visibility, and dependency-driven multi-file compilation.

## Core Rules

Every `.mdr` file must satisfy these rules:

- It must contain exactly one explicit `module` declaration.
- That `module` declaration must be the first top-level statement in the file.
- Leading whitespace and comments are allowed before `module`.
- Any top-level code, `import`, `use`, or export block before `module` is rejected.

After the `module` declaration, top-level module statements are flexible:

- `import` may appear anywhere at top level.
- `use` may appear anywhere at top level.
- `public export` and `private export` may appear anywhere at top level.
- Multiple import blocks and multiple export blocks are allowed.

`ModuleManager` scans these statements before ordinary parsing, so their placement after `module` is a source-layout choice rather than a semantic phase boundary.

## Syntax

### Module Declaration

```midori
module Math.Vector
```

Module names use dot-separated identifiers and define the qualification prefix used by `::`.

### Export Blocks

```midori
module Math.Vector
public export { add, dot }
private export { debug_helper }
```

Visibility levels:

- `public export`: accessible to all importers
- `private export`: accessible only to modules that share the same namespace prefix
- unexported symbols: module-internal only

### Import Forms

System import through `MIDORI_PATH`:

```midori
import { <IO> }
import { <Math.Vector> }
```

Path import relative to the importing file:

```midori
import { "./helpers.mdr" }
import { "../lib/database.mdr" }
```

Multiple imports can share a block:

```midori
import { <IO>, "./helpers.mdr" }
```

### Use Forms

Single imported symbol:

```midori
use Math.Vector.add
```

Braced list:

```midori
use Math.Vector.{add, multiply}
```

Without `use`, cross-module access stays qualified:

```midori
def result = Math.Vector::add(v1, v2);
```

## Flexible Placement

Only `module` is fixed in position. Other module statements can be scattered:

```midori-test name=module-system/flexible_placement path=.doc_examples/module_system/flexible_placement.mdr
module Example

defun LocalHelper(x: Int): Int => x + 1;

import { <IO> }

public export { main }

use IO.{PrintLine}

defun main(): Int => {
    PrintLine((LocalHelper(41)) as Text);
    0
};
```

This matches the current implementation and the regression fixtures under `test/module/success/`.

## Resolution Behavior

### Import Resolution

`ImportResolver` resolves:

- `<Module.Name>` by converting it to `Module/Name.mdr` and searching `MIDORI_PATH`
- `"relative/path.mdr"` relative to the importing file

Platform notes:

- `MIDORI_PATH` uses `;` on Windows and `:` on Unix-like systems.
- Resolved import paths are normalized to absolute paths.

### Build Graph Construction

`ModuleManager` recursively loads imported modules and builds a `BuildGraph` containing:

- a stripped token stream for each module body
- source lines for later diagnostics
- dependency edges
- collected `use` imports
- module declarations and export metadata
- a module-name-to-file map for duplicate detection

Errors raised here include:

- unresolved import
- import file open failure
- circular dependency
- missing module declaration
- duplicate module declaration
- duplicate module name across files

### Duplicate Module Names

Two different files cannot declare the same module name. The build graph rejects the second declaration before parsing proceeds.

## Symbol Visibility

### Qualified Access

Cross-module names use `::`:

```midori-test name=module-system/qualified_access path=.doc_examples/module_system/qualified_access.mdr module=ModuleQualifiedAccess
import { <IO> }

defun main(): Int => {
    IO::PrintLine("Hello");
    0
};
```

### Unqualified Access via `use`

```midori-test name=module-system/use_access path=.doc_examples/module_system/use_access.mdr module=ModuleUseAccess
import { <IO> }
use IO.{PrintLine}

defun main(): Int => {
    PrintLine("Hello");
    0
};
```

### Privacy

Private exports are visible only when the importer shares the same namespace prefix.

Examples:

- `Math.Vector` can access private exports from `Math.Internal`
- `App.Main` cannot access private exports from `Math.Internal`

### Exported Types

Types must be exported to be used from another module through qualified access.

```midori
module MyLib
public export { PublicType, GetValue }

struct PublicType
{
    value: Int
};

struct InternalType
{
    value: Int
};
```

`MyLib::PublicType` is visible to importers. `MyLib::InternalType` is not.

When a union type is exported, its constructors become available with it.

## Compilation Scheduling

The module system computes stable tiers, but compilation itself is dependency-driven.

Current behavior in `Compiler.cpp`:

- `BuildGraph::GetCompilationTiers()` is used for deterministic progress output and final linking order.
- The compiler separately tracks remaining dependency counts for each module.
- Modules whose dependencies are satisfied are pushed into a ready queue immediately.
- Native builds use a pool of `std::jthread` workers to consume the queue.
- Emscripten builds use the same dependency logic with a single-threaded deque.

This means tiers are metadata for reporting and stable ordering, not a hard "finish tier N before starting tier N+1" execution barrier.

## Per-Module Compilation

After module resolution, each module is compiled through:

1. parse
2. type-signature extraction for exported API
3. type checking
4. static analysis
5. optimization
6. code generation

Each completed module contributes:

- exported symbol visibility
- exported type signatures
- typeclass metadata
- warnings
- optional bytecode ready for linking

## Linking

`BytecodeLinker` combines all compiled modules into a single executable by:

- assigning global procedure and global-variable offsets
- collecting exports
- checking duplicate exported symbols
- resolving imports and patching bytecode
- concatenating procedures
- generating the bootstrap entry path

The declared entry module name is preserved for bootstrap and debug labeling when available.

## Package Interaction

If an imported module's directory contains `package.midori`, `ModuleManager` loads that manifest during graph construction. If `[ffi].enabled = true` and the declared library exists, the dynamic FFI registry loads it before compilation continues. See [Package System](package-system.md).
