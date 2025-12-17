# Module System Tests

This directory contains comprehensive tests for Midori's module system, including imports, exports, and flexible module statement placement.

## Directory Structure

- `success/` - Tests that should compile and run successfully
- `failure/` - Tests that should fail with appropriate error messages

## Test Categories

### Basic Module Features
- `basic.mdr` - Basic module functionality
- `test_self_contained.mdr` - Module without imports
- `test_no_module_decl.mdr` - File without explicit module declaration

### Local File Imports
- `local_module_lib.mdr` + `local_module_import.mdr` - Basic local file import
- `simple_module_lib.mdr` + `simple_module_import.mdr` - Simple module example
- `test_qualified_access.mdr` - Qualified name access (Module::Symbol)

### System Imports (Angle Bracket Syntax)
- `system_import.mdr` - Single system import using `<IO>`
- `system_import_multiple.mdr` - Multiple system imports
- `mixed_imports.mdr` - Mix of system and local imports

### Flexible Module Statement Placement
These tests demonstrate that `module`, `import`, `export`, and `use` statements can appear anywhere at global scope:

- `flexible_placement.mdr` - Import and use statements scattered throughout
- `flexible_import_middle.mdr` - Import statement in middle of code
- `flexible_use_scattered.mdr` - Use statements at different positions
- `flexible_module_lib.mdr` + `flexible_module_import.mdr` - Module with flexible statement placement
- `import_at_end.mdr` - Import statements at the end of file
- `multiple_import_blocks.mdr` - Multiple separate import blocks

### Mixed Local and System Imports
- `mixed_local_system_lib.mdr` + `mixed_local_system_import.mdr` - Combining local and system imports

### Complex Module Interactions
- `test_multiple_imports.mdr` - File with multiple imports
- `test_transitive_deps.mdr` - Transitive dependency chain
- `test_complex_chain.mdr` - Complex dependency chain
- `layer_a.mdr`, `layer_b.mdr`, `layer_c.mdr` - Multi-layer module structure
- `math_ops.mdr`, `math_lib.mdr`, `string_utils.mdr` - Module library examples

## Failure Tests

- `import_nonexistent_file.mdr` - Importing a file that doesn't exist
- `import_nonexistent_system.mdr` - Importing a system module that doesn't exist
- `export_missing_symbol.mdr` - Exporting a symbol that isn't defined
- `test_privacy.mdr` - Testing privacy violations
- `circular_a.mdr` + `circular_b.mdr` - Circular dependency detection

## Running Tests

Run individual tests:
```bash
./build/Midori.exe test/module/success/flexible_placement.mdr
```

Run all success tests:
```bash
for test in test/module/success/*.mdr; do
    echo "Testing: $test"
    ./build/Midori.exe "$test" || echo "FAILED"
done
```

## Key Features Tested

1. **Module Declaration**: `module ModuleName`
2. **Public Exports**: `public export { Symbol1, Symbol2 }`
3. **Private Exports**: `private export { Symbol }`
4. **Local File Imports**: `import { "path/to/file.mdr" }`
5. **System Imports**: `import { <SystemModule> }`
6. **Mixed Imports**: `import { "local.mdr", <System> }`
7. **Use Statements**: `use Module.{Symbol1, Symbol2}`
8. **Qualified Access**: `Module::Symbol`
9. **Flexible Statement Placement**: Module statements can appear anywhere at global scope

## Notes

- All module statements (`module`, `import`, `export`, `use`) must be at global scope (brace depth 0)
- They can appear in any order and anywhere in the file
- Use statements bring symbols into scope from imported modules
- System modules use angle bracket syntax: `<IO>`, `<Math>`
- Local file imports use quoted paths: `"file.mdr"`
