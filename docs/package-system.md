# Package System

Midori has an early package integration layer built around `package.midori` manifests and dynamically loaded FFI libraries. It is not yet a full package manager.

## What Exists Today

Implemented:

- TOML manifest parsing through `PackageManifest`
- package metadata tables under `package.midori`
- platform-specific library path selection
- automatic dynamic library loading when an imported module sits beside a `package.midori`
- runtime lookup of dynamically loaded foreign functions through `DynamicFFIRegistry`

Not implemented yet:

- package dependency resolution
- version solving
- registry or install workflow
- manifest-driven export enforcement

The `[dependencies]` table is currently parsed as metadata only.

## Package Layout

The current implementation works best when the imported package entry module lives next to `package.midori`.

```text
PackageName/
  package.midori
  PackageName.mdr
  lib/
    windows/x64/packagename.dll
    linux/x86_64/libpackagename.so
    macos/libpackagename.dylib
```

Manifest discovery is not recursive. `ModuleManager` checks only the parent directory of the imported `.mdr` file.

## Manifest Format

`package.midori` is parsed by `src/Compiler/PackageManager/PackageManifest.cpp`.

Recognized tables and fields:

### `[package]`

- `name`
- `version`
- `authors`
- `description`
- `license`
- `repository`
- `midori_version`

### `[package.modules]`

- `main`
- `exports`

Notes:

- `main` is used by `PackageManifest::GetMainModulePath()`.
- `exports` is parsed and retained in the manifest object, but actual symbol visibility is still enforced by the `.mdr` module's `public export` and `private export` blocks.

### `[dependencies]`

- arbitrary string-to-string entries

These are parsed but not resolved by the compiler today.

### `[ffi]`

- `enabled`
- `library_name`
- `functions`

`functions` maps exported Midori foreign names such as `"MIDORI_FFI_Package_Add"` to concrete symbol names inside the shared library.

### `[build]`

- `cmake_minimum_version`
- `cpp_standard`

This is metadata only; the compiler does not run a build tool from the manifest.

### `[prebuilt]`

- `windows_x64`
- `linux_x86_64`
- `macos_arm64`
- `macos_x86_64`

Each prebuilt entry contains:

- `path`
- `checksum`

Checksums are parsed but not enforced yet.

## Library Path Selection

`PackageManifest::GetFFILibraryPath()` chooses a library path like this:

1. use the matching `[prebuilt]` entry for the current platform if present
2. otherwise fall back to the conventional platform path under `lib/`

Fallback paths:

- Windows: `lib/windows/x64/<library_name>.dll`
- macOS: `lib/macos/lib<library_name>.dylib`
- Linux: `lib/linux/x86_64/lib<library_name>.so`

## How Packages Are Loaded

During import processing:

1. `ModuleManager` resolves an imported `.mdr` file.
2. It checks that file's parent directory for `package.midori`.
3. If a manifest exists, `PackageManifest::Load()` parses it.
4. If `[ffi].enabled = true` and the selected library path exists, `DynamicFFIRegistry` loads the library and registers the declared functions.
5. Compilation then continues as normal.

This is import-triggered loading, not a separate package-install step.

## FFI ABI for Dynamic Packages

Dynamic packages use the generic `CALL_FOREIGN` runtime path:

```c
void function_name(void** args, void* ret)
```

That matters because dynamic packages do not get the richer builtin `FFIArgumentKind` and `FFIReturnKind` metadata used by `CALL_FOREIGN_INDEXED`.

### Argument Passing

For dynamically loaded package functions, the VM currently marshals arguments like this:

- `Text`: `args[i]` is a `const char*`
- `Array<T>`: `args[i]` points to an array view struct
- raw scalar values such as `Int`, `Float`, `Bool`, `Byte`, and `Word`: the value's bytes are copied directly into the pointer-sized `args[i]` slot

Current array-view shape:

```c
struct ArrayArgument {
    void* data;
    int length;
};
```

### Reading Raw Scalar Arguments

Do not cast `args[i]` directly to the target integer or float type. The runtime stores raw scalar bits inside the `void*` slot itself, so native code should copy from `&args[i]`.

Example:

```c
void MIDORI_FFI_AddOne(void** args, void* ret)
{
    int64_t value = 0;
    std::memcpy(&value, &args[0], sizeof(value));

    value += 1;
    std::memcpy(ret, &value, sizeof(value));
}
```

Reading text and arrays:

```c
void MIDORI_FFI_Describe(void** args, void* ret)
{
    const char* text = static_cast<const char*>(args[0]);
    const ArrayArgument* array = static_cast<const ArrayArgument*>(args[1]);
    (void)text;
    (void)array;
}
```

### Return Values

The VM currently expects:

- raw scalars: write the value bytes into `ret`
- `Text`: write a heap-allocated `char*` pointer value into `ret`
- `Array<T>`: write a heap-allocated pointer to a heap-allocated array wrapper into `ret`

Current array return wrapper:

```c
struct FFIArray {
    void* data;
    int length;
};
```

### Ownership Rules

For dynamically loaded package FFI:

- returned `char*` text is copied into a Midori-managed `Text` and then freed by the VM
- returned arrays are wrapped through `MidoriArray::FromFFI`
- short returned arrays are copied into Midori small-object storage and the original FFI buffer is freed
- longer returned arrays are adopted directly without an element copy
- the outer `FFIArray` wrapper itself is always freed by the VM

Practical consequence:

- allocate returned text buffers and returned array buffers with `malloc`/`free` compatible allocation
- do not free them yourself after writing the pointer into `ret`

### Scope of the Dynamic ABI

The dynamic package path is best suited to:

- primitive scalars
- `Text`
- flat `Array<T>` values whose element representation already matches Midori's runtime values

The richer builtin-only kinds such as `TraceableHandle`, `ValueHandle`, `ArrayStrings`, and `Value` are described in `MidoriFFIRegistry`, but they are part of the statically registered runtime FFI path rather than the dynamic package ABI.

## Example Manifest

```toml
[package]
name = "PackageName"
version = "0.1.0"
authors = ["Author Name <email@example.com>"]
description = "Package description"
license = "MIT"
midori_version = ">=1.0.0"

[package.modules]
main = "PackageName.mdr"
exports = ["PackageName"]

[dependencies]

[ffi]
enabled = true
library_name = "packagename"

[ffi.functions]
"MIDORI_FFI_PackageName_Function" = "native_function_name"
```

## Example Midori Surface

```midori
module PackageName
public export { FunctionName }

foreign "MIDORI_FFI_PackageName_Function" FunctionName : fn(Int) -> Int;
```

## Runtime Lookup Order

At call time the VM resolves foreign functions in this order:

1. `MidoriFFIRegistry` for built-in runtime FFI
2. `DynamicFFIRegistry` for dynamically loaded package functions

If neither path resolves the name, execution fails with a runtime error.

## Thread Safety

`DynamicFFIRegistry` uses a mutex around library loading and function lookup, so package FFI registration is thread-safe at the registry level.

## Current Limitations

- Package manifests are discovered only from the imported module's immediate directory.
- There is no package CLI and no dependency graph resolution.
- Manifest checksums and build metadata are not enforced.
- `package.modules.exports` is not a second export mechanism; actual exports still come from the module source.
- Dynamic package FFI does not expose the full builtin typed-FFI surface.
