# Package System

The Midori package system enables distribution and use of third-party modules with native FFI (Foreign Function Interface) bindings.

## Overview

Packages in Midori consist of:
- Midori source modules (`.mdr` files)
- Package metadata (`package.midori` manifest)
- Optional native libraries for FFI functions
- Documentation and examples

## Architecture

### Core Components

#### 1. PackageManifest
**Location**: `src/Compiler/PackageManager/PackageManifest.h/cpp`

Parses and validates TOML-based package manifests. Extracts:
- Package metadata (name, version, authors, license)
- Module information and exports
- FFI configuration and function mappings
- Platform-specific binary paths

#### 2. DynamicFFIRegistry
**Location**: `src/Library/DynamicFFIRegistry/DynamicFFIRegistry.h/cpp`

Manages runtime loading of native libraries:
- Loads shared libraries (`.dll`, `.so`, `.dylib`) at runtime
- Registers FFI function pointers by name
- Thread-safe operation with `std::mutex`
- Platform-specific library loading (Windows `LoadLibraryW`, Unix `dlopen`)

#### 3. Module Manager Integration
**Location**: `src/Compiler/ModuleManager/ModuleManager.cpp`

During module compilation:
- Detects `package.midori` manifest in imported module directories
- Loads package manifest and FFI configuration
- Automatically loads native libraries via `DynamicFFIRegistry`
- Registers FFI functions before module compilation

#### 4. Virtual Machine Integration
**Location**: `src/Interpreter/VirtualMachine/VirtualMachine.cpp`

At runtime:
- Checks `MidoriFFIRegistry` (built-in functions) first
- Falls back to `DynamicFFIRegistry` for package FFI functions
- Maintains backward compatibility with static FFI

## Package Structure

```
PackageName/
├── package.midori           # TOML manifest (required)
├── PackageName.mdr          # Main module at package root (required)
├── native/                  # Native FFI implementation (optional)
│   ├── Cargo.toml          # Build configuration
│   └── src/
│       └── lib.rs          # FFI functions
├── lib/                     # Pre-built binaries (optional)
│   ├── windows/x64/
│   │   └── packagename.dll
│   ├── linux/x86_64/
│   │   └── libpackagename.so
│   └── macos/
│       └── libpackagename.dylib
├── examples/                # Usage examples
└── README.md               # Documentation
```

## Package Manifest

The `package.midori` file uses TOML format:

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
# Future: package dependencies

[ffi]
enabled = true
library_name = "packagename"

[ffi.functions]
"MIDORI_FFI_PackageName_Function" = "native_function_name"

[build]
cmake_minimum_version = "3.24"
cpp_standard = "23"

[prebuilt]
# Optional pre-built binary paths and checksums
```

## Creating a Package

### 1. Module Definition

The main module file must be at the package root for system imports:

```midori
module PackageName
public export
{
    FunctionName
}

foreign "MIDORI_FFI_PackageName_Function" FunctionName : fn(ArgType, ...) -> RetType;
```

### 2. FFI Implementation

Native functions must match the FFI signature:

```rust
// Rust example
use std::os::raw::c_void;

#[no_mangle]
pub extern "C" fn native_function_name(args: *mut *mut c_void, ret: *mut c_void) {
    unsafe {
        // For primitive types (Int, Float, Bool):
        // args[i] contains the value directly, cast pointer to i64
        let arg1 = *args.offset(0) as i64;

        // For strings:
        // args[i] is pointer to C string
        let text_ptr = *args.offset(0) as *const i8;

        // Return value: write to ret
        let result = ret as *mut i64;
        *result = computed_value;
    }
}
```

### 3. Building

```bash
cd native
cargo build --release
cp target/release/packagename.dll ../lib/windows/x64/
```

## Using Packages

### 1. Set MIDORI_PATH

The `MIDORI_PATH` environment variable must include:
- Package directory
- MidoriPrelude (standard library)

```bash
export MIDORI_PATH="/path/to/packages/PackageName:/path/to/MidoriPrelude"
```

### 2. Import in Code

```midori
import { <PackageName> }
import { <IO> }

def result = PackageName::FunctionName(arg1, arg2);
IO::PrintLine("Result: " ++ (result as Text));
```

### 3. Run

```bash
midori your_program.mdr
```

## FFI Function Signature

All package FFI functions use this C signature:

```c
void function_name(void** args, void* ret)
```

### Argument Handling

The `args` array contains `MidoriValue` data:

**Primitive types** (Int, Float, Bool):
- `args[i]` contains the value's bytes directly (8 bytes)
- Access: cast pointer to type: `*args.offset(i) as i64`

**Text/String types**:
- `args[i]` is a pointer to null-terminated C string
- Access: `*args.offset(i) as *const i8`

**Array types**:
- `args[i]` points to `ArrayArgument` struct
```c
struct ArrayArgument {
    void* data;      // Pointer to first element
    int length;      // Number of elements
};
```

### Return Value

The `ret` parameter points to a `MidoriValue` (8 bytes):
- Cast to appropriate type: `ret as *mut i64`
- Write result: `*result = value;`

## Implementation Details

### Library Loading

**Windows**: Uses `LoadLibraryW`, `FreeLibrary`, `GetProcAddress`
**Unix/macOS**: Uses `dlopen`, `dlclose`, `dlsym`

Libraries are loaded with flags:
- Windows: Default
- Unix: `RTLD_LAZY | RTLD_LOCAL`

### Function Lookup Order

1. `MidoriFFIRegistry` - Built-in standard library functions
2. `DynamicFFIRegistry` - Package FFI functions
3. Error if not found

### Thread Safety

`DynamicFFIRegistry` is thread-safe:
- `std::mutex` protects all operations
- Singleton pattern via `GetInstance()`
- Safe concurrent function lookups

### Memory Management

- Libraries remain loaded for program lifetime
- `DynamicFFIRegistry` destructor unloads libraries
- No manual cleanup required

## Compilation Integration

During compilation (`ModuleManager::GenerateBuildGraphImpl`):

1. Import resolution locates module file
2. Check for `package.midori` in parent directory
3. If found, parse manifest
4. If FFI enabled, locate library path (platform-specific)
5. Load library via `DynamicFFIRegistry::LoadLibraryWithFunctions`
6. Register all FFI functions
7. Continue with normal module compilation

## Runtime Integration

During FFI call execution (`VirtualMachine::CALL_FOREIGN`):

1. Pop function name from stack
2. Look up in `MidoriFFIRegistry` (built-in)
3. If not found, look up in `DynamicFFIRegistry`
4. If found, call function with arguments
5. Push return value to stack

## Platform-Specific Paths

The package manifest can specify different library paths per platform:

```toml
[prebuilt]
windows_x64 = { path = "lib/windows/x64/lib.dll", checksum = "sha256:..." }
linux_x86_64 = { path = "lib/linux/x86_64/lib.so", checksum = "sha256:..." }
macos_arm64 = { path = "lib/macos/liblib.dylib", checksum = "sha256:..." }
```

`PackageManifest::GetFFILibraryPath()` automatically selects the correct path based on the current platform.

## External Dependencies

**TOML Parser**: [toml11](https://github.com/ToruNiina/toml11)
- Added via CMake `FetchContent`
- Version: v4.2.0
- Header-only library
- C++23 compatible

## Future Enhancements

- Package manager CLI tool
- Dependency resolution
- Version constraints and compatibility checking
- Package registry/repository
- Cryptographic checksum verification
- Automatic CMake builds for native code
- WASM support for packages
