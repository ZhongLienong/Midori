# Package System

Midori now has a local package dependency system built around `package.midori`,
`project.midori`, `midori.lock`, and project-local vendoring into `packages/`.
The current implementation resolves from package directories that already exist
on disk. Remote registry fetch and publish support are still deferred.

## What Exists Today

- semantic version parsing and comparison, including prerelease and build metadata
- dependency constraints in `project.midori` and `package.midori`
- compiler version validation through `package.midori` `midori_version`
- local package index scanning across project, cache, and path roots
- dependency resolution with cycle and version-conflict diagnostics
- project-local vendoring into `packages/<name>-<version>/`
- `midori.lock` generation and reuse
- package source checksum recording in the lockfile
- checksum verification for selected prebuilt native libraries
- CLI package commands: `midori install`, `midori update`, `midori remove`, `midori list`
- import-triggered dynamic FFI loading for resolved packages

## Still Deferred

- remote registry fetch and publish workflows
- automatic native builds from `native/` or `[build]`
- manifest-driven export enforcement beyond normal module `public export`
- targeted `midori update <package>` resolution; the current command refreshes the whole graph
- full garbage collection of every newly orphaned transitive vendored package directory

Because registry fetching is not implemented yet, fully reproducible installs on
a clean machine currently depend on the vendored `packages/` directory or some
other local package root already being available.

## Package Discovery

Resolution is based on a local package index. Midori scans these roots, in this
order:

1. the active project's `packages_dir` or `packages/`
2. the global cache directory
3. `project.midori` `midori_path` entries
4. existing `MIDORI_PATH` entries

Each root contributes:

- the root itself, if it contains `package.midori`
- any immediate child directories that contain `package.midori`

Available versions are sorted by:

1. highest semantic version
2. root priority for ties
3. path for deterministic ties

That means a newer compatible version in a lower-priority root still wins over
an older version in a higher-priority root.

On Windows the global cache root is `%LOCALAPPDATA%/Midori/cache` when
available. On other systems Midori falls back to `~/.midori/cache`.

## Resolution And Installation

When Midori prepares a project package environment, it:

1. loads the active manifest from `project.midori`, or from `package.midori`
   when no project manifest exists
2. parses direct dependencies and version constraints
3. prefers `midori.lock` when the root manifest checksum still matches and the
   locked package directories are available
4. otherwise scans the local index, resolves the highest compatible versions,
   and vendors them into `packages/<name>-<version>/`
5. writes a new `midori.lock`
6. rebuilds `MIDORI_PATH` from the resolved package graph and project settings

The effective search path order inside a project is:

1. `source_dir`
2. resolved package directories in dependency order
3. `midori_path` entries from the active manifest
4. `prelude_dir`
5. existing `MIDORI_PATH` entries from the environment

Missing directories are skipped and duplicate paths are removed.

## Vendored Layout

A resolved package is copied into the project-local packages directory using a
versioned folder name.

```text
MyApp/
  project.midori
  midori.lock
  packages/
    Greeter-1.2.0/
      package.midori
      Greeter.mdr
      lib/
        windows/x64/greeter.dll
```

The resolver and lockfile then refer to that vendored directory as the active
package location.

## CLI Workflow

- `midori install`
  Resolve current dependencies, vendor packages locally, and update
  `midori.lock`.
- `midori install <package> [--version <constraint>]`
  Add a direct dependency to the active manifest, then resolve and install.
  If `--version` is omitted, Midori looks up the highest locally available
  version and writes a caret constraint such as `^1.2.0`.
- `midori update [package]`
  Force a fresh resolve and rewrite `midori.lock`. The optional package name is
  currently validated, but the command still refreshes the full dependency
  graph.
- `midori remove <package>`
  Remove a direct dependency from the active manifest, refresh the lockfile, and
  remove unused vendored directories for that package name.
- `midori list`
  Print the resolved dependency tree. Midori prefers `midori.lock` and falls
  back to a fresh resolve when needed.

All four commands operate on the active manifest from the current directory:
`project.midori` if present, otherwise `package.midori`.

## Manifest Format

`package.midori` is parsed by `PackageManifest`.

### `[package]`

Recognized fields:

- `name`
- `version`
- `authors`
- `description`
- `license`
- `repository`
- `midori_version`

Current validation:

- `version` must parse as semantic versioning
- `midori_version` must parse as a version constraint
- the current compiler version must satisfy `midori_version`

### `[package.modules]`

Recognized fields:

- `main`
- `exports`

Notes:

- `main` identifies the package entry module
- `exports` is retained as manifest metadata
- actual visibility is still controlled by the module source with
  `public export` and `private export`

### `[dependencies]`

`[dependencies]` is a string table of package name to version constraint.

Supported constraint forms include:

- `^1.2.3`
- `~1.2.3`
- `=1.2.3`
- `>=1.0.0`
- `<2.0.0`
- `>=1.0.0, <2.0.0`

Constraints are validated during manifest load and resolved transitively.

### `[ffi]`

Recognized fields:

- `enabled`
- `library_name`
- `functions`

`functions` maps Midori foreign names to concrete exported symbol names inside
the shared library.

### `[build]`

Recognized fields:

- `cmake_minimum_version`
- `cpp_standard`

This section is still metadata only. Midori does not invoke a native build tool
from the manifest today.

### `[prebuilt]`

Recognized platform keys:

- `windows_x64`
- `linux_x86_64`
- `macos_arm64`
- `macos_x86_64`

Each entry contains:

- `path`
- `checksum`

When a matching prebuilt entry exists, Midori uses that path for the native
library and verifies its checksum before loading.

## Lockfile

The lockfile is `midori.lock` in the project root.

```toml
# Auto-generated by Midori. Do not edit manually.
[metadata]
midori_version = "..."
generated = "2026-04-05T12:00:00Z"
manifest_checksum = "sha256:..."

[[package]]
name = "Greeter"
version = "1.2.0"
source = "local:packages/Greeter-1.2.0"
checksum = "sha256:..."
dependencies = []
```

`manifest_checksum` tracks the active root manifest file. If it changes, Midori
treats the lockfile as stale and re-resolves.

Each package `checksum` is computed from:

- `package.midori`
- all `.mdr` files under the package directory

When a locked package directory is missing or its manifest no longer matches the
lockfile entry, Midori falls back to a fresh resolve. When package source
checksums drift, Midori emits warnings.

## Native Library Selection And Verification

For FFI-enabled packages, `PackageManifest::GetFFILibraryPath()` chooses the
library path like this:

1. use the matching `[prebuilt]` entry for the current platform when present
2. otherwise fall back to the conventional `lib/` path

Fallback paths:

- Windows: `lib/windows/x64/<library_name>.dll`
- macOS: `lib/macos/lib<library_name>.dylib`
- Linux: `lib/linux/x86_64/lib<library_name>.so`

`DynamicFFIRegistry` verifies the selected prebuilt checksum before loading:

- matching checksum: load continues
- checksum mismatch: load fails
- no checksum: load continues with a warning

Midori does not yet build native libraries automatically when no prebuilt binary
is available.

## Import-Time Package Loading

Package resolution happens before compilation by preparing the project search
path. During compilation, `ModuleManager` still loads dynamic FFI libraries on
demand:

1. an import resolves to an `.mdr` file on the effective `MIDORI_PATH`
2. `ModuleManager` checks that module's directory for `package.midori`
3. if `ffi.enabled = true`, Midori selects the library path and registers the
   declared functions through `DynamicFFIRegistry`

This keeps module import behavior file-based while the search path itself is now
lockfile-backed and package-aware.
