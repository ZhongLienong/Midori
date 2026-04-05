# Project Standard

This document defines the standard layout and manifest for a Midori project or
package workspace.

## Active Manifest

Midori looks for `project.midori` first.

If no project manifest exists, a package root with `package.midori` is treated
as the active manifest instead. In that mode:

- the workspace name comes from `[package].name`
- the entry file comes from `[package.modules].main`
- `source_dir` defaults to `.`
- `packages_dir` defaults to `packages`
- `prelude_dir` defaults to `MidoriPrelude`

## Manifest

Example:

```toml
[project]
name = "MyApp"
entry = "src/Main.mdr"
source_dir = "src"
packages_dir = "packages"
prelude_dir = "MidoriPrelude"
midori_path = ["registry", "../shared-packages"]

[dependencies]
Greeter = "^1.2.0"
Image = ">=0.4.0, <0.5.0"

[test]
dir = "test"
timeout_ms = 30000
```

### Fields

- `name` (optional): display name for tools
- `entry` (optional): entry source file for tooling
- `source_dir` (optional): source directory for project modules. When omitted,
  Midori uses the conventional `src` directory.
- `packages_dir` (optional): project-local vendored package directory. This is
  where `midori install` writes resolved packages as
  `packages/<name>-<version>/`. When omitted, Midori uses the conventional
  `packages` directory.
- `prelude_dir` (optional): directory containing `MidoriPrelude`. When omitted,
  Midori uses the conventional `MidoriPrelude` directory.
- `midori_path` (optional): extra search paths and package index roots, relative
  to the project root unless absolute
- `[dependencies]` (optional): top-level table of direct package dependencies,
  mapping package names to version constraints
- `[test].dir` (optional): test directory used by `midori test`. Defaults to
  `test`.
- `[test].timeout_ms` (optional): per-test timeout budget enforced by
  `midori test`. Defaults to `30000`.

Dependency constraints use the same syntax as `package.midori`, including caret,
tilde, exact, comparison, and compound ranges.

## Layout

```text
MyApp/
  project.midori
  midori.lock
  src/
  test/
  packages/
  MidoriPrelude/
  native/
  README.md
```

`midori.lock` is written after dependency resolution. `packages/` contains the
vendored package sources that back the current local package workflow.

## Package Workflow

Projects declare direct dependencies in the top-level `[dependencies]` table.
You can edit that table manually or use the CLI:

- `midori install`
- `midori install <package> [--version <constraint>]`
- `midori update [package]`
- `midori remove <package>`
- `midori list`

`midori install` resolves direct and transitive dependencies, vendors the chosen
versions into `packages/`, and writes `midori.lock`.

Because remote registry fetching is not implemented yet, reproducible dependency
setups currently depend on keeping the vendored `packages/` directory or some
other local package root available.

## Module Naming

Module names should mirror paths relative to `source_dir`:

`src/Foo/Bar.mdr` -> `module Foo.Bar`

If you keep modules at the project root, set `source_dir = "."`.

## MIDORI_PATH Behavior

When Midori operates inside a project, it prepares an effective `MIDORI_PATH`
from the resolved package graph and the active manifest.

The effective search path order is:

1. `source_dir`
2. resolved package directories in dependency order
3. `midori_path` entries from the active manifest
4. `prelude_dir`
5. existing `MIDORI_PATH` entries from the environment

Existing environment entries are appended after project-managed paths, duplicate
paths are removed, and missing directories are skipped.

`packages_dir` is no longer treated as a single flat import root during project
resolution. Instead, Midori resolves packages first and then adds each selected
package directory to the search path individually.

## Project Initialization

Use the CLI to scaffold a project:

```text
midori init [path] [--name <project_name>]
```

This creates:

- `project.midori`
- `src/Main.mdr`
- `test/`
- `packages/`
