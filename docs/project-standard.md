# Project Standard

This document defines the standard layout and manifest for a Midori project.

## Manifest

A project uses a `project.midori` file in the project root. If `project.midori`
is not present, a `[project]` table inside `package.midori` is also accepted.
When both exist, `project.midori` takes precedence.

Example:

```toml
[project]
name = "MyApp"
entry = "src/Main.mdr"
source_dir = "src"
packages_dir = "packages"
prelude_dir = "MidoriPrelude"
midori_path = ["shared", "../common"]
```

### Fields

- `name` (optional): Display name for tools.
- `entry` (optional): Entry source file for tooling.
- `source_dir` (optional): Source directory to add to `MIDORI_PATH`.
  Defaults to `src` if it exists.
- `packages_dir` (optional): Directory of installed packages.
  Defaults to `packages` if it exists.
- `prelude_dir` (optional): Directory containing `MidoriPrelude`.
  Defaults to `MidoriPrelude` if it exists.
- `midori_path` (optional): Extra search paths (relative to project root unless absolute).

## Layout

```
MyApp/
  project.midori
  src/
  packages/
  native/
  lib/
  README.md
```

## Module Naming

Module names should mirror paths relative to `source_dir`:

`src/Foo/Bar.mdr` -> `module Foo.Bar`

If you keep modules at the project root, set `source_dir = "."`.

## MIDORI_PATH Behavior

When running Midori on a file inside a project, the CLI prepends these directories
to `MIDORI_PATH` (in order):

1. `source_dir`
2. `packages_dir`
3. `midori_path` entries (in listed order)
4. `prelude_dir`

Existing `MIDORI_PATH` entries are appended after these, and duplicates are removed.

## Project Initialization

Use the CLI to scaffold a project:

```
Midori.exe init [path] [--name <project_name>]
```

This creates:
- `project.midori`
- `src/Main.mdr`
- `packages/`
