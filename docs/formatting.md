# Formatting

Midori ships a first-party formatter through:

```powershell
Midori.exe fmt <file|dir>
```

## Commands

Format a single file to stdout:

```powershell
Midori.exe fmt src/Main.mdr
```

Rewrite files in place:

```powershell
Midori.exe fmt src -w
```

Check whether formatting would change anything:

```powershell
Midori.exe fmt test --check
```

Machine-readable summary:

```powershell
Midori.exe fmt src --check --format json
```

## Canonical Style

The formatter currently enforces:

- 4-space indentation
- one command or declaration per line
- blank-line separation between top-level sections
- spaces after commas and around assignment / arrow operators
- normalized brace and block indentation
- normalized string literal escaping
- preserved `//` and `/* ... */` comments

## Comment Rules

Comments are kept with these attachment rules:

- leading comments stay attached to the next declaration or statement
- trailing comments stay attached to the line they originally followed
- inline block comments remain inline when they sit between tokens on the same line

`midori fmt` is intended to be idempotent for both comment-free and comment-bearing
source files.
