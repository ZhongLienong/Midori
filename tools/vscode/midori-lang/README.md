# Midori VSCode Extension

This sample extension provides:

- `.mdr` file association
- TextMate syntax highlighting
- on-save diagnostics powered by `Midori.exe check <file> --format json`
- quick-fix entries that surface Midori suggestions

## Local Use

1. Open this folder in VSCode extension development mode.
2. Set `midori.executablePath` if `Midori.exe` is not already on your `PATH`.
3. Open a `.mdr` file and save to refresh diagnostics.
