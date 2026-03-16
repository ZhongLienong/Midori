# Runtime Architecture

Midori runs programs on a single `VirtualMachine` execution path.

## Execution Flow

1. The compiler produces one `MidoriExecutable`.
2. Native and WASM entry points construct `VirtualMachine(std::move(executable))`.
3. The VM executes the bootstrap procedure, module initializers, and user code through `Execute()`.

There is no secondary runtime scheduler, worker pool, or alternate execution mode.

## VirtualMachine

`VirtualMachine` owns:

- The value stack and call stack.
- The executable and global variable array for the running program.
- The allocator and mark-and-sweep garbage collector.
- Caches for procedure entry points, static closures, and interned literals.

## Closures and Captures

Midori has one closure-capture model:

- Uncaptured functions use `MAKE_FUNCTION`.
- Capturing functions use `MAKE_CLOSURE` followed by `BIND_CAPTURES`.
- Captured locals are promoted to `MidoriCellValue` boxes so nested closures preserve by-reference semantics inside the same VM.
- Closure reads and writes use `GET_CELL` / `SET_CELL`.

Non-captured locals continue to use `GET_LOCAL*` / `SET_LOCAL*`.

## Memory Model

All GC-managed objects are VM-local:

- Text
- Arrays
- Structs
- Unions
- Closures
- Cell boxes
- Range objects

The garbage collector traces values reachable from the VM stacks, globals, closure environments, and nested aggregate objects.

## Globals

Global variables live in the executable's global array and are accessed through:

- `DEFINE_GLOBAL`
- `GET_GLOBAL`
- `SET_GLOBAL`

The runtime does not maintain shared-global indirection or cross-VM copies.
