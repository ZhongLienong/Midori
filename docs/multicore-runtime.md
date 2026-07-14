# Midori Multicore Runtime

## Execution Model: Isolated Workers

Midori uses an **isolated-worker** concurrency model. Each worker is a full
`VirtualMachine` instance with its own allocator, garbage collector, value
stack, call stack, and global variables. Workers share read-only bytecode
(`std::shared_ptr<const MidoriExecutable>`) but cannot share mutable values.

Communication between workers happens exclusively through message passing
(typed binary-serialized values via channels).

## What Is Shared vs Isolated

| Component | Shared? | Notes |
|-----------|---------|-------|
| Bytecode (`MidoriExecutable`) | Yes (read-only) | Via `shared_ptr<const>` |
| Builtin FFI table | Yes (immutable) | `constexpr` array, compile-time |
| Dynamic FFI function pointers | Snapshot per VM | Lock-free lookup during execution |
| Shared library handles | Yes (cache) | `SharedLibraryCache` singleton, load-time only |
| Value stack | No | Per-VM, 10k slots |
| Call stack | No | Per-VM, 10k slots |
| Heap / Allocator | No | Per-VM `MidoriAllocator` |
| Garbage collector | No | Per-VM, independent generational collection |
| Global variables | No | Per-VM, zero-initialized for workers |
| String literal cache | No | Per-VM |
| Static closure cache | No | Per-VM |

## Value Transfer and the `Transferable` Typeclass

Cross-worker value transfer is governed by the `Transferable<T>` typeclass.
Types that satisfy `Transferable` can be deep-copied across worker boundaries
via `ValueTransfer` (direct VM-to-VM copy for `spawn`/`join`) or serialized to
`SerializedValue` (binary format for channels).

**Built-in `Transferable` instances** (deep copy):
- `Int`, `Float`, `Byte`, `Word`, `Bool`, `Unit` — bitwise copy (no heap allocation)
- `Text` — new allocation in target VM, bytes copied
- `Array<T>` where `Transferable<T>` — new allocation, elements recursively transferred
- `Channel<T>` where `Transferable<T>` — handle copy (process-wide registry index)

**Derivable for user types**:
- Structs and unions can `deriving (Transferable)` if all fields/variants satisfy `Transferable`
- Tuples are transferable if all element types satisfy `Transferable`

**Never transferable** (no `Transferable` instance):
- Closures (`fn(A) -> B`) — capture VM-specific `MidoriCellValue` pointers
- Ranges — semantically non-portable
- `Worker<T>` — joining from a non-owner worker is undefined

Transferability is checked at compile time. Attempting to `spawn` with a
non-transferable argument or create a `Channel<T>` where `T` lacks a
`Transferable` instance produces a constraint-failure error.

Cycle detection is handled via a `PointerMap` that tracks already-transferred
traceables.

## Concurrency Syntax

Midori provides first-class concurrency expressions instead of FFI wrappers:

```midori
def w = spawn ComputeRow(42, 800);   // w : Worker<Int>
def result = join w;                  // result : Int

def ch = channel<Int>(10);            // ch : Channel<Int>
ch -> 42;                             // send: Bool (false if closed)
def val = <- ch;                      // receive: Int (blocks if empty)
```

- `spawn` resolves the callee at compile time (must be a named `defun`, not a closure)
- `join` blocks and returns the typed result (not text)
- `channel<T>(capacity)` creates a typed bounded channel
- `->` (send) and `<-` (receive) are type-checked binary/unary operators

Auxiliary operations: `try_receive(ch)`, `close(ch)`, `is_done(w)`, `cancel(w)`.

## Worker Lifecycle

1. **Spawn**: `spawn Proc(args...)` creates a new `Worker` which:
   - Creates a `std::jthread`
   - Constructs a new `VirtualMachine` sharing the executable's bytecode
   - Takes a snapshot of dynamic FFI functions from `SharedLibraryCache`
   - Validates FFI thread safety before execution
   - Deep-copies arguments via `ValueTransfer` into the worker VM's stack
   - Calls `VirtualMachine::Execute()` — the same unmodified dispatch loop

2. **Join**: `join w` blocks until the worker finishes and returns the typed
   result via `SerializedValue` deserialization. If the worker panicked, the
   error propagates as a runtime error in the joining VM.

3. **Cancel**: `cancel(w)` requests cooperative cancellation via
   `std::jthread::request_stop()`. The stop token is checked before execution
   starts, not inside `ExecuteLoop()`.

4. **Poll**: `is_done(w)` checks if the worker has completed without blocking.

## Failure Propagation

- If a worker panics, the error message is captured in the `Worker`
- On `join`, the error propagates as a runtime error in the joining VM with
  the worker's error message and stack trace context
- If a worker is never joined and panics, the destructor prints to stderr

## FFI Under Multicore

Dynamic FFI libraries declare thread safety in `package.midori`:

```toml
[ffi]
enabled = true
abi_version = 1
thread_safe = true
```

- Default: `thread_safe = false` (safe by default)
- Validation happens at worker construction time, before `ExecuteLoop()`
- If any loaded package has `thread_safe = false`, worker creation fails with
  a clear error naming the offending packages
- `thread_safe = true` means the native code uses no global mutable state, or
  manages its own synchronization

## Channels

Channels provide typed message passing between workers:

- `channel<T>(capacity)` — creates a bounded `Channel<T>`
- `ch -> value` — sends a value (blocks if full, returns `Bool`)
- `<- ch` — receives a value (blocks if empty, runtime error if closed and empty)
- `try_receive(ch)` — non-blocking receive returning `Union<Some: T, None: Unit>`
- `close(ch)` — closes the channel, unblocks all waiters

Internally: `std::mutex` + `std::condition_variable` + `std::deque<SerializedValue>`

Channel handles are raw `Int` values managed by `ChannelRegistry`, not
VM-heap objects. `Channel<T>` is itself `Transferable`, so channels can be
passed directly to spawned workers.

## Relationship to OS Threads

1:1 mapping: each worker maps to one OS thread via `std::jthread`. Workers are
meant to be few and long-lived, not lightweight tasks. Spawning thousands of
workers will exhaust OS thread limits.

## Zero Single-Threaded Overhead Guarantee

`VirtualMachine::ExecuteLoop()` is **never modified** by the concurrency
system. The dispatch loop, GC trigger, allocator calls, stack operations, and
FFI dispatch are identical for the main VM and for worker VMs.

Worker-specific behavior (cancellation, failure capture) lives **outside** the
dispatch loop, in wrapper code that calls `ExecuteLoop()` and inspects the
result afterward.

The `DynamicFFIRegistry` refactor replaces a Meyer's singleton `GetInstance()`
(hidden branch + pointer load on every call) with a direct member lookup on the
VM instance (equal or cheaper).

## Future Extensibility

- M:N scheduling (green threads mapped to a thread pool)
- Work stealing for automatic load balancing
- Shared-memory opt-in for performance-critical workloads
- `Future<T>` / `Promise<T>` as single-value channels
- `ParallelMap`, `ParallelFor`, `ParallelReduce` collection helpers
