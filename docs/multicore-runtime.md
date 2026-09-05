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

- `spawn` resolves the callee at compile time (must be a named `def Name = fn(...)`, not a closure)
- `join` blocks and returns the typed result (not text)
- `channel<T>(capacity)` creates a typed bounded channel
- `->` (send) and `<-` (receive) are type-checked binary/unary operators

Auxiliary operations: `close(ch)`, `is_done(w)`, `cancel(w)`.

## Worker Lifecycle

1. **Spawn**: `spawn Proc(args...)` creates a new `Worker` which:
   - Creates a `std::jthread`
   - Constructs a new `VirtualMachine` sharing the executable's bytecode
   - Takes a snapshot of dynamic FFI functions from `SharedLibraryCache`
   - Validates FFI thread safety before execution
   - Deep-copies arguments via `ValueTransfer` into the worker VM's stack
   - Calls `VirtualMachine::Execute()` — the same dispatch loop the main VM uses

2. **Join**: `join w` blocks until the worker finishes and returns the typed
   result via `SerializedValue` deserialization. If the worker failed, the error
   propagates as a runtime error in the joining VM, keeping the worker's own
   error code.

3. **Cancel**: `cancel(w)` requests cooperative cancellation via
   `std::jthread::request_stop()`. The worker observes the request at
   safepoints: loop back-edges (`JUMP_BACK`), tail calls (`TAIL_CALL`), and the
   return of any foreign call. A worker blocked in `ch -> v` or `<- ch` is woken
   immediately, because channel waits take the stop token, and the builtin
   `Sleep` is interruptible for the same reason. On observing cancellation the
   worker terminates with the `WorkerCancelled` runtime error.

   Cancellation is cooperative, so it is bounded by whatever the worker is
   currently doing. Blocking calls that do not consult the stop token — stdin
   reads and third-party dynamic FFI — still run to completion first. See
   `src/Common/Cancellation/Cancellation.h`.

4. **Poll**: `is_done(w)` checks if the worker has completed without blocking.

## Failure Propagation

- If a worker fails, the `Worker` captures both the error message and the
  originating `RuntimeErrorCode`
- On `join`, the error propagates as a runtime error in the joining VM carrying
  that same code, so a cancelled worker surfaces as `error[WorkerCancelled]`
  rather than being flattened into a generic internal error
- If a worker is never joined and failed, the destructor prints the message to
  standard output

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
- `close(ch)` — closes the channel, unblocks all waiters

There is no bounded or non-blocking receive at the language level: a receive
waits until a value arrives, the channel closes, or the worker is cancelled.
`Channel::TryReceive` exists in the runtime but has no opcode or syntax, so it
is currently unreachable from Midori code. See
`docs/plan/concurrency-backlog.md`.

Internally: `std::mutex` + `std::condition_variable_any` +
`std::deque<SerializedValue>`. The waits are stop-token-aware, which is what
makes a channel-blocked worker cancellable.

Channel handles are raw `Int` values managed by `ChannelRegistry`, not
VM-heap objects. `Channel<T>` is itself `Transferable`, so channels can be
passed directly to spawned workers. A channel is reclaimed from the registry
once it is both closed and drained; operations on a reclaimed handle behave
like operations on a closed channel.

## Relationship to OS Threads

1:1 mapping: each worker maps to one OS thread via `std::jthread`. Workers are
meant to be few and long-lived, not lightweight tasks. Spawning thousands of
workers will exhaust OS thread limits.

## Single-Threaded Overhead

Nearly all of the concurrency system — `Worker`, `Channel`, the registries,
`ValueTransfer`, FFI thread-safety validation — lives outside
`VirtualMachine::ExecuteLoop()` and costs the main VM nothing.

The exception is cancellation. Making a running worker stoppable requires
safepoints *inside* the dispatch loop, so `ExecuteLoop()` now checks
`IsCancellationRequested()` at loop back-edges, at tail calls, and after
foreign calls return. The check is `m_stop_possible && m_stop_token.stop_requested()`,
and `m_stop_possible` is false for the main VM because only `Worker` ever calls
`SetStopToken`. The main VM therefore pays one never-taken, correctly predicted
branch at those sites and never touches the atomic. GC triggers, allocator
calls, stack operations, and FFI dispatch remain identical for main and worker
VMs.

The `DynamicFFIRegistry` refactor replaces a Meyer's singleton `GetInstance()`
(hidden branch + pointer load on every call) with a direct member lookup on the
VM instance (equal or cheaper).

## Future Extensibility

Tracked with triggers in `docs/plan/concurrency-backlog.md`; none of it is
scheduled work.

- Bounded and multi-channel waiting (`try_receive`, `select`, timeouts)
- M:N scheduling (green threads mapped to a thread pool)
- Work stealing for automatic load balancing
- Shared-memory opt-in for performance-critical workloads
- `Future<T>` / `Promise<T>` as single-value channels
- `ParallelMap`, `ParallelFor`, `ParallelReduce` collection helpers
