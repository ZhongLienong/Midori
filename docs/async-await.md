# Async and Await in Midori

Midori provides concurrency through `async` expressions and `Future<T>`.

## Quick Example

```midori
import { <IO> }

defun compute(x: Int) : Int => x * x;

def work : Future<Int> = async compute(42);
def result : Int = await work;

IO::PrintLine("Result: " ++ (result as Text));
```

## Core Semantics

### `Future<T>`

`Future<T>` is the type produced by `async` and consumed by `await`.

```midori
Future<Int>
Future<Text>
Future<Array<Float>>
```

### `async expr`

- Evaluates `expr` on the runtime scheduler.
- Returns immediately with `Future<T>`.
- Captures preserve Midori by-reference semantics through compiler-selected cell storage.

### `await future`

- If ready, returns the result immediately.
- If not ready, the runtime waits until completion and returns `T`.
- If the future completed with an error, `await` raises a runtime error.

## Execution Modes

Midori has two execution modes selected at compile/link time:

1. `ExecutionMode::SyncOnly`
   - No async runtime startup.
   - Program runs on a single VM instance.
2. `ExecutionMode::AsyncEnabled`
   - Runtime scheduler and worker pool are started.
   - The root program executes as a normal task.

This split keeps non-async programs on a zero-extra-overhead path.

## Capture and Shared State

In async-enabled programs:

- Each local slot is classified by the compiler as one of:
  - `ValueLocal`: plain VM stack slot (`GET_LOCAL` / `SET_LOCAL`).
  - `CellLocal`: VM-local heap box (`MidoriCellValue`) for closure capture in the same VM.
  - `SharedCellLocal`: shared handle-backed cell for async/cross-VM capture.
- `CellLocal` and `SharedCellLocal` use dedicated local opcodes (`*_LOCAL_CELL*`, `*_LOCAL_SHARED*`).
- Capture binding is explicit:
  - `BIND_CAPTURES` binds closure captures to stable VM-local cell boxes.
  - `BIND_CAPTURES_SHARED` binds captures to shared cell handles.
- Globals accessed from async-capable procedures use shared global storage.
- Futures are backed by shared future state handles.

This preserves language-level reference behavior while keeping runtime memory-safe.

## Race Semantics

Midori currently allows language-level data races. Results can be nondeterministic.

The runtime still guarantees process safety:

- no use-after-free from task scheduling,
- no dangling future pointers in queues,
- no cross-VM raw pointer ownership transfer.

## Error Model

- Task failures mark their future as failed.
- `await` on a failed future raises a generic async runtime error.
- Unawaited task errors are not currently surfaced as structured diagnostics.

## Current Limitations

- No cancellation API.
- No timeout-aware await.
- No rich error payload propagation from task to awaiter.
