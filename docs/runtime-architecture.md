# Runtime Architecture

This document describes the current Midori runtime architecture after the concurrency redesign rollout.

## Execution Modes

`MidoriExecutable` carries a runtime mode:

- `ExecutionMode::SyncOnly`
  - Starts no async runtime infrastructure.
  - Runs directly on `VirtualMachine(MidoriExecutable&&)`.
- `ExecutionMode::AsyncEnabled`
  - Starts `MidoriRuntime` workers and shared async state.
  - Runs the root program as a scheduled task.

## Main Components

### `VirtualMachine`

Single VM type used in both modes.

- Own value stack/call stack.
- Own allocator and VM-local GC heap.
- Executes normal procedures and runtime tasks through `Execute()` / `ExecuteTask(...)`.

### `MidoriRuntime` (async-enabled mode only)

Coordinates concurrent execution:

- Worker thread pool (`std::jthread`).
- Worker count defaults to hardware concurrency (with safe bounds) and can be overridden with `MIDORI_ASYNC_WORKERS`.
- Global task queue storing owning task payloads.
- Shared global cells for async-capable global access.
- Cross-VM managed object storage for safe transfers.

### Shared Runtime Objects

- `MidoriFuture::FutureState` (shared completion state and result).
- `MidoriSharedCellState` / `MidoriSharedCellHandle` (shared by-reference storage; lock-free atomic value path in non-full-debug builds).
- Runtime task records (`FutureState` handle + closure payload).

## Local and Capture Storage Model

Local storage semantics are compiler-selected per procedure/local slot:

- `ValueLocal`
  - Backed by raw VM stack slots.
  - Accessed via `GET_LOCAL*` / `SET_LOCAL*`.
- `CellLocal`
  - Backed by stable `MidoriCellValue` heap boxes in the current VM.
  - Accessed via `GET_LOCAL_CELL*` / `SET_LOCAL_CELL*`.
- `SharedCellLocal`
  - Backed by runtime shared-cell handles for cross-VM async capture.
  - Accessed via `GET_LOCAL_SHARED*` / `SET_LOCAL_SHARED*`.

Capture opcodes are explicit:

- `BIND_CAPTURES` binds local captures to VM-local cell boxes.
- `BIND_CAPTURES_SHARED` binds captures to shared handles.

This removes deferred stack-pointer promotion from closure correctness.

## Async Scheduling Model

All workers run the same loop:

1. Pop a task from the queue.
2. Execute task closure on the worker-local VM.
3. Publish completion to `FutureState`.
4. Continue with next task.

There is no special main-worker VM role split.

## Await Behavior

`MidoriRuntime::AwaitFuture(...)` uses cooperative waiting:

- If the awaited future is not ready, the waiting worker can execute other queued tasks.
- Completion wakes waiters via future state signaling.
- This prevents full-pool stalls in nested async fan-out/fan-in patterns.

## Memory and Ownership Model

### VM-Local Memory

- Managed by per-VM GC (`GarbageCollector`).
- Not shared directly across workers.

### Shared Async Memory

- Shared futures and shared cells are handle-owned (`std::shared_ptr` based).
- Task queues store handles, not raw future pointers.
- Cross-VM result transfer deep-copies VM-owned traceables before publishing.

## Global Variables

- Sync-only code path uses the VM global array and legacy global opcodes.
- Async-capable code path uses shared-global opcodes backed by runtime shared cells.

## Shutdown Ordering

`MidoriRuntime` shutdown follows safe ordering:

1. Stop accepting new tasks.
2. Signal worker shutdown.
3. Join worker threads.
4. Destroy runtime-managed cross-VM objects.

This prevents use-after-free during teardown.
