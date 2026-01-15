# Runtime Architecture

This document describes the architecture of the Midori interpreter runtime.

## Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                          MidoriRuntime                              │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────┐   │
│  │  GlobalVariables │  │   Thread Pool    │  │ Cross-VM Objects │   │
│  │                  │  │                  │  │  (deep-copied)   │   │
│  └──────────────────┘  └──────────────────┘  └──────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
         │                       │
         ▼                       ▼
┌─────────────────────┐  ┌─────────────────────┐  ┌─────────────────────┐
│   VirtualMachine    │  │   VirtualMachine    │  │   VirtualMachine    │
│  ┌───────────────┐  │  │  ┌───────────────┐  │  │  ┌───────────────┐  │
│  │ MidoriAllocator│  │  │  │ MidoriAllocator│  │  │  │ MidoriAllocator│  │
│  │ (malloc/free) │  │  │  │ (malloc/free) │  │  │  │ (malloc/free) │  │
│  └───────────────┘  │  │  └───────────────┘  │  │  └───────────────┘  │
│  ┌───────────────┐  │  │  ┌───────────────┐  │  │  ┌───────────────┐  │
│  │GarbageCollector│  │  │  │GarbageCollector│  │  │  │GarbageCollector│  │
│  │ (Mark-Sweep)  │  │  │  │ (Mark-Sweep)  │  │  │  │ (Mark-Sweep)  │  │
│  └───────────────┘  │  │  └───────────────┘  │  │  └───────────────┘  │
└─────────────────────┘  └─────────────────────┘  └─────────────────────┘
      VM 1                         VM2                VM3
```

## Components

### MidoriRuntime

The central coordinator for async task execution.

**Responsibilities:**
- Manages global variables (shared across VMs)
- Provides thread pool for async task execution
- Deep-copies return values from worker VMs to prevent dangling pointers

**Key Methods:**
- `SpawnTask(future, closure)` – Queues an async task to the thread pool
- `DeepCopyForCrossVM(value, gc)` – Deep-copies heap objects for safe cross-VM transfer

### MidoriAllocator

A thin wrapper around system malloc/free.

```cpp
void* Allocate(size_t size) { return std::malloc(size); }
void Free(void* ptr) { std::free(ptr); }
```

**Characteristics:**
- No size classes or page management
- Relies on system allocator for performance
- Each VM has its own allocator instance (no sharing)

### GarbageCollector

A simple mark-sweep garbage collector, one per VM.

**Configuration:**
| Parameter | Value |
|-----------|-------|
| Threshold | ~2 MB (`512000 * 4` bytes) |
| Algorithm | Mark-Sweep |
| Scope | Per-VM (no cross-VM coordination) |

**Collection Flow:**
1. Check `ShouldCollect()` after allocation-heavy operations
2. Gather roots from VM's stacks and globals
3. Mark: Trace reachable objects from roots
4. Sweep: Delete unmarked objects

```cpp
void TryCollect() {
    if (m_gc.ShouldCollect()) {
        m_gc.ReclaimMemory(GetGarbageCollectionRoots());
    }
}
```

### VirtualMachine

A bytecode interpreter with its own heap.

**Key Members:**
- `m_allocator` – Per-VM allocator
- `m_gc` – Per-VM garbage collector
- Value stack and call stack
- Pointer to shared globals via runtime

**Allocation:**
```cpp
template<typename T>
MidoriTraceable* AllocateTraceable(T&& arg, PointerTag tag) {
    void* mem = m_allocator.Allocate(sizeof(MidoriTraceable));
    MidoriTraceable* traceable = new(mem) MidoriTraceable(std::forward<T>(arg));
    m_gc.RegisterObject(traceable);
    return MidoriTaggedPointer(traceable, tag);
}
```

## Memory Model

### Per-VM Isolation

Each VM has its own:
- Allocator (malloc/free wrapper)
- Garbage collector (tracks only its own objects)
- Heap objects (not shared between VMs)

### Cross-VM Value Transfer

When an async task completes, its return value is deep-copied into runtime-managed memory:

```cpp
// In SpawnTask, before worker VM destruction:
MidoriValue result = DeepCopyForCrossVM(vm.GetAsyncResult(), vm.GetGC());
future->SetResult(result);
// Worker VM can now safely be destroyed
```

This prevents dangling pointers when the worker VM's heap is freed.

## Concurrency Model

### Shared State

| Resource | Sharing |
|----------|---------|
| Global Variables | Shared (via runtime) |
| Captured References | Point to parent VM's heap |
| Local Allocations | Per-VM only |

### Race Conditions

> [!WARNING]
> **Concurrent mutation of captured mutable references is undefined behavior.**

When multiple async tasks capture the same mutable reference (e.g., an array), concurrent modifications create data races:

```midori
let arr = [1, 2, 3];
spawn { arr.push(4); }  // Race!
spawn { arr.push(5); }  // Race!
```

The runtime does **not** prevent this. Users must:
- Avoid concurrent mutation of shared captures
- Use Futures for synchronization (spawn-then-await pattern)

## GC Trigger Points

Automatic GC is triggered after garbage-producing operations:
- `CONCAT_ARRAY` – Old arrays become garbage
- `CONCAT_TEXT` – Old texts become garbage

```cpp
case OpCode::CONCAT_ARRAY:
    // ... create new array ...
    left = AllocateTraceable(std::move(result), PointerTag::ARRAY);
    TryCollect();  // Check if GC needed
    break;
```

## Thread Safety

| Component | Thread Safety |
|-----------|---------------|
| MidoriAllocator | Per-VM (no sharing) |
| GarbageCollector | Per-VM (no sharing) |
| Global Variables | Shared (user must avoid races) |
| Cross-VM Objects | Mutex-protected in runtime |
