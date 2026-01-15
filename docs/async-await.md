# Async/Await in Midori

Midori provides concurrent execution through the `async`/`await` pattern with `Future<T>` types.

## Basic Usage

```midori
import { <IO> }

defun expensive_computation(x: Int) : Int => {
    def result = x * x;
    return result;
};

// Spawn concurrent task
def task : Future<Int> = async expensive_computation(42);

// Do other work while task runs...
IO::PrintLine("Task spawned, doing other work...");

// Block until result is ready
def result : Int = await task;
IO::PrintLine("Result: " ++ (result as Text));
```

## Key Concepts

### Future<T>

`Future<T>` represents a value of type `T` that will be available asynchronously:

```midori
Future<Int>           // Future resolving to an integer
Future<Text>          // Future resolving to text
Future<Array<Float>>  // Future resolving to an array
```

### async Expression

`async expr` spawns a concurrent task and immediately returns a `Future<T>`:

- The expression runs in a **separate worker VM** with its own heap
- Workers share global variables with the main VM
- Returns immediately without blocking

```midori
// These tasks run concurrently in separate VMs
def task1 = async compute(10);
def task2 = async compute(20);
def task3 = async compute(30);
```

### await Expression

`await future` blocks until the future completes and returns the unwrapped value:

```midori
def task : Future<Int> = async compute(42);
def result : Int = await task;  // Blocks until complete
```

## Variable Capture

Async blocks capture variables from their enclosing scope **by reference**:

```midori
defun make_tasks(values: Array<Int>) : Array<Future<Int>> => {
    def tasks : Array<Future<Int>> = [];

    for i in 0..1..#values {
        def value = values[i];  // Capture
        tasks ++= async {
            return value * value;
        };
    };

    return tasks;
};
```

> [!WARNING]
> **Captured references point to the parent VM's heap.** Concurrent mutation of shared captures is **undefined behavior**.

```midori
// UNSAFE - Race condition!
let arr = [1, 2, 3];
spawn { arr ++= 4; }
spawn { arr ++= 5; }  // Both tasks mutate same array concurrently
```

**Important**: When capturing loop variables, create a copy inside the loop:

```midori
// CORRECT - capture a copy
for i in 0..1..10 {
    def i_copy = i;
    tasks ++= async compute(i_copy);
};
```

## Return Values

Async task return values are **deep-copied** from the worker VM's heap to runtime-managed memory:

```midori
// Primitive types - copied by value
def int_task : Future<Int> = async 42;

// Heap types - deep-copied on completion  
def text_task : Future<Text> = async "Hello, async!";
def array_task : Future<Array<Int>> = async [1, 2, 3];
```

This prevents dangling pointers when the worker VM is destroyed.

## Runtime Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      MidoriRuntime                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────┐  │
│  │  Thread Pool    │  │ Global Variables│  │ Cross-VM    │  │
│  │  (N workers)    │  │   (shared)      │  │ Objects     │  │
│  └─────────────────┘  └─────────────────┘  └─────────────┘  │
└─────────────────────────────────────────────────────────────┘
         │                       │
         ▼                       ▼
┌─────────────────────┐  ┌─────────────────────┐
│   Main VM           │  │   Worker VM         │
│  ┌───────────────┐  │  │  ┌───────────────┐  │
│  │ Own Allocator │  │  │  │ Own Allocator │  │
│  │ Own GC        │  │  │  │ Own GC        │  │
│  └───────────────┘  │  │  └───────────────┘  │
└─────────────────────┘  └─────────────────────┘
```

**Key Points:**
- Each VM has its own allocator and garbage collector
- No stop-the-world coordination between VMs
- Return values are deep-copied before worker VM destruction
- Global variables are shared (user must avoid concurrent mutation)

## Best Practices

1. **Spawn all tasks first, then await** - Maximizes parallelism:
   ```midori
   // Good - tasks run in parallel
   def t1 = async work1();
   def t2 = async work2();
   def r1 = await t1;
   def r2 = await t2;

   // Bad - sequential execution
   def r1 = await async work1();
   def r2 = await async work2();
   ```

2. **Avoid concurrent mutation** - Don't modify captured references from multiple tasks

3. **Capture loop variables explicitly** - Create copies inside the loop

4. **Consider granularity** - Too many tiny tasks have overhead; too few reduce parallelism

## Limitations

- **No cancellation** - Once spawned, tasks run to completion
- **No timeouts** - Await blocks indefinitely
- **No inter-task communication** - Tasks cannot send messages to each other
- **Race conditions accepted** - Concurrent capture mutation is undefined behavior

## Performance Considerations

- Each async task allocates ~400KB for stacks
- Thread pool startup has one-time overhead
- Large return values (e.g., huge Arrays) incur deep-copy cost
- Per-VM GC runs independently (~2MB threshold)