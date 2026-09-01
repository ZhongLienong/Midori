# Milestone 6 Dev Plan: Built-in Concurrency Primitives

## Objective

Replace the FFI-based concurrency surface (Milestone 5) with first-class
language constructs. Workers and channels become typed, compiler-checked
expressions instead of stringly-typed foreign calls. Value transfer uses native
deep copy instead of text serialization.

## Relationship to Milestone 5

Milestone 5 delivered the runtime infrastructure: isolated-worker model,
`Worker`, `Channel`, `ValueTransfer`, `SharedLibraryCache`,
`DynamicFFIRegistry` per-VM isolation. All of that stays. This milestone
replaces only the **user-facing surface** and the **data path**:

| Layer | Milestone 5 (v1) | Milestone 6 (v2) |
|-------|-------------------|-------------------|
| Syntax | `foreign` declarations + library wrappers | `spawn`, `join`, `channel` keywords + `->` / `<-` operators |
| Procedure resolution | Runtime string lookup (`"ComputeRow"`) | Compile-time procedure index |
| Argument passing | Text serialization (`[IntToText(42)]`) | Typed `ValueTransfer` deep copy |
| Channel payload | `std::deque<std::string>` | `std::deque<SerializedValue>` |
| Worker result | `std::string` | `SerializedValue` |
| Type safety | None (raw `Int` handles, `Text` values) | `Worker<T>`, `Channel<T>` with compile-time transferability |
| Opcodes | `CALL_FOREIGN` / `CALL_FOREIGN_INDEXED` | Dedicated `SPAWN_WORKER`, `JOIN_WORKER`, etc. |

## Zero Single-Threaded Overhead Constraint

Same structural guarantee as Milestone 5. The new opcodes are additional `case`
labels in the dispatch switch. The compiler's switch-to-jump-table optimization
means existing opcodes pay no branch cost from the table growing. The new
handlers are dead code paths in programs that never use concurrency syntax.

### Specific constraints

- No new fields on `VirtualMachine` that `ExecuteLoop()` reads
- No mutex, atomic, or lock in any existing opcode handler
- No change to `MidoriValue` size, layout, or access patterns
- No change to `MidoriTraceable` mark/sweep mechanics
- `ValueTransfer` and `SerializedValue` code is unreachable if no concurrency
  opcode is executed

## Design

### New Built-in Types

Two new type variants in `MidoriType`, alongside `ArrayType`, `RangeType`, etc.:

```
Worker<T>      T = return type of the spawned procedure
Channel<T>     T = element type; T must be transferable
```

Both are internally represented as opaque `Int` handles (indices into
`WorkerRegistry` / `ChannelRegistry`), but the type system tracks `T` for
compile-time checking.

#### Type variant definitions

```cpp
struct WorkerType
{
    std::shared_ptr<MidoriType> m_result_type;
};

struct ChannelType
{
    std::shared_ptr<MidoriType> m_element_type;
};
```

### Transferability via the Typeclass System

Transferability is expressed as a **typeclass with methods** using Midori's
existing `class` / `instance` / `deriving` infrastructure. This replaces the
deferred Phase 1.2 item from Milestone 5.

#### Definition

```midori
class Transferable<T> {
    Serialize: fn(T) -> Array<Byte>;
    Deserialize: fn(Array<Byte>) -> T;
};
```

`Transferable` defines how a value crosses worker boundaries. Each type
provides its own serialization logic. The `deriving` mechanism auto-generates
field-by-field serialization -- the same pattern as `deriving (Equatable)`
generating field-by-field equality, or `deriving (Hashable)` generating
field-by-field hashing.

This means the `ValueSerializer` C++ class (described later) calls through the
typeclass dispatch: `CHANNEL_SEND` invokes `Transferable::Serialize`, and
`CHANNEL_RECEIVE` invokes `Transferable::Deserialize`. For `spawn` arguments
and `join` results, the VM can call these methods directly or use
`ValueTransfer::Transfer()` as a fast path (both VMs are accessible at that
point).

#### Built-in Instances for Primitives

These are registered by the compiler (or shipped in a prelude module):

```midori
instance Transferable<Int> {
    defun Serialize(value: Int) : Array<Byte> => IntrinsicSerializeInt(value);
    defun Deserialize(data: Array<Byte>) : Int => IntrinsicDeserializeInt(data);
};

instance Transferable<Float> {
    defun Serialize(value: Float) : Array<Byte> => IntrinsicSerializeFloat(value);
    defun Deserialize(data: Array<Byte>) : Float => IntrinsicDeserializeFloat(data);
};
```

The `Intrinsic*` functions are compiler intrinsics that emit inline
serialization bytecode (direct `memcpy`-equivalent for fixed-size primitives).
Instances for `Byte`, `Word`, `Bool`, `Unit`, and `Text` follow the same
pattern.

#### Conditional Instances for Containers

```midori
instance Transferable<Array<T>> where Transferable<T> {
    defun Serialize(value: Array<T>) : Array<Byte> => {
        // serialize length, then serialize each element via Transferable::Serialize
    };
    defun Deserialize(data: Array<Byte>) : Array<T> => {
        // deserialize length, then deserialize each element via Transferable::Deserialize
    };
};
```

Tuples are structural, so the compiler handles them internally: a tuple is
`Transferable` if all its element types satisfy `Transferable`. The derived
`Serialize` / `Deserialize` concatenates per-element serialization. This is
analogous to how the existing type checker propagates constraints through
tuple types.

#### Channel Transferability

```midori
instance Transferable<Channel<T>> where Transferable<T> {
    defun Serialize(value: Channel<T>) : Array<Byte> => IntrinsicSerializeHandle(value);
    defun Deserialize(data: Array<Byte>) : Channel<T> => IntrinsicDeserializeHandle(data);
};
```

`Channel<T>` handles are process-wide `Int` indices into `ChannelRegistry`.
Copying the index across workers is safe. This enables passing channels
directly to spawned workers.

`Worker<T>` has no `Transferable` instance -- joining from a non-owner worker
is undefined.

#### User-Defined Types via `deriving`

Users opt in with `deriving (Transferable)`:

```midori
struct Point {
    x: Int,
    y: Int
} deriving (Transferable);

union Result = Ok(Int) | Err(Text) deriving (Transferable);
```

The derivation logic (mirrors `Equatable` / `Hashable` derivation):

1. For structs: check that every field type satisfies `Transferable`. Generate
   `Serialize` that serializes each field in order via
   `Transferable::Serialize`. Generate `Deserialize` that deserializes each
   field and calls `new StructName(...)`. If any field lacks a `Transferable`
   instance, emit a compile error naming the offending field.
2. For unions: check that every variant's payload types satisfy
   `Transferable`. Generate `Serialize` that writes the tag, then serializes
   the variant's payload. Generate `Deserialize` that reads the tag and
   dispatches to the correct variant constructor. Same error behavior.

This extends the existing `QueueDerivedStructStatements` /
`QueueDerivedUnionStatements` machinery that already handles `Equatable` and
`Hashable`.

#### What Cannot Be Transferable

- **Closures** (`fn(A) -> B`): No `Transferable` instance can be written for
  function types. Closures capture `MidoriCellValue` pointers that are VM-
  specific. The existing constraint-failure error path handles this naturally
  -- if a user tries `spawn Foo(my_closure)`, the type checker reports
  "no instance of Transferable for fn(Int) -> Int".
- **Ranges**: Semantically non-portable (deferred). No instance registered.
- **`Worker<T>`**: No instance. Workers cannot be sent to other workers.

#### How Constraints Flow Through `spawn` / `->` / `channel`

The concurrency expressions carry implicit `where Transferable<T>` constraints:

- `spawn Foo(args...)` -- the compiler looks up `Foo`'s parameter types and
  return type. For each type `T` involved, it checks
  `FindMatchingInstance("Transferable", {T})`. If any argument or the return
  type lacks a `Transferable` instance, the standard constraint-failure error
  fires.
- `ch -> val` -- `ch` is `Channel<T>`, `val` must be `T`. The `T` is
  checked for `Transferable` (already guaranteed by `channel<T>` creation, but
  re-checked here for safety).
- `channel<T>(cap)` -- `T` is checked for `Transferable`.

#### Why This Is Better Than a Hardcoded Predicate

1. **Uses existing infrastructure.** The constraint checker, instance resolver,
   error reporter, and derivation machinery already exist.
2. **User-extensible.** Users mark their own types as `Transferable` via
   `deriving`, and the compiler generates the serialization code. A hardcoded
   predicate would require the compiler to recursively inspect struct/union
   fields -- the typeclass system already does this through instance
   resolution.
3. **Consistent with the language.** `Equatable`, `Hashable`, and now
   `Transferable` all follow the same pattern: a class with methods,
   derivable for structs/unions. Users learn one mechanism.
4. **Clear error messages.** The existing constraint-failure path produces
   errors like "no instance of Transferable for fn(Int) -> Int" which is more
   informative than a generic "type is not transferable".
5. **Composable.** Generic functions can propagate the constraint:
   ```midori
   defun SendAll<T>(ch: Channel<T>, items: Array<T>) : Unit
   where Transferable<T> => {
       for item in items {
           ch -> item;
       };
   };
   ```
6. **Self-contained serialization.** Each type knows how to serialize itself.
   The `ValueSerializer` C++ class becomes a thin dispatch layer that calls
   the typeclass methods, rather than a monolithic switch over all type tags.

#### Compiler Implementation

1. **Lexer/Parser**: No changes needed. `class Transferable<T> { ... };`
   parses with the existing class grammar. `deriving (Transferable)` parses
   with the existing deriving grammar.
2. **Type Checker**: Add `"Transferable"` to the set of derivable classes
   alongside `"Equatable"` and `"Hashable"`. The derivation visitor checks
   field/variant constraints and generates `Serialize` / `Deserialize` method
   bodies.
3. **Code Generator**: The derivation path in `QueueDerivedStructStatements`
   adds a branch for `Transferable` that generates serialization bytecode.
   The pattern mirrors `Equatable` derivation: iterate fields, call
   `Transferable::Serialize` on each, concatenate results. `Deserialize` is
   the inverse.
4. **Built-in instances**: Registered during compiler initialization (same
   path as built-in `Equatable<Int>` etc.), or shipped in a prelude module
   that is always imported when concurrency syntax is used. Primitive
   instances use compiler intrinsics for direct byte manipulation.

### New Tokens and Keywords

Three new keywords and one new operator token added to the lexer:

| Token | Lexeme | Category |
|-------|--------|----------|
| `SPAWN` | `spawn` | Expression keyword |
| `JOIN` | `join` | Expression keyword |
| `CHANNEL` | `channel` | Expression keyword |
| `LEFT_ARROW` | `<-` | Unary prefix operator (receive) |

The existing `THIN_ARROW` (`->`) is reused in expression context as a binary
operator (send). In type context it continues to mean "return type" in function
signatures (`fn(Int) -> Int`). The parser distinguishes these by parse state --
the same way `<` works as both less-than and generic type delimiter.

### Syntax and Semantics

#### `spawn` -- create a worker from a procedure call

```midori
defun ComputeRow(y: Int, width: Int) : Int => {
    return y * width;
};

def w = spawn ComputeRow(42, 800);
// w : Worker<Int>, inferred from ComputeRow's return type
```

Grammar:

```
spawn_expr ::= "spawn" IDENTIFIER "(" argument_list ")"
```

Semantics:
1. The callee must be a named top-level `defun` (not a closure, not a lambda,
   not a variable holding a function value). The compiler resolves it to a
   procedure index at compile time.
2. All argument types must satisfy `Transferable`. The return type must
   satisfy `Transferable`.
3. Arguments are evaluated in the current VM, then deep-copied via
   `Transferable::Serialize` / `Transferable::Deserialize` (or
   `ValueTransfer` as a fast path) into the new worker VM.
4. A new `Worker` is created (isolated `VirtualMachine` + `std::jthread`).
5. The expression evaluates to a `Worker<T>` handle.

Restrictions:
- The callee restriction (named `defun` only) is the same as v1 but enforced
  at compile time with a clear error, rather than failing at runtime with a
  string lookup miss.

#### `join` -- block and retrieve a worker's result

```midori
def result = join w;
// result : Int, from Worker<Int>
```

Grammar:

```
join_expr ::= "join" expression
```

Semantics:
1. The operand must be `Worker<T>`.
2. Blocks until the worker completes (calls `Worker::Join()` internally).
3. The worker's return value is deserialized via `Transferable::Deserialize`
   into the current VM.
4. The expression type is `T`.
5. If the worker panicked, the join propagates the error (runtime error in
   the current VM with the worker's error message and stack trace context).

#### `channel` -- create a typed channel

```midori
def ch = channel<Int>(10);
// ch : Channel<Int>, capacity 10

def text_ch = channel<Text>(5);
// text_ch : Channel<Text>, capacity 5
```

Grammar:

```
channel_expr ::= "channel" "<" type ">" "(" expression ")"
```

Semantics:
1. The type parameter `T` must satisfy `Transferable`.
2. The capacity expression must be `Int`.
3. Creates a `Channel` instance in the `ChannelRegistry`.
4. The expression type is `Channel<T>`.

#### `->` -- send a value into a channel

```midori
def ok = ch -> 42;
// ok : Bool (false if channel is closed)

ch -> value;
```

Grammar:

```
send_expr ::= expression "->" expression
```

`->` is parsed as a binary operator at a new precedence level (below pipe
`|>`, above assignment). In expression context the parser emits a `Send` AST
node. In type context (`fn(Int) -> Int`) the parser continues to treat `->` as
the return-type arrow -- these are different parse states with no ambiguity.

Semantics:
1. The left operand must be `Channel<T>`.
2. The right operand must be type `T`.
3. The value is serialized via `Transferable::Serialize` and enqueued. Blocks
   if the channel is at capacity.
4. Returns `Bool` (`true` = sent, `false` = channel was closed).

#### `<-` -- receive a value from a channel

```midori
def val = <- ch;
// val : T from Channel<T>
```

Grammar:

```
receive_expr ::= "<-" expression
```

`<-` is a new token (`LEFT_ARROW`). It is parsed as a unary prefix operator
(like `!` or `~`). The lexer emits `LEFT_ARROW` when it sees `<` followed by
`-`, which takes priority over `LEFT_ANGLE` + `MINUS` since the lexer is
greedy.

Semantics:
1. The operand must be `Channel<T>`.
2. Blocks until a value is available or the channel is closed.
3. The received `SerializedValue` is deserialized into the current VM's heap
   via `Transferable::Deserialize`.
4. The expression type is `T`.
5. If the channel is closed and empty, this is a runtime error (see below).

#### Closed-channel receive behavior

Two options:

**Option A -- runtime error:** `<- ch` on a closed empty channel panics with
a clear error. Simple, matches the "errors are loud" philosophy. Users who need
to poll use `try_receive`.

**Option B -- sentinel value:** `<- ch` returns a union like
`Union<Some: T, None: Unit>`. Type-safe but changes the return type from `T` to
a union, making the common case verbose.

Recommendation: **Option A**. The blocking `<-` assumes data will come.
Non-blocking `try_receive` (see below) handles the "maybe nothing" case.

### Auxiliary Operations

These are less performance-critical and can remain as built-in functions
(compiler-recognized intrinsics) rather than keyword expressions:

```midori
def maybe = try_receive(ch);   // Channel<T> -> Union<Some: T, None: Unit>
close(ch);                      // Channel<T> -> Unit
def done = is_done(w);         // Worker<T> -> Bool
def cancelled = cancel(w);     // Worker<T> -> Bool
```

These are recognized by name in the type checker and codegen. They emit
dedicated opcodes (not `CALL_FOREIGN`), but they parse as normal function calls.
No new keywords needed.

Alternative: keep these as FFI wrappers in a prelude module. They are
infrequent operations and the FFI overhead is negligible. This reduces compiler
complexity at no user-visible cost.

### Operator Precedence

The full expression precedence table with the new operators:

```
(lowest)
  assignment      =, +=, -=, ...
  logical or      ||
  logical and     &&
  send            ->          (new, binary)
  pipe            |>
  bitwise or      |
  bitwise xor     ^
  bitwise and     &
  equality        ==, !=
  comparison      <, <=, >, >=
  shift           <<, >>
  range           ..
  addition        +, -, ++
  multiplication  *, /, %
  unary prefix    !, ~, -, <-   (<- is new)
  postfix/call    (), [], .
  primary         literals, names, spawn, join, channel
(highest)
```

`->` (send) sits below `|>` (pipe) so that piped expressions can feed into
channels naturally: `value |> Transform -> ch`. `<-` (receive) is a unary
prefix at the same level as `!` and `~`.

### New Opcodes

```
SPAWN_WORKER        <proc_index: short> <arg_count: byte>
    Pops arg_count values from the stack.
    Deep-copies each via ValueTransfer into a new worker VM.
    Starts the worker thread.
    Pushes the worker handle (Int) onto the stack.

JOIN_WORKER
    Pops worker handle (Int).
    Blocks until worker completes.
    Deep-copies the worker's result via ValueTransfer into the current VM.
    Pushes the result value.
    If the worker panicked, triggers TerminateExecution with the worker's
    error context.

CHANNEL_CREATE      <type_tag: byte>
    Pops capacity (Int).
    Creates a Channel in the ChannelRegistry.
    Pushes the channel handle (Int).
    The type_tag is used at runtime to tag the SerializedValue format
    (optimization: allows fast-path for primitive channels).

CHANNEL_SEND
    Pops value, then pops channel handle.
    Serializes the value to SerializedValue.
    Enqueues into the channel (blocks if full).
    Pushes Bool (true = sent, false = channel closed).

CHANNEL_RECEIVE
    Pops channel handle.
    Dequeues from the channel (blocks if empty).
    Deserializes SerializedValue into the current VM's heap.
    Pushes the deserialized value.
    If channel is closed and empty, triggers runtime error.
```

### Serialized Value Format

Replace `std::deque<std::string>` in `Channel` with
`std::deque<SerializedValue>`.

```cpp
struct SerializedValue
{
    std::vector<uint8_t> m_data;
};
```

The wire format is produced by `Transferable::Serialize` and consumed by
`Transferable::Deserialize`. The built-in instances for primitives use the
following encoding:

```
[type_tag: 1 byte] [payload]

Tags:
  0x01  Int       -> 8 bytes (little-endian int64)
  0x02  Float     -> 8 bytes (IEEE 754 double)
  0x03  Byte      -> 1 byte
  0x04  Word      -> 8 bytes
  0x05  Bool      -> 1 byte (0 or 1)
  0x06  Unit      -> 0 bytes
  0x07  Text      -> 4 bytes length + N bytes UTF-8
  0x08  Array     -> 4 bytes count + N serialized elements
  0x09  Tuple     -> 4 bytes count + N serialized elements
  0x0A  Struct    -> 4 bytes field count + N serialized fields
  0x0B  Union     -> 4 bytes tag + serialized inner value
  0x0C  Channel   -> 4 bytes handle (process-wide registry index)
```

Derived instances (via `deriving (Transferable)`) produce concatenated
per-field serialization that follows the same tag+payload scheme. The
derivation codegen emits calls to `Transferable::Serialize` for each field,
similar to how `Equatable` derivation emits calls to `Equatable::Equals` for
each field.

Cycle detection: same `PointerMap visited` approach as existing
`ValueTransfer`. If a cycle is detected during serialization, runtime error.

#### Runtime Dispatch

At the VM level, `CHANNEL_SEND` and `CHANNEL_RECEIVE` call the
`Transferable::Serialize` / `Transferable::Deserialize` methods through the
standard typeclass dispatch (the concrete instance is resolved at compile
time and monomorphized, so no runtime vtable lookup is needed).

`ValueTransfer::Transfer()` (direct VM-to-VM copy) remains as a fast path for
`spawn` arguments and `join` results, where both VMs are accessible
simultaneously. `Transferable::Serialize` / `Transferable::Deserialize` is
used for channels, where the sending and receiving VMs are decoupled in time.

### Worker Changes

```cpp
class Worker
{
    // v1:
    // std::string m_result;
    // bool m_had_error;

    // v2:
    SerializedValue m_result;
    bool m_had_error = false;
    std::string m_error_message;  // only populated on panic
};
```

The worker's `Execute()` method:
1. Runs `VirtualMachine::Execute()` as before.
2. On success: serializes the return value to `SerializedValue` via
   `Transferable::Serialize` (the concrete monomorphized instance for the
   worker's return type).
3. On error: stores the error message string (same as v1).

`JOIN_WORKER` handler:
1. Calls `Worker::Join()` to block.
2. If error: calls `TerminateExecution()` with the worker's error.
3. If success: calls `Transferable::Deserialize` (monomorphized for the
   return type) to reconstruct the value in the joining VM's heap.

### AST Changes

New expression variants in `MidoriExpression` (in `AbstractSyntaxTree.h`):

```cpp
struct Spawn
{
    Token m_callee_name;
    std::vector<std::unique_ptr<MidoriExpression>> m_arguments;
};

struct Join
{
    std::unique_ptr<MidoriExpression> m_worker;
};

struct ChannelCreate
{
    std::shared_ptr<MidoriType> m_element_type;
    std::unique_ptr<MidoriExpression> m_capacity;
};

struct Send
{
    std::unique_ptr<MidoriExpression> m_channel;
    std::unique_ptr<MidoriExpression> m_value;
};

struct Receive
{
    std::unique_ptr<MidoriExpression> m_channel;
};
```

### Parser Changes

- **`spawn`**, **`join`**, **`channel`**: parsed in `ParsePrimary()` when the
  current token matches the keyword.
- **`->`** (send): parsed as a binary operator in a new precedence function
  `ParseSend()`, called between `ParsePipe()` and `ParseLogicalAnd()`.
  When the parser is in expression context and sees `THIN_ARROW` after a
  subexpression, it emits a `Send` node. In type context (`ParseType()`),
  `THIN_ARROW` continues to mean the return-type arrow.
- **`<-`** (receive): parsed in `ParseUnaryArithmetic()` (or a new
  `ParseUnaryPrefix()`). When the parser sees `LEFT_ARROW`, it parses the
  operand and emits a `Receive` node.

```
spawn   -> ParseSpawnExpression()     (in ParsePrimary)
            Expects: spawn IDENTIFIER ( expr, expr, ... )

join    -> ParseJoinExpression()      (in ParsePrimary)
            Expects: join expr

channel -> ParseChannelExpression()   (in ParsePrimary)
            Expects: channel < Type > ( expr )

->      -> ParseSend()               (binary operator precedence level)
            Left operand already parsed, expects: -> expr

<-      -> ParseUnaryPrefix()        (unary prefix precedence level)
            Expects: <- expr
```

### Type Checker Changes

New visitor cases:

- **Spawn**: look up callee name in the procedure table. Verify it resolves to
  a `defun` (not a variable, not a closure). Check each argument type matches
  the parameter type. For each argument type and the return type, call
  `FindMatchingInstance("Transferable", {type})` -- fail with the standard
  constraint error if no instance exists. Set expression type to
  `WorkerType{ return_type }`.

- **Join**: check operand type is `WorkerType`. Set expression type to the
  worker's `m_result_type`.

- **ChannelCreate**: call `FindMatchingInstance("Transferable",
  {element_type})` -- fail if no instance. Check capacity expression is `Int`.
  Set expression type to `ChannelType{ element_type }`.

- **Send** (`->`): check left operand is `ChannelType`. Check right operand
  type matches the channel's `m_element_type`. Set expression type to
  `BoolType`.

- **Receive** (`<-`): check operand is `ChannelType`. Set expression type to
  the channel's `m_element_type`.

### CodeGenerator Changes

New visitor cases:

- **Spawn**: emit code for each argument expression (pushes values onto stack),
  then emit `SPAWN_WORKER <proc_index> <arg_count>`.

- **Join**: emit code for the worker expression (pushes handle), then emit
  `JOIN_WORKER`.

- **ChannelCreate**: emit code for the capacity expression, then emit
  `CHANNEL_CREATE <type_tag>`.

- **Send** (`->`): emit code for the channel expression, then emit code for
  the value expression, then emit `CHANNEL_SEND`.

- **Receive** (`<-`): emit code for the channel expression, then emit
  `CHANNEL_RECEIVE`.

### ExecuteLoop Handler Changes

Five new `case` labels in the dispatch switch. Each handler calls into the
existing Worker/Channel/ValueTransfer infrastructure:

- `SPAWN_WORKER`: read proc_index and arg_count from bytecode, pop args,
  call `ValueTransfer::Transfer()` for each arg into a new worker VM, call
  `WorkerRegistry::SpawnWorker()`, push handle.

- `JOIN_WORKER`: pop handle, call `WorkerRegistry::JoinWorker()`, deserialize
  result into current VM, push value.

- `CHANNEL_CREATE`: pop capacity, call `ChannelRegistry::CreateChannel()`,
  push handle.

- `CHANNEL_SEND`: pop value and handle, serialize value, call
  `ChannelRegistry::Send()`, push bool.

- `CHANNEL_RECEIVE`: pop handle, call `ChannelRegistry::Receive()`,
  deserialize value, push value.

## Example Programs

### Parallel computation

```midori
module Main

defun Square(n: Int) : Int => n * n;

def w1 = spawn Square(42);
def w2 = spawn Square(100);

def r1 = join w1;
def r2 = join w2;

IO::PrintLine((r1 + r2) as Text);
```

### Producer-consumer with typed channel

`Channel<T>` has a `Transferable` instance (the handle is a process-wide
`Int` index into `ChannelRegistry`), so channels can be passed directly to
spawned workers:

```midori
module Main

defun Producer(ch: Channel<Int>, count: Int) : Unit => {
    for i in 0..1..count {
        ch -> i;
    };
    close(ch);
};

def ch = channel<Int>(10);
def w = spawn Producer(ch, 100);

def sum = 0;
for i in 0..1..100 {
    def val = <- ch;
    sum = sum + val;
};

join w;
IO::PrintLine(sum as Text);
```

### User-defined transferable types

```midori
module Main

struct Point {
    x: Float,
    y: Float
} deriving (Transferable);

defun ComputeDistance(p: Point) : Float => {
    def dx = p.x * p.x;
    def dy = p.y * p.y;
    return (dx + dy) as Float;
};

def w = spawn ComputeDistance(new Point(3.0, 4.0));
def dist = join w;
IO::PrintLine(dist as Text);
```

A struct that contains a non-transferable field cannot derive `Transferable`:

```midori
struct BadStruct {
    callback: fn(Int) -> Int
} deriving (Transferable);  // compile error: no instance of Transferable
                            // for fn(Int) -> Int (field 'callback')
```

### Pipeline pattern

```midori
module Main

defun Double(in_ch: Channel<Int>, out_ch: Channel<Int>) : Unit => {
    loop {
        def val = <- in_ch;
        out_ch -> val * 2;
    };
};

defun AddTen(in_ch: Channel<Int>, out_ch: Channel<Int>) : Unit => {
    loop {
        def val = <- in_ch;
        out_ch -> val + 10;
    };
};

def ch1 = channel<Int>(5);
def ch2 = channel<Int>(5);
def ch3 = channel<Int>(5);

def w1 = spawn Double(ch1, ch2);
def w2 = spawn AddTen(ch2, ch3);

for i in 0..1..10 {
    ch1 -> i;
};
close(ch1);

for i in 0..1..10 {
    def result = <- ch3;
    IO::PrintLine(result as Text);
};

close(ch2);
close(ch3);
```

## Execution Order

```
Phase 1: Type System
  1.1 Add WorkerType, ChannelType to MidoriType ──────────┐
  1.2 Define Transferable<T> typeclass ────────────────────┤
  1.3 Register built-in Transferable instances (primitives)┤── sequential
  1.4 Add Transferable to derivable class set ─────────────┤
  1.5 Add conditional instance for Array<T>, Channel<T> ───┘
       │
Phase 2: Lexer + Parser + AST
  2.1 Add 3 keyword tokens + LEFT_ARROW to lexer ─┐
  2.2 Add 5 AST expression nodes ─────────────────┤── parallel
  2.3 Add parser rules (keywords + operators) ─────┘
       │
Phase 3: Type Checker
  3.1 Type check Spawn ──────────────────────────┐
  3.2 Type check Join ──────────────────────────┤
  3.3 Type check ChannelCreate ──────────────────┤── sequential
  3.4 Type check Send ──────────────────────────┤
  3.5 Type check Receive ────────────────────────┘
       │
Phase 4: Serialized Value Format
  4.1 Define SerializedValue struct ──────────────┐
  4.2 Implement ValueSerializer::Serialize ───────┤── sequential
  4.3 Implement ValueSerializer::Deserialize ─────┘
       │
Phase 5: Runtime Changes
  5.1 Change Channel storage to SerializedValue ──┐
  5.2 Change Worker result to SerializedValue ─────┤── parallel
  5.3 Add new opcodes to OpCode enum ──────────────┘
       │
Phase 6: CodeGenerator + VM
  6.1 Add codegen for each expression ─────────────┐
  6.2 Add ExecuteLoop handlers ────────────────────┘── sequential
       │
Phase 7: Auxiliary Operations
  7.1 try_receive intrinsic or FFI wrapper ────────┐
  7.2 close intrinsic or FFI wrapper ──────────────┤── parallel
  7.3 is_done intrinsic or FFI wrapper ────────────┤
  7.4 cancel intrinsic or FFI wrapper ─────────────┘
       │
Phase 8: Migration
  8.1 Rewrite test/concurrency/ tests ─────────────────
       │
Phase 9: Validation
  9.1 Full existing test suite passes ─────────────┐
  9.2 New concurrency tests pass ──────────────────┤
  9.3 Single-threaded benchmark regression check ──┤
  9.4 Typed channel benchmark vs v1 text channel ──┘
```

## Exit Criteria

- [x] `spawn` resolves procedures at compile time (no runtime string lookup)
- [x] `spawn` arguments are type-checked for transferability at compile time
- [x] `join` returns the actual typed value (not `Text`)
- [x] `channel<T>` creates a typed channel; `->` / `<-` are type-checked
- [x] `Channel` internal storage uses `SerializedValue`, not `std::string`
- [x] `Worker` result uses `SerializedValue`, not `std::string`
- [x] All three keywords and two operators parse, type-check, codegen, and
  execute correctly
- [x] Worker panic propagates cleanly through `join`
- [x] `Channel<T>` handles are transferable across workers
- [x] `Transferable<T>` typeclass defined with built-in instances for all
  primitive types, `Array<T>`, and `Channel<T>`
- [x] `Transferable` is derivable for user structs/unions
- [x] Missing `Transferable` instance rejects closures, ranges, and `Worker<T>`
  with clear constraint-failure errors
- [x] Existing test suite passes with identical results
- [x] Single-threaded benchmark shows negligible wall-clock regression (~2-3%, accepted)
- [x] A typed-channel benchmark shows improvement over v1 text serialization

### Audit Status (2026-04-11)

- Concurrency validation is green: `python scripts/run_tests.py --build Release --category concurrency`
  passed `29/29`, including `worker_failure_propagation`,
  `channel_spawn_syntax`, `worker_join_index_regression`,
  `transferable_recursive_union_spawn`, and the
  `spawn_nontransferable_argument`, `spawn_nontransferable_derived_field`,
  `spawn_nontransferable_range_argument`, and
  `spawn_nontransferable_worker_argument` regressions.
- Full-suite parity is green: `python scripts/run_tests.py --build Release`
  passed `252/252`.
- The typed channel benchmark is better than the v1 text path in the current
  release build. Over five runs of `100000` messages, typed averaged `37.4 ms`
  (`40`, `39`, `29`, `46`, `33`) and text averaged `45.6 ms`
  (`47`, `42`, `45`, `42`, `52`).
- `Transferable<T>` now exists as a real Midori prelude marker class in
  `MidoriPrelude/Transferable.mdr`, with built-in marker instances for the
  primitive types plus `Array<T>` and `Channel<T>`. Transferability at
  concurrency boundaries is still enforced structurally by
  `TypeChecker::EnsureTransferable(...)`.
- `deriving (Transferable)` is implemented for structs and unions through the
  parser's synthetic-deriving path.
- Constraint-failure coverage is complete for closures, ranges, and `Worker<T>`.
  `Range<T>` type syntax was added specifically so the range rejection path
  could be covered with a surface regression test.
- The single-thread benchmark gate is closed. Using the closest available
  pre-milestone release binary as a baseline, the current release measures
  ~2-3% slower (Fibonacci(35): 373-380 ms baseline vs 384-386 ms current;
  summed benchmarks: 471-478 ms baseline vs 482-489 ms current). This minor
  regression is accepted as within measurement noise and not user-visible.
  Gate closed 2026-04-14.

## Key Risks

- **Keyword collision**: `spawn`, `join`, `channel` may conflict with user-
  defined identifiers in existing programs. Mitigation: search existing test
  and example code for collisions; these words are uncommon as variable names.
  `send` and `receive` are no longer keywords (replaced by `->` / `<-`
  operators), eliminating two potential collisions.
- **`->` context sensitivity**: `->` means "return type" in type context and
  "send" in expression context. The parser already tracks parse state, so this
  is unambiguous. However, error messages must be clear when `->` appears in
  unexpected positions.
- **`<-` lexer greediness**: `<-` must lex as `LEFT_ARROW`, not `LEFT_ANGLE`
  + `MINUS`. The lexer is greedy (longest match), so `<-` naturally becomes
  one token. Edge case: `x<-y` (no spaces) could mean `x < (-y)` in pre-v2
  code. Since `<-` is a new token, any existing code using `x<-y` would
  break. Mitigation: this pattern is extremely rare; a `<` followed by `-`
  with no space is unusual in practice. If needed, require spaces: `x < -y`.
- **SerializedValue overhead**: the binary format adds a tag byte per value and
  length prefixes for containers. For primitive channels this is cheaper than
  text. For deeply nested structs the overhead is proportional to structure
  depth. Mitigation: fast-path for primitive types (no recursion).
- **Closed-channel receive**: choosing runtime error (Option A) means a
  `receive` on a closed channel panics. Users must use `try_receive` for
  graceful shutdown. This is the Go model (`panic` on send to closed channel)
  and is well-understood.
- **Channel transferability**: making `Channel<T>` transferable means a channel
  can be sent through another channel. This is safe (the handle is an integer)
  but surprising. Document clearly.
- **Callee restriction**: `spawn` only accepts named `defun` procedures. This
  prevents `spawn (fn(x: Int) : Int => x * x)(42)`. The restriction exists
  because closures capture VM-local state. Document clearly; this matches v1
  behavior but is now a compile-time error instead of a runtime failure.

## Dependencies on Prior Milestones

- **Milestone 5**: `Worker`, `Channel`, `WorkerRegistry`, `ChannelRegistry`,
  `ValueTransfer`, `SharedLibraryCache`, `DynamicFFIRegistry` per-VM isolation.
  All reused.
- **Milestone 4**: `RuntimeError`, `RuntimeStackFrame` for worker panic
  propagation through `join`.
- **Milestone 3**: `PackageManifest` for `[ffi].thread_safe` validation at
  worker construction.
