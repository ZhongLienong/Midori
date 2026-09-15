# Concurrency Backlog

Status as of 2026-07-15. The correctness work is done and merged into `gc-overhaul`
(commits `7a0215b..994e3fa`): worker cancellation actually stops workers, blocked
channel operations are interruptible, the channel registry no longer leaks, error
codes survive the join boundary, and builtin sleeps are cancellable.

Everything below is **expressiveness or scale**, not correctness. Nothing here is
high-priority, and the roadmap already says why: Milestone 6 should be "guided by
real package, multicore, and sample application pain points", Milestone 7 should be
"selective and usage-driven, not speculative", and "an elaborate async story without
a clear runtime design" is listed as an explicit anti-goal.

Each item below therefore records a **trigger** — the observation that should promote
it from backlog to plan. Building any of them before its trigger fires is speculative.

## 1. `try_receive`, `select`, timeouts — needs design, not a plan

**Gap.** `<- ch` blocks until a value arrives or the channel closes. There is no
bounded wait and no way to wait on two channels at once.

**Evidence it is half-built.** `Channel::TryReceive` and `ChannelRegistry::TryReceive`
are fully implemented, but there is no opcode, no syntax, and no type-checker support —
`grep -rn "TRY_RECEIVE\|try_receive" src/` returns nothing. The runtime half is
unreachable dead code today.

**Why it is not urgent.** Fan-in already works with one shared channel and multiple
producers, which covers the common worker-pool shape. The real gap is bounded waiting.

**Why it needs brainstorming first.** `select` is a syntax and semantics decision
(statement or expression? default branch? timeout as a channel or a clause?) that
shapes the grammar, type checker, and opcode set. It should start from a design
conversation, not from an implementation plan written in isolation.

**Trigger.** A real program that must wait on two channels, or must give up waiting.

## 2. Worker spawn cost

**Partly resolved 2026-09-14.** Workers no longer re-run module initializers:
`spawn` copies the spawning VM's globals into the new worker instead (the "shape
of the fix" below). This also fixed a correctness bug the old scheme had, where
data globals read as zero in workers and pointer-valued ones crashed them.

**Remaining gap.** Every `spawn` still constructs a fresh `VirtualMachine`, and now
serializes every global on each spawn, so a program holding a large global array
copies it per worker. Each worker also allocates its own value and call stacks
(10000 slots each, per `s_value_stack_size` / `s_call_stack_size`), allocator, and
GC.

**Why it is not urgent.** It is invisible at the handful-of-workers scale current
programs use, and it is a constant per spawn, not a leak.

**Shape of the fix.** Initialize once and clone the resulting global state into new
workers, rather than re-executing initializers per spawn. Contained to `Worker.cpp`
plus whatever snapshot primitive the VM needs.

**Trigger.** A benchmark showing spawn latency dominating, or a program spawning
workers in a loop.

## 3. Thread-per-worker scheduler

**Gap.** One `spawn` is one OS thread. This is fine at core count and bad at thousands.

**Why it is not urgent, and why it is the riskiest item.** It is the largest change
here, it would subsume item 2, and the roadmap explicitly warns against committing to
a threading design before drafting the multicore runtime model. Doing this first would
be building the expensive thing before knowing which shape is needed.

**Trigger.** Written multicore runtime model first (roadmap "Immediate Next Steps" #5),
plus benchmark evidence that thread count is the limit. Not before both.

## 4. WASM concurrency

**Gap.** Concurrency opcodes are a hard runtime error under `MIDORI_WASM`
("Concurrency is not supported in the WebAssembly build").

**Scope note.** The website ships `public/midori.wasm`, so this is the web playground's
gap, not just a theoretical one — but no `.mdr` example on the site currently uses
`spawn` or `channel`, so nothing is visibly broken today.

**Trigger.** Wanting the playground to demo concurrency. Then decide between emscripten
pthreads and a cooperative single-threaded fallback.

## 5. Residual uncancellable blocking (lowest)

Stdin reads (`IO.cpp`) and third-party dynamic FFI still run to completion before a
worker observes cancellation. Builtin `Sleep` is already interruptible. The limitation
is documented at `src/Common/Cancellation/Cancellation.h`, which is where anyone adding
a blocking builtin will see it.

**Trigger.** A worker that blocks on stdin in practice.

## Recommended order if work resumes

1 (after a design conversation) → 2 → 4 (if the playground needs it) → 3 (only with a
runtime model and benchmarks) → 5.
