# Midori Roadmap

## Goal

Make Midori feel credible for medium-sized real programs, not just language demos.

By the end of this roadmap, Midori should offer:

- a trustworthy documented language surface
- a usable day-to-day toolchain
- reproducible dependency management
- release-grade diagnostics and debugging
- a standard library that covers common application work
- targeted language polish driven by real usage

## Current Position

Midori already has a strong core:

- static typing with inference
- structs, unions, pattern matching, generics, and typeclasses
- modules and project initialization
- a VM runtime, optimizer passes, and regression tests
- FFI and package loading

What still makes it feel early is the platform around the language:

- packaging is still mostly manual
- tooling is still narrow
- docs and implementation can drift
- runtime diagnostics are not yet strong enough in normal builds
- the standard library is still intentionally small

## Ordering Principles

- Finish the platform before chasing advanced type theory.
- Treat docs, tests, and tooling as part of the language, not side work.
- Every milestone should end in something users can rely on immediately.
- Prefer removing ambiguity over adding more surface area.
- Do not add complex features that the package system and tooling cannot support well.

## Milestone 1: Make The Language Surface Trustworthy

### Why First

Before Midori can feel real, users need to know that the docs, parser, typechecker, and runtime all describe the same language.

### Scope

- Audit every user-facing feature claim in `README.md` and `docs/`.
- Remove or mark any stale claims that are not implemented.
- Add a single source of truth for supported language features.
- Define a clear experimental/stable distinction for features.
- Tighten documentation tests so documented examples are continuously verified.

### Concrete Deliverables

- A feature matrix document that maps syntax and capabilities to implementation status.
- Removal or correction of stale documentation, especially around features that appear in docs but not in the current AST/token/runtime surface.
- A documented versioning policy for language changes and breaking changes.
- A regression suite that compiles all documented examples as part of normal testing.

### Exit Criteria

- Every feature advertised in user-facing docs is either implemented, tested, or explicitly marked experimental.
- No documented syntax exists without parser coverage.
- No implemented stable feature lacks at least one regression or unit test.
- A new user can answer "is this feature real?" without reading C++ source.

## Milestone 2: Ship A Real Developer Workflow

### Why Second

A language stops feeling toy-like when normal development stops being awkward.

### Scope

- Expand the CLI beyond the current run/check/init baseline.
- Add a formatter with a stable style.
- Improve machine-readable diagnostics for editor integration.
- Make project-aware commands the default workflow.
- Add first-party editor support at the syntax and diagnostics level.

### Concrete Deliverables

- CLI commands for `run`, `check`, `fmt`, `test`, and project/package management.
- Stable JSON diagnostics suitable for editor integration.
- A first-party formatter with an idempotent style.
- Basic editor support covering syntax highlighting, project-aware diagnostics, and command examples for common workflows.
- Better help output and command discoverability.

### Exit Criteria

- A new user can initialize, format, check, run, and test a project without learning the internal CMake/test harness structure first.
- Editor integration can show errors and warnings from a stable machine-readable interface.
- Formatting stops being a source of style drift in examples and packages.

## Milestone 3: Turn Packages Into A Real Dependency System

### Why Third

A language ecosystem does not become real until users can share code reproducibly.

### Scope

- Add dependency resolution and version constraints.
- Add a lockfile for reproducible builds.
- Add package install/update/remove flows.
- Support package publishing and validation.
- Improve native package ergonomics and binary verification.

### Concrete Deliverables

- Manifest support for direct dependencies and version constraints.
- A lockfile checked into projects.
- A package cache and install location strategy.
- CLI support for package install and update.
- Registry or index support, even if it starts as a simple first-party registry.
- Checksum verification for downloaded prebuilt binaries.
- Automatic native build support where prebuilt artifacts are unavailable.

### Exit Criteria

- A project can declare dependencies and reproduce the same dependency graph on a clean machine.
- Package installation no longer depends on users manually wiring directories into `MIDORI_PATH`.
- Native FFI-backed packages have a standard install story instead of handwritten setup steps.

## Milestone 4: Harden Runtime, Debugging, And Release Diagnostics

### Why Fourth

Users forgive an early ecosystem more easily than a runtime they cannot debug.

### Scope

- Improve stack traces and runtime diagnostics in non-debug builds.
- Stabilize panic and fatal error presentation.
- Add runtime observability for GC and performance.
- Make the FFI boundary safer and easier to reason about.

### Concrete Deliverables

- Source-aware stack traces available in development and release-quality workflows.
- Better runtime error messages with module and line information when available.
- GC and runtime profiling hooks that are usable without patching the runtime.
- A documented and versioned FFI ABI policy.

### Exit Criteria

- A runtime crash or panic produces actionable output for normal users, not only for compiler developers.
- FFI package authors have a stable contract to target.

## Milestone 5: Add Multicore Runtime Capabilities ✓

Completed. Delivered in two phases:

Isolated-worker runtime with built-in concurrency primitives: `spawn`, `join`,
`channel` keywords, `->` / `<-` operators, `Worker<T>` and `Channel<T>` types,
`Transferable<T>` typeclass with `deriving` support, `SerializedValue` binary
format, and dedicated VM opcodes. Underlying infrastructure — `Worker`,
`Channel`, `WorkerRegistry`, `ChannelRegistry`, `ValueTransfer`,
`SharedLibraryCache`, per-VM `DynamicFFIRegistry` isolation, FFI thread-safety
validation — lives outside `ExecuteLoop()`; single-threaded regression ~2-3%
(accepted).

A correctness follow-up (2026-07-15) closed three defects the first pass left
behind: `cancel(w)` did not stop a running worker, so `join` and process exit
could hang indefinitely; blocking channel operations were uninterruptible; and
`ChannelRegistry` never reclaimed a channel. Cancellation now uses safepoints
inside `ExecuteLoop()` (loop back-edges, tail calls, foreign-call returns),
gated on a flag that is false for the main VM, so the overhead guarantee holds
as one never-taken branch rather than as an untouched loop. Worker error codes
now survive the join boundary and builtin sleeps are cancellable. All 257 tests
pass; 31/31 concurrency tests pass. Remaining concurrency work is recorded with
triggers in `docs/plan/concurrency-backlog.md`.

### Exit Criteria (all met)

- A Midori program can scale across multiple CPU cores for at least one representative class of workloads.
- The concurrency model is explicit and documented, rather than implied by stale syntax or runtime assumptions.
- FFI interactions under multicore execution have a documented safety model.
- Users can reach multicore parallelism without writing native code.
- Concurrency expressions (`spawn`, `join`, `channel`, `->`, `<-`) are type-checked at compile time.
- Value transfer uses typed `Transferable<T>` constraints instead of text serialization.

## Milestone 6: Expand The Standard Library Where Real Apps Need It

### Why Sixth

Right now Midori has a respectable core prelude, but many nontrivial applications will still drop into FFI too quickly.

### Scope

- Add the missing "ordinary application" building blocks.
- Keep the core library small but practical.
- Prefer typed wrappers and predictable behavior over thin raw bindings.

### Priority Areas

- bytes and binary buffers
- path and filesystem helpers beyond raw file calls
- richer process and environment helpers
- serialization and configuration support such as JSON and TOML
- more collection and algorithm utilities
- better text processing primitives
- time, randomness, and data conversion helpers where current wrappers are too thin

### Concrete Deliverables

- A `Bytes`-style module or equivalent binary-friendly abstraction.
- Structured config/data parsing for common formats.
- Path APIs that make cross-platform file work less error-prone.
- Stronger collection and iterator-style utilities for everyday code.
- Examples that build real CLI/data-processing programs without custom FFI.

### Exit Criteria

- A reasonable CLI app, config-driven tool, or offline renderer can be written mostly in Midori plus the standard library.
- The first recommendation for common app tasks is no longer "write an FFI package."

## Milestone 7: Language Polish And High-Value Language Features

### Why Last

The language core is already strong enough to write useful programs. The remaining language work should be driven by real package and application needs, not by feature envy.

### Scope

- Improve diagnostics and ergonomics in the current type system.
- Finish incomplete semantic checks.
- Broaden deriving where it removes real boilerplate.
- Add only the advanced type-system features that have demonstrated demand.

### Concrete Deliverables

- Better unification and type error messages.
- Deeper nested-pattern exhaustiveness checking.
- Broader deriving support where the generated behavior is predictable.
- Default method implementations for typeclasses.
- A decision document on advanced type-system work such as higher-kinded types, GADTs, existentials, and rank-N polymorphism.

### Exit Criteria

- The common complaint shifts from "I cannot build this cleanly" to "I want more expressive abstractions."
- Advanced type-system work is prioritized by actual ecosystem pressure, not by aesthetics.

## Recommended 1.0 Gate

Midori should not call itself "1.0" until all of the following are true:

- the documented language surface matches the implementation
- formatting, checking, testing, and project initialization are first-party workflows
- dependency installation and locking are reproducible
- runtime diagnostics are usable in ordinary builds
- a supported multicore execution model exists for CPU-bound work
- the standard library covers normal application tasks without immediate FFI escape hatches
- at least a small set of external packages can be installed and used by third parties

## What Not To Prioritize Yet

- higher-kinded types before package management
- GADTs before runtime diagnostics
- a broad web framework before a stable dependency workflow
- syntax expansion that makes docs and tooling harder to keep aligned
- an elaborate async story without a clear runtime design

## Suggested Execution Sequence

1. Milestone 1 and Milestone 2 should begin immediately and overlap.
2. Milestone 3 should start once CLI and diagnostics are stable enough to support package workflows.
3. Milestone 4 should run in parallel with the latter half of Milestone 3.
4. Milestone 5 should begin only after runtime diagnostics and the FFI contract are stable enough to support parallel execution safely.
5. Milestone 6 should be guided by real package, multicore, and sample application pain points.
6. Milestone 7 should be selective and usage-driven, not speculative.

## Immediate Next Steps

If work starts now, the highest-value next actions are:

1. Create the feature audit and remove doc drift, especially around features currently mentioned in docs but absent from the token/AST/runtime surface.
2. Add first-party `fmt` and richer CLI workflows so normal development is not tied to repo internals.
3. Design package dependency resolution and a lockfile before adding more example packages.
4. Promote usable stack traces and runtime diagnostics out of debug-only workflows.
5. Draft the multicore runtime model around isolated workers or multi-VM execution before committing to any shared-heap threading design.
6. Pick two or three representative real applications and use them to drive both multicore APIs and standard-library expansion.
