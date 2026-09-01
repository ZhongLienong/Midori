# Milestone 3 Status: Turn Packages Into A Real Dependency System

## Status

Milestone 3 is core complete. Midori now has local dependency resolution,
version constraints, lockfiles, vendoring into `packages/`, package CLI
commands, and checksum verification for prebuilt native libraries.

Milestone 4 can start from here. The remaining Milestone 3 work is follow-up
scope, not a blocker for runtime hardening and release diagnostics.

## Outcome

Shipped:

- projects and packages can declare direct dependencies with version constraints
- `midori install` resolves transitive dependencies, vendors exact versions, and
  writes `midori.lock`
- package search paths are derived from the resolved graph rather than requiring
  users to manually wire package directories into `MIDORI_PATH`
- prebuilt native libraries can be checksum-verified before load
- `midori list` prints the resolved dependency tree

Still deferred:

- registry-backed package fetching and publishing
- automatic native builds when no prebuilt binary exists
- package-scoped update behavior; `midori update [package]` still refreshes the
  full graph
- full cleanup of every newly orphaned transitive vendored package directory

## Implemented Scope

### Phase 1: Version Constraint System

- `SemanticVersion` parses and compares semantic versions, including prerelease
  and build metadata
- `VersionConstraint` supports caret, tilde, equality, inequality, and
  comma-separated compound ranges
- `PackageManifest` validates package versions, dependency constraints, and
  `midori_version`
- `ProjectManifest` validates direct dependency constraints in the active root
  manifest

### Phase 2: Package Resolution

- `PackageIndex` scans local package roots for `package.midori`
- multiple versions of the same package can coexist
- the resolver selects the highest compatible version
- transitive dependencies are resolved recursively
- version conflicts and dependency cycles produce explicit errors
- resolved package directories are fed into the effective project search path

### Phase 3: Lockfile

- `midori.lock` is written in TOML at the project root
- the lockfile records Midori version, generation timestamp, manifest checksum,
  resolved package versions, package sources, package checksums, and dependency
  edges
- package environment setup prefers the lockfile when the root manifest checksum
  still matches and locked package directories are available
- stale manifests or missing locked packages force a fresh resolve and rewrite

### Phase 4: Checksums

- SHA-256 hashing utilities were added
- selected prebuilt native libraries are checksum-verified before load
- lockfile entries record per-package checksums derived from `package.midori`
  and `.mdr` sources
- lockfile package checksum drift emits warnings

### Phase 5: CLI Package Commands

- `midori install`
- `midori install <package> [--version <constraint>]`
- `midori update [package]`
- `midori remove <package>`
- `midori list`

The implemented workflow is local-first. `midori install <package>` can infer a
default constraint from the highest locally available version, but it does not
fetch from a remote registry.

### Phase 6: Registry

Not started in code:

- registry index format and fetch layer
- local filesystem registry abstraction
- HTTP registry support
- package publishing

### Phase 7: Native Package Build Support

Not started in code:

- automatic CMake or Cargo build fallback
- build artifact caching keyed by native source state
- install-time native prebuild generation

## Exit Criteria Reconciliation

| Criterion | Status | Notes |
| --- | --- | --- |
| A project can declare dependencies with version constraints in `project.midori` or `package.midori` | Done | Implemented and covered by unit tests. |
| `midori install` resolves dependencies, populates the packages directory, and writes `midori.lock` | Done | Local package roots only. |
| `midori.lock` reproduces the same dependency graph on a clean machine | Partial | Works when vendored `packages/` content or equivalent local package roots are available. Remote rehydration is deferred. |
| Transitive dependencies are resolved automatically | Done | Resolver walks dependency manifests recursively. |
| Version conflicts produce clear error messages | Done | Resolver reports direct conflict details. |
| Package installation no longer depends on users manually editing `MIDORI_PATH` | Done | Project package environment now rebuilds the effective search path from the resolved graph. |
| Prebuilt binary checksums are verified before loading | Done | Enforced at dynamic library load time. |
| Native FFI packages have a standard build and install story | Partial | Prebuilt binaries have a standard local install story. Automatic native builds are still deferred. |
| At least one registry is functional | Deferred | No registry implementation yet. |
| `midori list` shows the full dependency tree with versions | Done | Uses lockfile when valid, otherwise resolves. |

## Milestone Assessment

Milestone 3 is ready to hand off to Milestone 4 as long as the remaining
package-system follow-ups stay visible:

- registry and publish support
- automatic native build fallback
- a true package-scoped update operation
- stronger vendored package cleanup

That leaves the package system in a usable local-first state without pretending
the registry and native-build pieces already exist.
