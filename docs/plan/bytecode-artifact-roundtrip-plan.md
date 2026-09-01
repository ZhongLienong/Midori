# Plan: bytecode round-trip in a single binary

## Goals
1. `MidoriExecutable` can be serialized to and deserialized from a stable binary format.
2. `midori build foo.mdr` produces a runnable `.mbc` binary artifact (the JSON disassembly stays available as opt-in).
3. `midori run foo.mbc` loads and executes that artifact, with output identical to `midori run foo.mdr`.
4. A version field on the artifact lets the VM reject mismatched bytecode cleanly.

## Non-goals (deferred to later)
- Splitting compiler and VM into separate binaries.
- Cross-platform/cross-architecture artifact portability beyond what falls out for free.
- Bytecode-level optimization or stripping.

---

## Phase 1 — Format design and writer

**1.1 Define the binary layout** in a new `src/Common/BytecodeArtifact/` (move out of `Utility/` since both compiler and VM will need it).

```
header:
  magic[4]         "MBC\0"
  format_version   u32   // bump on opcode/layout change
  midori_version[3] u16 each (major, minor, patch — informational)
  flags            u32   // bit 0: source_files_embedded
  payload_size     u64
  payload_crc32    u32

payload:
  file_name        len-prefixed string (u32 len)
  string_pool      u32 count, then (u32 len + bytes) per entry
  globals          u32 count, then (u32 len + bytes) per entry — MidoriText
  procedure_count  u32
  per procedure:
    name             len-prefixed string
    source_path      len-prefixed string
    bytecode_size    u32
    bytecode_bytes   raw u8 stream  // already endian-agnostic; constants emitted LSB-first
    line_info_count  u32
    line_info        (i32 line, i32 count) pairs
  source_files     (only if flag bit 0 set)
                   u32 count, then (path string + u32 line_count + line strings)
```

All multi-byte fields are little-endian on disk. Constants embedded in `bytecode_bytes` are already LSB-first (verified: `CodeGenerator::EmitNumericConstant` writes byte0=LSB), so the byte stream is portable as-is.

**1.2 Implement `WriteExecutable(const MidoriExecutable&, std::ostream&)`** using a small `BinaryWriter` helper (no dependencies, just `write` + endian fixups on big-endian hosts via `MIDORI_BIG_ENDIAN`). Reuse `MidoriChecksum::HashBytes` for the payload CRC (or switch to a real CRC32 — `HashBytes` looks like a content hash, double-check what it returns).

**1.3 Add an `MBC_FORMAT_VERSION` constant** in `Common/BuildConfig`. Bump in the same commit as any opcode change.

---

## Phase 2 — Reader and `MidoriExecutable` mutators

**2.1 Audit `MidoriExecutable` setters.** The class already exposes `AttachProcedures`, `AddStringPool`, `AttachProcedureNames`, `AttachProcedureSourcePaths`, `AttachSourceFiles`, `SetFileName`, `AddGlobalVariable` — all the deserializer needs. `BytecodeStream` has no `Append(bytes, line_info)` constructor; add a `static BytecodeStream FromRaw(std::vector<OpCode>&&, std::vector<std::pair<int,int>>&&)` so the loader can install both fields without replaying `AddByteCode` per instruction.

**2.2 Implement `ReadExecutable(std::istream&)`** returning `std::expected<MidoriExecutable, std::string>`. Validate magic, format_version, payload_crc, and per-procedure sizes. Reject early on any mismatch with a precise error.

**2.3 Round-trip test.** New test in `tests/`: take every `.mdr` in `test/`, compile, write to memory, read back, assert byte-equal serialization on the second pass. This catches asymmetric encoding bugs.

---

## Phase 3 — CLI integration

**3.1 `midori build`** — switch the default artifact to `<source>.mbc` (binary). Keep `--format json` to emit the existing `.mbc.json` disassembly (move that JSON code into a `Disassembler` if it isn't already; it overlaps with `Utility/Disassembler/`).

**3.2 `midori run`** — detect input by extension:
- `.mdr` → existing compile-and-run path (`MidoriDriver::CompileAndRunFile`).
- `.mbc` → new `MidoriDriver::LoadAndRunArtifact(path)` that calls `ReadExecutable` + `RunExecutable`.
- Anything else → unchanged error.

Behavior on a stale artifact (source mtime > artifact mtime) is **not** auto-rebuild for now — keep the user in control. Document explicitly.

**3.3 New CLI test cases** in whatever covers `CLI.cpp` today: build then run, mismatched version rejection, corrupted-payload rejection, missing-file error path.

---

## Phase 4 — Runtime error fidelity

**4.1 Source-file embedding flag.** Add `--embed-sources` to `midori build`. Default off (smaller artifacts; VM falls back to reading the path from `m_procedure_source_paths`). With the flag, the writer captures the linker's `m_global_source_files` table verbatim and the VM uses `FindSourceLines` as today.

**4.2 Verify runtime error rendering** still works when running from `.mbc` without sources on disk and without `--embed-sources` — it should degrade to "source not available" rather than crash. Add a test that deletes the `.mdr` and runs the `.mbc`.

---

## Phase 5 — Validation sweep

**5.1 Equivalence harness.** Extend the test runner: for each `.mdr` test, run twice — once direct, once via `build` → `run <.mbc>` — and assert identical exit code, stdout, stderr.

**5.2 Benchmark.** Compare cold-start time of `midori run hello.mdr` vs `midori run hello.mbc` on a representative sample. Confirms the load path is a real win, not just architectural.

**5.3 Update `docs/`** with the artifact format spec (the layout above) and a short note on versioning policy: any opcode change requires bumping `MBC_FORMAT_VERSION`.

---

## Risks and how to handle them

- **Opcode churn invalidates artifacts.** Expected. Bump `MBC_FORMAT_VERSION` per change; rejection message tells the user to rebuild. Don't try to support old versions.
- **Endian portability of the line_info struct.** Pairs are `int`, host-sized — write as `i32` explicitly. Same for any `size_t` you serialize: pin to `u32`/`u64`.
- **`MidoriText` long-form layout drift.** The serializer must walk via `GetCString()`/`GetByteLength()`, never `memcpy` the struct. Same for any traceable. Currently we only serialize names (text-as-bytes), so this is fine — add a comment in the serializer marking the boundary.
- **CRC vs hash.** If `HashBytes` is a non-CRC hash and bigger than 4 bytes, just store the full hash digest in the header instead of squeezing into `u32`. Verify before coding.

---

## Suggested commit slicing

1. New `Common/BytecodeArtifact` directory with format constants + `BinaryWriter`/`BinaryReader` primitives + unit tests.
2. `WriteExecutable` + golden test on a small fixture.
3. `BytecodeStream::FromRaw` + `ReadExecutable` + round-trip test on the test corpus.
4. CLI: `build` writes `.mbc`, `--format json` opt-in for disassembly.
5. CLI: `run` accepts `.mbc`.
6. `--embed-sources` flag + runtime error fidelity tests.
7. Equivalence harness + docs.

Each commit is independently reviewable and shippable; the language behavior doesn't change until commit 4.
