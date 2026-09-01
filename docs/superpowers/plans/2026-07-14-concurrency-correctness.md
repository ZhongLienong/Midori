# Concurrency Correctness Implementation Plan

> **Status: COMPLETE (2026-07-15).** All six tasks executed and merged into
> `gc-overhaul` as `7a0215b..4247a6e`, plus two follow-ups (`868c658`,
> `994e3fa`) that came out of review and use. Outcomes: cancellation is
> observed at loop back-edges, tail calls, foreign-call returns and blocking
> channel waits; worker error codes survive the join boundary; closed-and-drained
> channels are reclaimed; builtin `Sleep` is interruptible (a cancelled 30s sleep
> went from 30.5s to under 1s). Verified at 257/257 `.mdr` tests, 752/755 unit
> assertions (3 failures pre-date this work), and a clean WASM build.
>
> Two deviations from the plan as written, both review-driven and both kept:
> a `TAIL_CALL` safepoint was added because tail-recursive workers have no loop
> back-edge and were otherwise uncancellable, and `EraseIfDrained` was gated
> behind a lock-free closed check to keep the hot receive path off the global
> registry mutex. Remaining work is in `docs/plan/concurrency-backlog.md`.
>
> Kept as the record of what was done; the checkboxes below are historical.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make worker cancellation actually stop running workers, make blocking channel operations interruptible, and stop the global channel registry from leaking every channel ever created.

**Architecture:** Midori workers are OS threads each running an isolated `VirtualMachine`; they communicate through a global `ChannelRegistry` of blocking MPMC queues carrying `SerializedValue` deep copies. Today `Worker::Cancel()` only sets a `std::stop_token` that is checked once before execution starts, so a running or channel-blocked worker can never be cancelled, and `~Worker`/`JoinValue` can hang forever. This plan threads the worker's `std::stop_token` into the VM (polled at loop back-edges) and into channel waits (`std::condition_variable_any::wait(lock, stop_token, pred)`), surfaces cancellation as a new `WorkerCancelled` runtime error, and converts the channel registry to `shared_ptr` entries erased once a channel is closed and drained.

**Tech Stack:** C++23 (MSVC, Ninja/CMake presets), Catch2 unit tests in `tests/unit`, end-to-end `.mdr` tests in `test/concurrency/success` run via `python scripts/run_tests.py`.

---

## Background: current behavior (evaluation findings this plan fixes)

- `Worker::Execute` checks `stop_token.stop_requested()` once at thread start ([Worker.cpp:131](../../../src/Interpreter/Worker/Worker.cpp)); after that the VM never observes stop. `cancel(w)` returns `true` but does nothing.
- `Channel::Send`/`Receive` block on plain `std::condition_variable` with no stop support ([Channel.cpp:10-45](../../../src/Interpreter/Channel/Channel.cpp)); a worker blocked in `<- ch` is un-cancellable and `~Worker`/`JoinValue` then deadlock.
- `ChannelRegistry::m_channels` entries are never erased; `FindChannel` returns a raw `Channel*` whose safety silently depends on that leak.

Out of scope (candidates for separate plans): `try_receive`/`select`/timeouts at the language level, worker spawn cost (each spawn re-runs all module initializers in a fresh VM), a lightweight task scheduler instead of thread-per-worker, WASM concurrency support.

## Build & test commands (used throughout)

```powershell
# Configure once (if out/build/ninja/x64-development does not exist)
cmake --preset x64-development

# Build everything including tests
cmake --build out/build/ninja/x64-development

# Run unit tests (filter by tag)
Get-ChildItem -Recurse -Filter MidoriUnitTests.exe out\build\ninja\x64-development | Select-Object -First 1 -ExpandProperty FullName
# then e.g.:
out\build\ninja\x64-development\tests\MidoriUnitTests.exe "[channel]"

# Run end-to-end .mdr tests
python scripts/run_tests.py --category concurrency --build Development
```

## File structure

| File | Change |
|---|---|
| `src/Common/Error/Error.h` | Add `WorkerCancelled` to `RuntimeErrorCode` |
| `src/Common/Error/Error.cpp` | Name mapping + panic classification for `WorkerCancelled` |
| `src/Interpreter/Channel/Channel.h` | `ChannelOpStatus`, `ChannelReceiveResult`, stop-token-aware `Send`/`Receive`, `IsDrained`, `shared_ptr` registry, `GetChannelCount` |
| `src/Interpreter/Channel/Channel.cpp` | `condition_variable_any` waits, registry erase-on-drain |
| `src/Interpreter/VirtualMachine/VirtualMachine.h` | `m_stop_token`/`m_stop_possible` members, `SetStopToken` |
| `src/Interpreter/VirtualMachine/VirtualMachine.cpp` | JUMP_BACK safepoint, cancellation-aware channel opcodes |
| `src/Interpreter/Worker/Worker.cpp` | Pass the thread's stop token into the worker VM |
| `tests/unit/runtime/ChannelTests.cpp` | New: Channel + registry unit tests |
| `tests/unit/runtime/WorkerCancellationTests.cpp` | New: end-to-end cancel/join tests via `ExecuteSnippet` |
| `test/concurrency/success/worker_cancel_spin.mdr` (+ `.expected`) | New: cancel interrupts a compute loop |
| `test/concurrency/success/worker_cancel_blocked_receive.mdr` (+ `.expected`) | New: cancel interrupts a blocked receive |

Repo style (CLAUDE.md/AGENTS.md): no `auto`, `m_`/`s_` prefixes, PascalCase functions, always braces, implementations in `.cpp`, no comments for self-explanatory code.

---

### Task 1: `WorkerCancelled` runtime error code

**Files:**
- Modify: `src/Common/Error/Error.h:66-80`
- Modify: `src/Common/Error/Error.cpp:115-146` and `src/Common/Error/Error.cpp:349-368`

- [ ] **Step 1: Add the enum value**

In `src/Common/Error/Error.h`, extend the enum:

```cpp
enum class RuntimeErrorCode
{
	None,
	IndexOutOfBounds,
	NegativeArraySize,
	ArraySizeExceeded,
	ArrayPopEmpty,
	FFIFunctionNotFound,
	StackOverflow,
	MemoryAccessViolation,
	DivisionByZero,
	InternalTypeError,
	InternalFFITypeError,
	UnsupportedPlatformOperation,
	WorkerCancelled
};
```

- [ ] **Step 2: Add the name mapping**

In `RuntimeErrorCodeName` (`src/Common/Error/Error.cpp`, before the `default:` case):

```cpp
	case RuntimeErrorCode::WorkerCancelled:
		return "WorkerCancelled";
```

- [ ] **Step 3: Classify it as a non-panic error**

In `IsRuntimePanicCode` (`src/Common/Error/Error.cpp`), add `WorkerCancelled` to the non-panic group:

```cpp
		case RuntimeErrorCode::None:
		case RuntimeErrorCode::IndexOutOfBounds:
		case RuntimeErrorCode::NegativeArraySize:
		case RuntimeErrorCode::ArraySizeExceeded:
		case RuntimeErrorCode::ArrayPopEmpty:
		case RuntimeErrorCode::FFIFunctionNotFound:
		case RuntimeErrorCode::DivisionByZero:
		case RuntimeErrorCode::WorkerCancelled:
		default:
			return false;
```

- [ ] **Step 4: Build to verify**

Run: `cmake --build out/build/ninja/x64-development`
Expected: clean build (behavioral coverage comes from Tasks 2–5 tests).

- [ ] **Step 5: Commit**

```bash
git add src/Common/Error/Error.h src/Common/Error/Error.cpp
git commit -m "feat(runtime): add WorkerCancelled runtime error code"
```

---

### Task 2: Stop-token-aware channels

**Files:**
- Modify: `src/Interpreter/Channel/Channel.h`
- Modify: `src/Interpreter/Channel/Channel.cpp`
- Modify: `src/Interpreter/VirtualMachine/VirtualMachine.cpp:424-461` (call sites, interim `std::stop_token{}`)
- Test: `tests/unit/runtime/ChannelTests.cpp` (new; picked up by the `unit/*.cpp` glob in `tests/CMakeLists.txt`)

- [ ] **Step 1: Write the failing tests**

Create `tests/unit/runtime/ChannelTests.cpp`:

```cpp
#include <catch2/catch_test_macros.hpp>

#include "Interpreter/Channel/Channel.h"

#include <chrono>
#include <stop_token>
#include <thread>

TEST_CASE("Channel round-trips a value", "[channel]")
{
	Channel channel(2);
	SerializedValue message;
	message.m_raw_bits = 42u;

	REQUIRE(channel.Send(std::move(message), std::stop_token{}) == ChannelOpStatus::Ok);

	const ChannelReceiveResult received = channel.Receive(std::stop_token{});
	REQUIRE(received.m_status == ChannelOpStatus::Ok);
	REQUIRE(received.m_value.has_value());
	REQUIRE(received.m_value.value().m_raw_bits == 42u);
}

TEST_CASE("Blocked receive is cancelled by stop request", "[channel]")
{
	Channel channel(1);
	std::stop_source stop_source;

	ChannelReceiveResult received;
	std::thread receiver([&channel, &stop_source, &received]()
	{
		received = channel.Receive(stop_source.get_token());
	});

	std::this_thread::sleep_for(std::chrono::milliseconds(50));
	stop_source.request_stop();
	receiver.join();

	REQUIRE(received.m_status == ChannelOpStatus::Cancelled);
}

TEST_CASE("Blocked send is cancelled by stop request", "[channel]")
{
	Channel channel(1);
	SerializedValue first;
	REQUIRE(channel.Send(std::move(first), std::stop_token{}) == ChannelOpStatus::Ok);

	std::stop_source stop_source;
	ChannelOpStatus send_status = ChannelOpStatus::Ok;
	std::thread sender([&channel, &stop_source, &send_status]()
	{
		SerializedValue second;
		send_status = channel.Send(std::move(second), stop_source.get_token());
	});

	std::this_thread::sleep_for(std::chrono::milliseconds(50));
	stop_source.request_stop();
	sender.join();

	REQUIRE(send_status == ChannelOpStatus::Cancelled);
}

TEST_CASE("Close unblocks a pending receive with Closed status", "[channel]")
{
	Channel channel(1);
	ChannelReceiveResult received;
	std::thread receiver([&channel, &received]()
	{
		received = channel.Receive(std::stop_token{});
	});

	std::this_thread::sleep_for(std::chrono::milliseconds(50));
	channel.Close();
	receiver.join();

	REQUIRE(received.m_status == ChannelOpStatus::Closed);
}

TEST_CASE("Send to a closed channel reports Closed", "[channel]")
{
	Channel channel(1);
	channel.Close();

	SerializedValue message;
	REQUIRE(channel.Send(std::move(message), std::stop_token{}) == ChannelOpStatus::Closed);
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cmake --build out/build/ninja/x64-development --target MidoriUnitTests`
Expected: COMPILE ERROR (`ChannelOpStatus` undefined, `Send` takes one argument). A compile failure is the red state here.

- [ ] **Step 3: Rewrite `Channel.h`**

Replace the `Channel` class portion of `src/Interpreter/Channel/Channel.h` (leave `ChannelRegistry` as-is except for the signature changes shown):

```cpp
#pragma once

#include "Interpreter/ValueTransfer/ValueTransfer.h"

#include <condition_variable>
#include <deque>
#include <memory>
#include <mutex>
#include <optional>
#include <stop_token>
#include <unordered_map>

enum class ChannelOpStatus
{
	Ok,
	Closed,
	Cancelled
};

struct ChannelReceiveResult
{
	ChannelOpStatus m_status = ChannelOpStatus::Closed;
	std::optional<SerializedValue> m_value = std::nullopt;
};

class Channel
{
public:
	explicit Channel(int capacity);

	ChannelOpStatus Send(SerializedValue message, std::stop_token stop_token);

	ChannelReceiveResult Receive(std::stop_token stop_token);

	std::optional<SerializedValue> TryReceive();

	void Close();

	bool IsClosed() const;

	bool IsDrained() const;

private:
	mutable std::mutex m_mutex;
	std::condition_variable_any m_not_empty;
	std::condition_variable_any m_not_full;
	std::deque<SerializedValue> m_queue;
	int m_capacity;
	bool m_closed = false;
};

class ChannelRegistry
{
public:
	static ChannelRegistry& GetInstance();

	int CreateChannel(int capacity);

	ChannelOpStatus Send(int channel_id, SerializedValue message, std::stop_token stop_token);

	ChannelReceiveResult Receive(int channel_id, std::stop_token stop_token);

	std::optional<SerializedValue> TryReceive(int channel_id);

	void Close(int channel_id);

private:
	ChannelRegistry() = default;

	mutable std::mutex m_mutex;
	std::unordered_map<int, std::unique_ptr<Channel>> m_channels;
	int m_next_id = 1;

	Channel* FindChannel(int channel_id) const;
};
```

(`IsDrained` is used by Task 5; implement it now so the header is final.)

- [ ] **Step 4: Rewrite the blocking operations in `Channel.cpp`**

```cpp
ChannelOpStatus Channel::Send(SerializedValue message, std::stop_token stop_token)
{
	std::unique_lock<std::mutex> lock(m_mutex);
	const bool ready = m_not_full.wait(lock, stop_token, [this]()
	{
		return m_closed || static_cast<int>(m_queue.size()) < m_capacity;
	});

	if (!ready)
	{
		return ChannelOpStatus::Cancelled;
	}

	if (m_closed)
	{
		return ChannelOpStatus::Closed;
	}

	m_queue.push_back(std::move(message));
	m_not_empty.notify_one();
	return ChannelOpStatus::Ok;
}

ChannelReceiveResult Channel::Receive(std::stop_token stop_token)
{
	std::unique_lock<std::mutex> lock(m_mutex);
	const bool ready = m_not_empty.wait(lock, stop_token, [this]()
	{
		return !m_queue.empty() || m_closed;
	});

	if (!ready)
	{
		return ChannelReceiveResult{ ChannelOpStatus::Cancelled, std::nullopt };
	}

	if (m_queue.empty())
	{
		return ChannelReceiveResult{ ChannelOpStatus::Closed, std::nullopt };
	}

	SerializedValue message = std::move(m_queue.front());
	m_queue.pop_front();
	m_not_full.notify_one();
	return ChannelReceiveResult{ ChannelOpStatus::Ok, std::move(message) };
}

bool Channel::IsDrained() const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	return m_closed && m_queue.empty();
}
```

Update the registry pass-throughs in the same file:

```cpp
ChannelOpStatus ChannelRegistry::Send(int channel_id, SerializedValue message, std::stop_token stop_token)
{
	Channel* channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return ChannelOpStatus::Closed;
	}
	return channel->Send(std::move(message), std::move(stop_token));
}

ChannelReceiveResult ChannelRegistry::Receive(int channel_id, std::stop_token stop_token)
{
	Channel* channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return ChannelReceiveResult{ ChannelOpStatus::Closed, std::nullopt };
	}
	return channel->Receive(std::move(stop_token));
}
```

`TryReceive`, `Close`, `IsClosed`, `CreateChannel`, `FindChannel` are unchanged (the `condition_variable_any` member type change is source-compatible with `notify_one`/`notify_all`).

- [ ] **Step 5: Fix the VM call sites (interim, no behavior change)**

In `src/Interpreter/VirtualMachine/VirtualMachine.cpp`, `ExecuteConcurrencyInstruction`:

`CHANNEL_SEND` — replace the send call and push:

```cpp
		const ChannelOpStatus send_status = ChannelRegistry::GetInstance().Send(channel_id, std::move(serialized_value.value()), std::stop_token{});
		Push(send_status == ChannelOpStatus::Ok);
		return true;
```

`CHANNEL_RECEIVE` — replace the receive call and empty-check:

```cpp
		ChannelReceiveResult received_value = ChannelRegistry::GetInstance().Receive(channel_id, std::stop_token{});
		if (received_value.m_status != ChannelOpStatus::Ok)
		{
			m_instruction_pointer = ip;
			static_cast<void>(TerminateExecution(GenerateRuntimeError(RuntimeErrorCode::InternalTypeError, "Cannot receive from a closed and empty channel.", GetLine())));
			return false;
		}

		std::expected<MidoriValue, std::string> deserialized_value = ValueTransfer::Deserialize(received_value.m_value.value(), *this);
```

(The `Cancelled` branch is unreachable with a default token; Task 3 wires the real token and distinct error.)

- [ ] **Step 6: Run the unit tests**

Run: `cmake --build out/build/ninja/x64-development --target MidoriUnitTests` then `MidoriUnitTests.exe "[channel]"`
Expected: all 5 `[channel]` tests PASS.

- [ ] **Step 7: Run the concurrency regression suite**

Run: `python scripts/run_tests.py --category concurrency --build Development`
Expected: all existing tests PASS (behavior unchanged for non-cancelled paths).

- [ ] **Step 8: Commit**

```bash
git add src/Interpreter/Channel tests/unit/runtime/ChannelTests.cpp src/Interpreter/VirtualMachine/VirtualMachine.cpp
git commit -m "feat(runtime): make channel send/receive stop_token-aware"
```

---

### Task 3: VM cancellation plumbing (safepoint + channel ops + worker token)

**Files:**
- Modify: `src/Interpreter/VirtualMachine/VirtualMachine.h` (members near `m_worker_proc_index`, public accessor near `PrepareWorkerCall`)
- Modify: `src/Interpreter/VirtualMachine/VirtualMachine.cpp:2268-2274` (JUMP_BACK) and `ExecuteConcurrencyInstruction`
- Modify: `src/Interpreter/Worker/Worker.cpp:140` (pass token)
- Test: `tests/unit/runtime/WorkerCancellationTests.cpp` (new)

- [ ] **Step 1: Write the failing end-to-end test**

Create `tests/unit/runtime/WorkerCancellationTests.cpp`:

```cpp
#include <catch2/catch_test_macros.hpp>

#include "support/CompileHelpers.h"
#include "support/TempDir.h"

#include <cstdlib>
#include <expected>
#include <filesystem>
#include <format>
#include <string>

namespace
{
	std::filesystem::path RepositoryRoot()
	{
		return std::filesystem::path(__FILE__).parent_path().parent_path().parent_path().parent_path();
	}

	std::string MidoriPathLiteral(const std::filesystem::path& path)
	{
		std::error_code error_code;
		const std::filesystem::path canonical_path = std::filesystem::weakly_canonical(path, error_code);
		if (!error_code)
		{
			return canonical_path.generic_string();
		}

		return path.lexically_normal().generic_string();
	}

	const MidoriTest::ExecutedSnippet& RequireExecutedSnippet(const std::expected<MidoriTest::ExecutedSnippet, CompilerError>& run_result)
	{
		if (!run_result.has_value())
		{
			FAIL(std::string(run_result.error().Rendered()));
		}

		return run_result.value();
	}
}

TEST_CASE("Joining a cancelled spinning worker reports a cancellation error", "[runtime][worker][cancel]")
{
	const std::filesystem::path system_module_path = RepositoryRoot() / "MidoriPrelude" / "System.mdr";
	const MidoriTest::TempDir temp_dir("midori-worker-cancel");
	const std::filesystem::path source_file_path = temp_dir.Path() / "WorkerCancelJoin.mdr";

	const std::string source_code = std::format(
		R"(module WorkerCancelJoin
import {{ "{}" }}
defun Spin(dummy: Int) : Int => {{
    def i = 0;
    loop
    {{
        i = i + 1;
        if i < 0 then break () else ();
    }};
    i
}};
def w = spawn Spin(0);
System::Sleep(50);
def cancelled = cancel(w);
def r = join w;
defun main(): Int => 0;
)",
		MidoriPathLiteral(system_module_path));

	const std::expected<MidoriTest::ExecutedSnippet, CompilerError> run_result =
		MidoriTest::ExecuteSnippet(source_code, source_file_path.string());
	const MidoriTest::ExecutedSnippet& executed = RequireExecutedSnippet(run_result);

	REQUIRE(executed.m_exit_code != EXIT_SUCCESS);
	REQUIRE(executed.m_output.m_stderr.find("cancelled") != std::string::npos);
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cmake --build out/build/ninja/x64-development --target MidoriUnitTests` then `MidoriUnitTests.exe "[cancel]"`
Expected: the test HANGS (join waits on a worker that never observes stop) or FAILS. If it hangs, kill it — that hang is the bug being fixed. Do not leave it running.

- [ ] **Step 3: Add the stop token to `VirtualMachine.h`**

Add `#include <stop_token>` to the header includes. Next to `int m_worker_proc_index = -1;` add:

```cpp
	std::stop_token m_stop_token;
	bool m_stop_possible = false;
```

In the public section (next to `PrepareWorkerCall`):

```cpp
	void SetStopToken(std::stop_token stop_token) noexcept
	{
		m_stop_token = std::move(stop_token);
		m_stop_possible = m_stop_token.stop_possible();
	}
```

- [ ] **Step 4: Add the JUMP_BACK safepoint**

In `src/Interpreter/VirtualMachine/VirtualMachine.cpp` replace the `JUMP_BACK` case:

```cpp
		case OpCode::JUMP_BACK:
		{
			int offset = ReadShort(ip);
			ip -= offset;
			if (m_stop_possible && m_stop_token.stop_requested()) [[unlikely]]
			{
				SyncMachineState(ip, sp, bp, env);
				return TerminateExecution(GenerateRuntimeError(RuntimeErrorCode::WorkerCancelled, "Worker cancelled.", GetLine()));
			}
			TryCollect(ip, sp, bp, env);
			break;
		}
```

- [ ] **Step 5: Wire the real token into the channel opcodes**

In `ExecuteConcurrencyInstruction`, replace the Task 2 interim `std::stop_token{}` arguments with `m_stop_token` and handle `Cancelled`:

`CHANNEL_SEND`:

```cpp
		const ChannelOpStatus send_status = ChannelRegistry::GetInstance().Send(channel_id, std::move(serialized_value.value()), m_stop_token);
		if (send_status == ChannelOpStatus::Cancelled)
		{
			m_instruction_pointer = ip;
			static_cast<void>(TerminateExecution(GenerateRuntimeError(RuntimeErrorCode::WorkerCancelled, "Worker cancelled.", GetLine())));
			return false;
		}

		Push(send_status == ChannelOpStatus::Ok);
		return true;
```

`CHANNEL_RECEIVE`:

```cpp
		ChannelReceiveResult received_value = ChannelRegistry::GetInstance().Receive(channel_id, m_stop_token);
		if (received_value.m_status == ChannelOpStatus::Cancelled)
		{
			m_instruction_pointer = ip;
			static_cast<void>(TerminateExecution(GenerateRuntimeError(RuntimeErrorCode::WorkerCancelled, "Worker cancelled.", GetLine())));
			return false;
		}
		if (received_value.m_status == ChannelOpStatus::Closed)
		{
			m_instruction_pointer = ip;
			static_cast<void>(TerminateExecution(GenerateRuntimeError(RuntimeErrorCode::InternalTypeError, "Cannot receive from a closed and empty channel.", GetLine())));
			return false;
		}

		std::expected<MidoriValue, std::string> deserialized_value = ValueTransfer::Deserialize(received_value.m_value.value(), *this);
```

- [ ] **Step 6: Pass the worker's token into its VM**

In `Worker::Execute` (`src/Interpreter/Worker/Worker.cpp`), immediately after `VirtualMachine worker_vm(m_executable, 0, nullptr);`:

```cpp
		worker_vm.SetStopToken(stop_token);
```

- [ ] **Step 7: Run the test to verify it passes**

Run: `MidoriUnitTests.exe "[cancel]"`
Expected: PASS — the worker terminates at the loop back-edge, `join` surfaces "Worker cancelled." as a runtime error, exit code is non-zero.

- [ ] **Step 8: Run the full regression suites**

Run: `MidoriUnitTests.exe` (all) and `python scripts/run_tests.py --build Development`
Expected: all PASS. The main VM has a stop-impossible default token, so `m_stop_possible` is `false` and the safepoint costs one predictable branch.

- [ ] **Step 9: Commit**

```bash
git add src/Interpreter/VirtualMachine src/Interpreter/Worker/Worker.cpp tests/unit/runtime/WorkerCancellationTests.cpp
git commit -m "feat(vm): cooperative worker cancellation via stop-token safepoints"
```

---

### Task 4: End-to-end `.mdr` cancellation tests

**Files:**
- Create: `test/concurrency/success/worker_cancel_spin.mdr` + `.expected`
- Create: `test/concurrency/success/worker_cancel_blocked_receive.mdr` + `.expected`

- [ ] **Step 1: Add the spin-loop cancellation test**

`test/concurrency/success/worker_cancel_spin.mdr`:

```
module worker_cancel_spin

import { "../../../MidoriPrelude/IO.mdr" }
use IO.{PrintLine}

import { "../../../MidoriPrelude/System.mdr" }
use System.{Sleep}

defun Spin(dummy: Int) : Int => {
    def i = 0;
    loop
    {
        i = i + 1;
        if i < 0 then break () else ();
    };
    i
};

def w = spawn Spin(0);
Sleep(50);
def cancelled = cancel(w);
def attempts = 0;
loop
{
    if is_done(w) then break () else ();
    Sleep(10);
    attempts = attempts + 1;
    if attempts >= 500 then break () else ();
};
if cancelled && is_done(w)
then PrintLine("spin worker cancelled")
else PrintLine("spin worker still running");
```

`test/concurrency/success/worker_cancel_spin.expected`:

```
spin worker cancelled
```

- [ ] **Step 2: Add the blocked-receive cancellation test**

`test/concurrency/success/worker_cancel_blocked_receive.mdr`:

```
module worker_cancel_blocked_receive

import { "../../../MidoriPrelude/IO.mdr" }
use IO.{PrintLine}

import { "../../../MidoriPrelude/System.mdr" }
use System.{Sleep}

defun BlockOnChannel(ch: Channel<Int>) : Int => {
    def v = <- ch;
    v
};

def ch = channel<Int>(1);
def w = spawn BlockOnChannel(ch);
Sleep(50);
def cancelled = cancel(w);
def attempts = 0;
loop
{
    if is_done(w) then break () else ();
    Sleep(10);
    attempts = attempts + 1;
    if attempts >= 500 then break () else ();
};
if cancelled && is_done(w)
then PrintLine("blocked worker cancelled")
else PrintLine("blocked worker still running");
```

`test/concurrency/success/worker_cancel_blocked_receive.expected`:

```
blocked worker cancelled
```

- [ ] **Step 3: Run both tests**

Run: `python scripts/run_tests.py --pattern worker_cancel --build Development`
Expected: `worker_cancel`, `worker_cancel_spin`, `worker_cancel_blocked_receive` all PASS.

- [ ] **Step 4: Commit**

```bash
git add test/concurrency/success/worker_cancel_spin.mdr test/concurrency/success/worker_cancel_spin.expected test/concurrency/success/worker_cancel_blocked_receive.mdr test/concurrency/success/worker_cancel_blocked_receive.expected
git commit -m "test(concurrency): cover cancellation of spinning and channel-blocked workers"
```

---

### Task 5: Channel registry lifetime (fix the leak)

**Files:**
- Modify: `src/Interpreter/Channel/Channel.h` (`ChannelRegistry` only)
- Modify: `src/Interpreter/Channel/Channel.cpp` (`ChannelRegistry` only)
- Test: `tests/unit/runtime/ChannelTests.cpp` (append)

- [ ] **Step 1: Write the failing tests**

Append to `tests/unit/runtime/ChannelTests.cpp`:

```cpp
TEST_CASE("Registry erases a channel once closed and drained", "[channel][registry]")
{
	ChannelRegistry& registry = ChannelRegistry::GetInstance();
	const size_t baseline = registry.GetChannelCount();

	const int channel_id = registry.CreateChannel(1);
	REQUIRE(registry.GetChannelCount() == baseline + 1u);

	SerializedValue message;
	message.m_raw_bits = 7u;
	REQUIRE(registry.Send(channel_id, std::move(message), std::stop_token{}) == ChannelOpStatus::Ok);

	registry.Close(channel_id);
	REQUIRE(registry.GetChannelCount() == baseline + 1u);

	const ChannelReceiveResult drained = registry.Receive(channel_id, std::stop_token{});
	REQUIRE(drained.m_status == ChannelOpStatus::Ok);
	REQUIRE(registry.GetChannelCount() == baseline);

	const ChannelReceiveResult after = registry.Receive(channel_id, std::stop_token{});
	REQUIRE(after.m_status == ChannelOpStatus::Closed);
}

TEST_CASE("Registry erases an empty channel on close", "[channel][registry]")
{
	ChannelRegistry& registry = ChannelRegistry::GetInstance();
	const size_t baseline = registry.GetChannelCount();

	const int channel_id = registry.CreateChannel(1);
	registry.Close(channel_id);
	REQUIRE(registry.GetChannelCount() == baseline);

	SerializedValue message;
	REQUIRE(registry.Send(channel_id, std::move(message), std::stop_token{}) == ChannelOpStatus::Closed);
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cmake --build out/build/ninja/x64-development --target MidoriUnitTests`
Expected: COMPILE ERROR (`GetChannelCount` does not exist).

- [ ] **Step 3: Convert the registry to `shared_ptr` with erase-on-drain**

In `src/Interpreter/Channel/Channel.h`, replace the `ChannelRegistry` class:

```cpp
class ChannelRegistry
{
public:
	static ChannelRegistry& GetInstance();

	int CreateChannel(int capacity);

	ChannelOpStatus Send(int channel_id, SerializedValue message, std::stop_token stop_token);

	ChannelReceiveResult Receive(int channel_id, std::stop_token stop_token);

	std::optional<SerializedValue> TryReceive(int channel_id);

	void Close(int channel_id);

	size_t GetChannelCount() const;

private:
	ChannelRegistry() = default;

	mutable std::mutex m_mutex;
	std::unordered_map<int, std::shared_ptr<Channel>> m_channels;
	int m_next_id = 1;

	std::shared_ptr<Channel> FindChannel(int channel_id) const;

	void EraseIfDrained(int channel_id);
};
```

In `src/Interpreter/Channel/Channel.cpp`, replace the registry implementations:

```cpp
int ChannelRegistry::CreateChannel(int capacity)
{
	std::lock_guard<std::mutex> lock(m_mutex);
	const int id = m_next_id;
	m_next_id += 1;
	m_channels.emplace(id, std::make_shared<Channel>(capacity));
	return id;
}

ChannelOpStatus ChannelRegistry::Send(int channel_id, SerializedValue message, std::stop_token stop_token)
{
	std::shared_ptr<Channel> channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return ChannelOpStatus::Closed;
	}
	return channel->Send(std::move(message), std::move(stop_token));
}

ChannelReceiveResult ChannelRegistry::Receive(int channel_id, std::stop_token stop_token)
{
	std::shared_ptr<Channel> channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return ChannelReceiveResult{ ChannelOpStatus::Closed, std::nullopt };
	}

	ChannelReceiveResult result = channel->Receive(std::move(stop_token));
	EraseIfDrained(channel_id);
	return result;
}

std::optional<SerializedValue> ChannelRegistry::TryReceive(int channel_id)
{
	std::shared_ptr<Channel> channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return std::nullopt;
	}

	std::optional<SerializedValue> result = channel->TryReceive();
	EraseIfDrained(channel_id);
	return result;
}

void ChannelRegistry::Close(int channel_id)
{
	std::shared_ptr<Channel> channel = FindChannel(channel_id);
	if (channel != nullptr)
	{
		channel->Close();
		EraseIfDrained(channel_id);
	}
}

size_t ChannelRegistry::GetChannelCount() const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	return m_channels.size();
}

std::shared_ptr<Channel> ChannelRegistry::FindChannel(int channel_id) const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	std::unordered_map<int, std::shared_ptr<Channel>>::const_iterator channel_it = m_channels.find(channel_id);
	if (channel_it == m_channels.end())
	{
		return nullptr;
	}
	return channel_it->second;
}

void ChannelRegistry::EraseIfDrained(int channel_id)
{
	std::lock_guard<std::mutex> lock(m_mutex);
	std::unordered_map<int, std::shared_ptr<Channel>>::iterator channel_it = m_channels.find(channel_id);
	if (channel_it != m_channels.end() && channel_it->second->IsDrained())
	{
		m_channels.erase(channel_it);
	}
}
```

Blocked senders/receivers hold a `shared_ptr`, so erasing the map entry never destroys a channel that a thread is still waiting on.

- [ ] **Step 4: Run the unit tests**

Run: `MidoriUnitTests.exe "[channel]"`
Expected: all `[channel]` and `[registry]` tests PASS.

- [ ] **Step 5: Run the concurrency regression suite**

Run: `python scripts/run_tests.py --category concurrency --build Development`
Expected: all PASS (`channel_close_unblock` exercises the drain path: two sends, close, two receives).

- [ ] **Step 6: Commit**

```bash
git add src/Interpreter/Channel tests/unit/runtime/ChannelTests.cpp
git commit -m "fix(runtime): reclaim closed-and-drained channels from the registry"
```

---

### Task 6: Full verification sweep

**Files:** none (verification only)

- [ ] **Step 1: Full build**

Run: `cmake --build out/build/ninja/x64-development`
Expected: clean build, no warnings introduced in touched files.

- [ ] **Step 2: All unit tests**

Run: `MidoriUnitTests.exe`
Expected: all PASS.

- [ ] **Step 3: Full `.mdr` suite**

Run: `python scripts/run_tests.py --build Development`
Expected: all PASS, including the two new concurrency tests.

- [ ] **Step 4: Commit anything outstanding and stop**

If steps 1–3 are green and the tree is clean, the plan is complete. Use the superpowers:finishing-a-development-branch skill to decide on merge/PR.

---

## Self-review notes

- Spec coverage: cancellation of running workers (Tasks 3–4), cancellation of channel-blocked workers (Tasks 2–4), join-after-cancel no longer hangs (Task 3 test), `~Worker`/exit no longer hangs on cancelled workers (same mechanism), channel registry leak (Task 5). Deliberately deferred: `try_receive`/`select`, spawn cost, scheduler, WASM.
- Type consistency: `ChannelOpStatus`/`ChannelReceiveResult` defined in Task 2 and used verbatim in Tasks 3 and 5; `SetStopToken`/`m_stop_token`/`m_stop_possible` defined in Task 3 and used only there.
- Known risk: `MidoriUnitTests.exe` path may differ per generator — locate it with the `Get-ChildItem` command in the build/test section. If the WASM build fails on `<stop_token>`, guard the `SetStopToken` include path with the existing `MIDORI_WASM` define (concurrency opcodes are already stubbed there).
