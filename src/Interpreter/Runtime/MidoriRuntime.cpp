#include "MidoriRuntime.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <limits>

namespace
{
constexpr unsigned int MAX_WORKER_COUNT = 1024u;

unsigned int GetDefaultWorkerCount()
{
	unsigned int thread_count = std::thread::hardware_concurrency();
	if (thread_count == 0u)
	{
		thread_count = 1u;
	}

	return thread_count;
}

unsigned int GetConfiguredWorkerCount()
{
	const unsigned int default_count = GetDefaultWorkerCount();
#if defined(_MSC_VER)
	char* env_value = nullptr;
	size_t env_size = 0u;
	if (_dupenv_s(&env_value, &env_size, "MIDORI_ASYNC_WORKERS") != 0 || env_value == nullptr || env_value[0] == '\0')
	{
		if (env_value != nullptr)
		{
			free(env_value);
		}
		return default_count;
	}

	char* parse_end = nullptr;
	const unsigned long configured = std::strtoul(env_value, &parse_end, 10);
	const bool invalid_value = (parse_end == env_value) || (*parse_end != '\0') || (configured == 0ul);
	free(env_value);
	if (invalid_value)
	{
		return default_count;
	}
#else
	const char* env_value = std::getenv("MIDORI_ASYNC_WORKERS");
	if (env_value == nullptr || env_value[0] == '\0')
	{
		return default_count;
	}

	char* parse_end = nullptr;
	const unsigned long configured = std::strtoul(env_value, &parse_end, 10);
	if (parse_end == env_value || *parse_end != '\0' || configured == 0ul)
	{
		return default_count;
	}
#endif

	if (configured > static_cast<unsigned long>(std::numeric_limits<unsigned int>::max()))
	{
		return MAX_WORKER_COUNT;
	}

	return std::clamp(static_cast<unsigned int>(configured), 1u, MAX_WORKER_COUNT);
}
}

MidoriRuntime::MidoriRuntime(MidoriExecutable&& executable)
	: m_executable(std::make_shared<MidoriExecutable>(std::move(executable)))
{
	m_globals.resize(static_cast<size_t>(m_executable->GetGlobalVariableCount()));
	if (m_executable->GetExecutionMode() == ExecutionMode::AsyncEnabled)
	{
		m_shared_globals.resize(static_cast<size_t>(m_executable->GetGlobalVariableCount()));
		for (MidoriSharedCellHandle::SharedState& shared_global : m_shared_globals)
		{
			shared_global = std::make_shared<MidoriSharedCellState>(MidoriValue());
		}
	}

	if (m_executable->GetExecutionMode() != ExecutionMode::AsyncEnabled)
	{
		return;
	}

	m_accepting_tasks.store(true, std::memory_order_release);

#ifndef __EMSCRIPTEN__
	const unsigned int thread_count = GetConfiguredWorkerCount();

	m_workers.reserve(thread_count);
	for (unsigned int i = 0u; i < thread_count; i += 1u)
	{
		m_workers.emplace_back([this]() { WorkerLoop(); });
	}
#endif
}

MidoriRuntime::~MidoriRuntime()
{
#ifndef __EMSCRIPTEN__
	Shutdown();
#endif

	for (MidoriTraceable* ptr : m_cross_vm_objects)
	{
		delete ptr;
	}
	m_cross_vm_objects.clear();
}

MidoriValue& MidoriRuntime::GetGlobal(int index)
{
	return m_globals[static_cast<size_t>(index)];
}

void MidoriRuntime::SetGlobal(int index, MidoriValue value)
{
	m_globals[static_cast<size_t>(index)] = value;
}

int MidoriRuntime::GetGlobalCount() const
{
	return static_cast<int>(m_globals.size());
}

MidoriRuntime::GlobalVariables* MidoriRuntime::GetGlobalsPtr()
{
	return &m_globals;
}

MidoriValue MidoriRuntime::GetSharedGlobal(int index) const
{
	const MidoriSharedCellHandle::SharedState& shared_cell = m_shared_globals[static_cast<size_t>(index)];
	if (!shared_cell)
	{
		return MidoriValue();
	}

#if MIDORI_DEBUG_FULL
	std::lock_guard<std::mutex> lock(shared_cell->m_mutex);
	return shared_cell->m_value;
#else
	return MidoriValue::FromRawBits(shared_cell->m_value_bits.load(std::memory_order_acquire));
#endif
}

void MidoriRuntime::SetSharedGlobal(int index, MidoriValue value)
{
	const MidoriSharedCellHandle::SharedState& shared_cell = m_shared_globals[static_cast<size_t>(index)];
	if (!shared_cell)
	{
		return;
	}

#if MIDORI_DEBUG_FULL
	std::lock_guard<std::mutex> lock(shared_cell->m_mutex);
	shared_cell->m_value = value;
#else
	shared_cell->m_value_bits.store(value.GetRawBits(), std::memory_order_release);
#endif
}

void MidoriRuntime::SetSharedGlobal(int index, MidoriValue value, const GarbageCollector& gc)
{
	SetSharedGlobal(index, DeepCopyForCrossVM(value, gc));
}

MidoriTraceable* MidoriRuntime::CreateManagedFuture()
{
	MidoriFuture future_val;
	MidoriTraceable* future_ptr = new MidoriTraceable(std::move(future_val));

	{
		std::lock_guard<std::mutex> lock(m_cross_vm_mutex);
		m_cross_vm_objects.emplace_back(future_ptr);
	}

	return future_ptr;
}

int MidoriRuntime::RunRootTask()
{
	if (m_executable->GetExecutionMode() != ExecutionMode::AsyncEnabled)
	{
		VirtualMachine vm(*this);
		return vm.Execute();
	}

	MidoriFuture::FutureStateHandle root_future_state = std::make_shared<MidoriFuture::FutureState>();
	Task root_task;
	root_task.m_future_state = root_future_state;
	root_task.m_closure.m_proc_index = 0;
	root_task.m_closure.m_cell_values = MidoriTuple();

#ifdef __EMSCRIPTEN__
	VirtualMachine vm(*this);
	int exit_code = vm.ExecuteTask(root_task.m_closure);

	if (exit_code == EXIT_SUCCESS)
	{
		root_future_state->SetResult(MidoriValue());
	}
	else
	{
		root_future_state->SetError();
	}
#else
	if (!TryEnqueueTask(std::move(root_task)))
	{
		root_future_state->SetError();
	}
#endif

	root_future_state->Get();
	return root_future_state->HasError() ? EXIT_FAILURE : EXIT_SUCCESS;
}

void MidoriRuntime::SpawnTask(MidoriFuture::FutureStateHandle future_state, const MidoriClosure& closure, const GarbageCollector& gc)
{
	if (!future_state)
	{
		return;
	}

	if (!m_accepting_tasks.load(std::memory_order_acquire))
	{
		future_state->SetError();
		return;
	}

	Task task;
	task.m_future_state = future_state;
	task.m_closure.m_proc_index = closure.m_proc_index;

	const int length = closure.m_cell_values.GetLength();
	task.m_closure.m_cell_values = MidoriTuple(length);
	for (int i = 0; i < length; i += 1)
	{
		task.m_closure.m_cell_values[i] = DeepCopyForCrossVM(closure.m_cell_values[i], gc);
	}

#ifdef __EMSCRIPTEN__
	// WASM: Run synchronously (no thread support)
	VirtualMachine vm(*this);
	int exit_code = vm.ExecuteTask(task.m_closure);

	if (exit_code == EXIT_SUCCESS)
	{
		MidoriValue result = DeepCopyForCrossVM(vm.GetAsyncResult(), vm.GetGC());
		task.m_future_state->SetResult(result);
	}
	else
	{
		task.m_future_state->SetError();
	}
#else
	if (!TryEnqueueTask(std::move(task)))
	{
		future_state->SetError();
	}
#endif
}

MidoriValue MidoriRuntime::AwaitFuture(const MidoriFuture::FutureStateHandle& future_state)
{
	if (!future_state)
	{
		return MidoriValue();
	}

#ifdef __EMSCRIPTEN__
	return future_state->Get();
#else
	if (future_state->IsReady())
	{
		return future_state->Get();
	}

	VirtualMachine helper_vm(*this);

	// Cooperative waiting: while this worker awaits one future, it executes other runnable tasks.
	while (!future_state->IsReady())
	{
		Task task;
		if (!WaitForTaskOrFuture(task, future_state))
		{
			if (m_shutdown.load(std::memory_order_acquire) && !future_state->IsReady())
			{
				future_state->SetError();
			}
			continue;
		}

		ExecuteQueuedTask(task, helper_vm);
	}

	return future_state->Get();
#endif
}

const MidoriExecutable& MidoriRuntime::GetExecutable() const
{
	return *m_executable;
}

std::shared_ptr<const MidoriExecutable> MidoriRuntime::GetExecutablePtr() const
{
	return m_executable;
}

MidoriValue MidoriRuntime::DeepCopyForCrossVM(MidoriValue value, const GarbageCollector& gc)
{
	DeepCopyCache copied_map;
	copied_map.reserve(64u);
	std::vector<MidoriTraceable*> new_objects;
	new_objects.reserve(64u);

	MidoriValue copied_value = DeepCopyForCrossVM(value, gc, copied_map, new_objects);

	if (!new_objects.empty())
	{
		std::lock_guard<std::mutex> lock(m_cross_vm_mutex);
		m_cross_vm_objects.reserve(m_cross_vm_objects.size() + new_objects.size());
		m_cross_vm_objects.insert(m_cross_vm_objects.end(), new_objects.begin(), new_objects.end());
	}

	return copied_value;
}

MidoriValue MidoriRuntime::DeepCopyForCrossVM
(
	MidoriValue value,
	const GarbageCollector& gc,
	DeepCopyCache& copied_map,
	std::vector<MidoriTraceable*>& new_objects
)
{
	MidoriTraceable* ptr = value.GetPointer();
	if (ptr == nullptr || !gc.Contains(ptr))
	{
		return value;
	}

	std::unordered_map<MidoriTraceable*, MidoriTraceable*>::iterator it = copied_map.find(ptr);
	if (it != copied_map.end())
	{
		return MidoriValue(it->second);
	}

	MidoriTraceable* copied = DeepCopyTraceable(ptr, gc, copied_map, new_objects);
	return MidoriValue(copied);
}

MidoriTraceable* MidoriRuntime::DeepCopyTraceable(MidoriTraceable* src, const GarbageCollector& gc, DeepCopyCache& copied_map, std::vector<MidoriTraceable*>& new_objects)
{
	std::unordered_map<MidoriTraceable*, MidoriTraceable*>::iterator copied_it = copied_map.find(src);
	if (copied_it != copied_map.end())
	{
		return copied_it->second;
	}

	MidoriTraceable* result = nullptr;

	if (src->IsTraceable<MidoriText>())
	{
		MidoriText& original = src->GetTraceable<MidoriText>();
		result = new MidoriTraceable(MidoriText(original));
		copied_map.emplace(src, result);
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriArray>())
	{
		MidoriArray& original = src->GetTraceable<MidoriArray>();
		const int length = original.GetLength();
		MidoriArray copied(length);
		result = new MidoriTraceable(std::move(copied));
		copied_map.emplace(src, result);

		MidoriArray& copied_array = result->GetTraceable<MidoriArray>();
		for (int i = 0; i < length; i += 1)
		{
			copied_array[i] = DeepCopyForCrossVM(original[i], gc, copied_map, new_objects);
		}
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriStruct>())
	{
		MidoriStruct& original = src->GetTraceable<MidoriStruct>();
		const int length = original.m_values.GetLength();
		MidoriStruct copied;
		copied.m_values = MidoriTuple(length);
		result = new MidoriTraceable(std::move(copied));
		copied_map.emplace(src, result);

		MidoriStruct& copied_struct = result->GetTraceable<MidoriStruct>();
		for (int i = 0; i < length; i += 1)
		{
			copied_struct.m_values[i] = DeepCopyForCrossVM(original.m_values[i], gc, copied_map, new_objects);
		}
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriUnion>())
	{
		MidoriUnion& original = src->GetTraceable<MidoriUnion>();
		const int length = original.m_values.GetLength();
		MidoriUnion copied;
		copied.m_index = original.m_index;
		copied.m_values = MidoriTuple(length);
		result = new MidoriTraceable(std::move(copied));
		copied_map.emplace(src, result);

		MidoriUnion& copied_union = result->GetTraceable<MidoriUnion>();
		for (int i = 0; i < length; i += 1)
		{
			copied_union.m_values[i] = DeepCopyForCrossVM(original.m_values[i], gc, copied_map, new_objects);
		}
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriIntRange>())
	{
		MidoriIntRange& original = src->GetTraceable<MidoriIntRange>();
		result = new MidoriTraceable(MidoriIntRange(original.GetStart(), original.GetEnd(), original.GetStep()));
		copied_map.emplace(src, result);
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriFloatRange>())
	{
		MidoriFloatRange& original = src->GetTraceable<MidoriFloatRange>();
		result = new MidoriTraceable(MidoriFloatRange(original.GetStart(), original.GetEnd(), original.GetStep()));
		copied_map.emplace(src, result);
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriCellValue>())
	{
		MidoriCellValue& original = src->GetTraceable<MidoriCellValue>();
		result = new MidoriTraceable(MidoriCellValue(MidoriValue()));
		copied_map.emplace(src, result);

		MidoriCellValue& copied_cell = result->GetTraceable<MidoriCellValue>();
		copied_cell.GetValue() = DeepCopyForCrossVM(original.GetValue(), gc, copied_map, new_objects);
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriClosure>())
	{
		MidoriClosure& original = src->GetTraceable<MidoriClosure>();
		const int length = original.m_cell_values.GetLength();
		MidoriClosure copied;
		copied.m_proc_index = original.m_proc_index;
		copied.m_cell_values = MidoriTuple(length);
		result = new MidoriTraceable(std::move(copied));
		copied_map.emplace(src, result);

		MidoriClosure& copied_closure = result->GetTraceable<MidoriClosure>();
		for (int i = 0; i < length; i += 1)
		{
			copied_closure.m_cell_values[i] = DeepCopyForCrossVM(original.m_cell_values[i], gc, copied_map, new_objects);
		}
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriSharedCellHandle>())
	{
		MidoriSharedCellHandle& original = src->GetTraceable<MidoriSharedCellHandle>();
		result = new MidoriTraceable(MidoriSharedCellHandle(original.m_state));
		copied_map.emplace(src, result);
		new_objects.emplace_back(result);
	}
	else if (src->IsTraceable<MidoriFuture>())
	{
		copied_map.emplace(src, src);
		return src;
	}

	return result;
}

bool MidoriRuntime::TryEnqueueTask(Task&& task)
{
#ifdef __EMSCRIPTEN__
	(void)task;
	return false;
#else
	{
		std::lock_guard<std::mutex> lock(m_task_mutex);
		if (!m_accepting_tasks.load(std::memory_order_acquire) || m_shutdown.load(std::memory_order_acquire))
		{
			return false;
		}

		m_task_queue.emplace(std::move(task));
	}

	m_task_condition.notify_one();
	return true;
#endif
}

bool MidoriRuntime::WaitForTaskOrFuture(Task& task, const MidoriFuture::FutureStateHandle& awaited_future)
{
#ifdef __EMSCRIPTEN__
	(void)task;
	(void)awaited_future;
	return false;
#else
	std::unique_lock<std::mutex> lock(m_task_mutex);
	using ClockDuration = std::chrono::steady_clock::duration;
	const ClockDuration wait_step_min = std::chrono::microseconds(50);
	const ClockDuration wait_step_max = std::chrono::milliseconds(2);
	ClockDuration wait_step = wait_step_min;

	for (;;)
	{
		if (awaited_future && awaited_future->IsReady())
		{
			return false;
		}

		if (!m_task_queue.empty())
		{
			task = std::move(m_task_queue.front());
			m_task_queue.pop();
			return true;
		}

		if (m_shutdown.load(std::memory_order_acquire))
		{
			return false;
		}

		if (awaited_future)
		{
			m_task_condition.wait_for(lock, wait_step);
			wait_step = std::min(wait_step * 2, wait_step_max);
		}
		else
		{
			m_task_condition.wait
			(
				lock,
				[this]()
				{
					return m_shutdown.load(std::memory_order_acquire) || !m_task_queue.empty();
				}
			);
		}
	}
#endif
}

void MidoriRuntime::ExecuteQueuedTask(Task& task, VirtualMachine& vm)
{
	if (!task.m_future_state)
	{
		return;
	}

	const int exit_code = vm.ExecuteTask(task.m_closure);
	if (exit_code == EXIT_SUCCESS)
	{
		MidoriValue result = DeepCopyForCrossVM(vm.GetAsyncResult(), vm.GetGC());
		task.m_future_state->SetResult(result);
	}
	else
	{
		task.m_future_state->SetError();
	}
}

void MidoriRuntime::WorkerLoop()
{
	VirtualMachine worker_vm(*this);

	while (true)
	{
		Task task;
		if (!WaitForTaskOrFuture(task, nullptr))
		{
			if (m_shutdown.load(std::memory_order_acquire))
			{
				return;
			}
			continue;
		}

		ExecuteQueuedTask(task, worker_vm);
	}
}

void MidoriRuntime::Shutdown()
{
#ifndef __EMSCRIPTEN__
	m_accepting_tasks.store(false, std::memory_order_release);

	bool expected = false;
	if (!m_shutdown.compare_exchange_strong(expected, true, std::memory_order_acq_rel, std::memory_order_acquire))
	{
		return;
	}

	m_task_condition.notify_all();

	for (std::jthread& worker : m_workers)
	{
		if (worker.joinable())
		{
			worker.join();
		}
	}

	m_workers.clear();
#endif
}
