#include "MidoriRuntime.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"

MidoriRuntime::MidoriRuntime(MidoriExecutable&& executable)
	: m_executable(std::make_shared<MidoriExecutable>(std::move(executable)))
{
	m_globals.resize(static_cast<size_t>(m_executable->GetGlobalVariableCount()));

	unsigned int thread_count = std::thread::hardware_concurrency();
	if (thread_count == 0u)
	{
		thread_count = 4u;
	}

	m_workers.reserve(thread_count);
	for (unsigned int i = 0u; i < thread_count; i += 1u)
	{
		m_workers.emplace_back([this]() { WorkerLoop(); });
	}
}

MidoriRuntime::~MidoriRuntime()
{
	Shutdown();

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

void MidoriRuntime::SpawnTask(MidoriFuture* future, const MidoriClosure& closure)
{
	std::function<void()> task = [this, future, closure]()
	{
		VirtualMachine vm(*this, closure);
		int exit_code = vm.Execute();

		if (exit_code == EXIT_SUCCESS)
		{
			MidoriValue result = DeepCopyForCrossVM(vm.GetAsyncResult(), vm.GetGC());
			future->SetResult(result);
		}
		else
		{
			future->SetError();
		}
	};

	{
		std::lock_guard<std::mutex> lock(m_task_mutex);
		m_task_queue.emplace(std::move(task));
	}
	m_task_condition.notify_one();
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
	MidoriTraceable* ptr = value.GetPointer();
	if (ptr == nullptr || !gc.Contains(ptr))
	{
		return value;
	}

	MidoriTraceable* copied = DeepCopyTraceable(ptr, gc);
	return MidoriValue(copied);
}

MidoriTraceable* MidoriRuntime::DeepCopyTraceable(MidoriTraceable* src, const GarbageCollector& gc)
{
	MidoriTraceable* result = nullptr;

	if (src->IsTraceable<MidoriText>())
	{
		MidoriText& original = src->GetTraceable<MidoriText>();
		result = new MidoriTraceable(MidoriText(original));
	}
	else if (src->IsTraceable<MidoriArray>())
	{
		MidoriArray& original = src->GetTraceable<MidoriArray>();
		MidoriArray copied(original.GetLength());
		for (int i = 0; i < original.GetLength(); i += 1)
		{
			MidoriValue elem = original[i];
			copied[i] = DeepCopyForCrossVM(elem, gc);
		}
		result = new MidoriTraceable(std::move(copied));
	}
	else if (src->IsTraceable<MidoriStruct>())
	{
		MidoriStruct& original = src->GetTraceable<MidoriStruct>();
		MidoriStruct copied;
		copied.m_values = MidoriTuple(original.m_values.GetLength());
		for (int i = 0; i < original.m_values.GetLength(); i += 1)
		{
			copied.m_values[i] = DeepCopyForCrossVM(original.m_values[i], gc);
		}
		result = new MidoriTraceable(std::move(copied));
	}
	else if (src->IsTraceable<MidoriUnion>())
	{
		MidoriUnion& original = src->GetTraceable<MidoriUnion>();
		MidoriUnion copied;
		copied.m_index = original.m_index;
		copied.m_values = MidoriTuple(original.m_values.GetLength());
		for (int i = 0; i < original.m_values.GetLength(); i += 1)
		{
			copied.m_values[i] = DeepCopyForCrossVM(original.m_values[i], gc);
		}
		result = new MidoriTraceable(std::move(copied));
	}
	else if (src->IsTraceable<MidoriIntRange>())
	{
		MidoriIntRange& original = src->GetTraceable<MidoriIntRange>();
		result = new MidoriTraceable(MidoriIntRange(original.GetStart(), original.GetEnd(), original.GetStep()));
	}
	else if (src->IsTraceable<MidoriFloatRange>())
	{
		MidoriFloatRange& original = src->GetTraceable<MidoriFloatRange>();
		result = new MidoriTraceable(MidoriFloatRange(original.GetStart(), original.GetEnd(), original.GetStep()));
	}
	else if (src->IsTraceable<MidoriCellValue>())
	{
		MidoriCellValue& original = src->GetTraceable<MidoriCellValue>();
		MidoriValue copiedVal = DeepCopyForCrossVM(original.GetValue(), gc);
		result = new MidoriTraceable(MidoriCellValue(copiedVal));
	}
	else if (src->IsTraceable<MidoriClosure>())
	{
		MidoriClosure& original = src->GetTraceable<MidoriClosure>();
		MidoriClosure copied;
		copied.m_proc_index = original.m_proc_index;
		copied.m_cell_values = MidoriTuple(original.m_cell_values.GetLength());
		for (int i = 0; i < original.m_cell_values.GetLength(); i += 1)
		{
			copied.m_cell_values[i] = DeepCopyForCrossVM(original.m_cell_values[i], gc);
		}
		result = new MidoriTraceable(std::move(copied));
	}
	else if (src->IsTraceable<MidoriFuture>())
	{
		return src;
	}

	if (result)
	{
		std::lock_guard<std::mutex> lock(m_cross_vm_mutex);
		m_cross_vm_objects.emplace_back(result);
	}

	return result;
}

void MidoriRuntime::WorkerLoop()
{
	while (true)
	{
		Task task;
		{
			std::unique_lock<std::mutex> lock(m_task_mutex);
			m_task_condition.wait
			(
				lock, 
				[this]()
				{
					return m_shutdown.load(std::memory_order_acquire) || !m_task_queue.empty();
				}
			);

			if (m_shutdown.load(std::memory_order_acquire) && m_task_queue.empty())
			{
				return;
			}

			if (!m_task_queue.empty())
			{
				task = std::move(m_task_queue.front());
				m_task_queue.pop();
			}
		}

		if (task)
		{
			task();
		}
	}
}

void MidoriRuntime::Shutdown()
{
	m_shutdown.store(true, std::memory_order_release);
	m_task_condition.notify_all();

	for (std::jthread& worker : m_workers)
	{
		if (worker.joinable())
		{
			worker.join();
		}
	}

	m_workers.clear();
}
