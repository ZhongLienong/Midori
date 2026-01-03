#include "TaskRunner.h"
#include "Common/Constant/Constant.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"

TaskRunner& TaskRunner::Instance()
{
	static TaskRunner s_instance;
	return s_instance;
}

TaskRunner::TaskRunner()
{
#if MIDORI_ENABLE_MULTITHREADING
	unsigned int thread_count = std::thread::hardware_concurrency();
	if (thread_count == 0u)
	{
		thread_count = DEFAULT_THREAD_POOL_SIZE;
	}

	m_workers.reserve(thread_count);
	for (unsigned int i = 0u; i < thread_count; i += 1u)
	{
		m_workers.emplace_back([this]() { WorkerLoop(); });
	}
#endif
}

TaskRunner::~TaskRunner()
{
	Shutdown();
}

void TaskRunner::WorkerLoop()
{
	while (true)
	{
		Task task;
		{
			std::unique_lock<std::mutex> lock(m_mutex);
			m_condition.wait(lock, [this]() { return m_shutdown.load(std::memory_order_acquire) || !m_tasks.empty(); });

			if (m_shutdown.load(std::memory_order_acquire) && m_tasks.empty())
			{
				return;
			}

			task = std::move(m_tasks.front());
			m_tasks.pop();
		}
		task();
	}
}

void TaskRunner::SpawnTask(std::shared_ptr<const MidoriExecutable> executable, MidoriFuture* future, GlobalVariables globals)
{
	std::function<void()> task = [executable, future, globals = std::move(globals)]()
	{
		VirtualMachine vm(executable, future->m_closure, globals);
		int exit_code = vm.Execute();

		if (exit_code == EXIT_SUCCESS)
		{
			future->SetResult(vm.GetAsyncResult());
		}
		else
		{
			future->SetError();
		}
	};

#if MIDORI_ENABLE_MULTITHREADING == 0
	task();
#else
	{
		std::lock_guard<std::mutex> lock(m_mutex);
		m_tasks.emplace(std::move(task));
	}
	m_condition.notify_one();
#endif
}

void TaskRunner::Shutdown()
{
#if MIDORI_ENABLE_MULTITHREADING
	m_shutdown.store(true, std::memory_order_release);
	m_condition.notify_all();

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
