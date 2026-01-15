#pragma once

#include "Common/Executable/Executable.h"
#include "Common/Value/Value.h"

#include <atomic>
#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

class VirtualMachine;
class GarbageCollector;

class MidoriRuntime
{
public:
	using GlobalVariables = std::vector<MidoriValue>;
	using Task = std::function<void()>;

private:
	std::shared_ptr<const MidoriExecutable> m_executable;
	GlobalVariables m_globals;

	std::vector<std::jthread> m_workers;
	std::queue<Task> m_task_queue;
	std::mutex m_task_mutex;
	std::condition_variable m_task_condition;
	std::atomic<bool> m_shutdown{false};

	std::vector<MidoriTraceable*> m_cross_vm_objects;
	std::mutex m_cross_vm_mutex;

public:
	MidoriRuntime(MidoriExecutable&& executable);
	~MidoriRuntime();

	MidoriRuntime(const MidoriRuntime&) = delete;
	MidoriRuntime& operator=(const MidoriRuntime&) = delete;

	MidoriValue& GetGlobal(int index);
	void SetGlobal(int index, MidoriValue value);
	int GetGlobalCount() const;
	GlobalVariables* GetGlobalsPtr();

	void SpawnTask(MidoriFuture* future, const MidoriClosure& closure);

	const MidoriExecutable& GetExecutable() const;
	std::shared_ptr<const MidoriExecutable> GetExecutablePtr() const;

	MidoriValue DeepCopyForCrossVM(MidoriValue value, const GarbageCollector& gc);

private:
	void WorkerLoop();
	void Shutdown();

	MidoriTraceable* DeepCopyTraceable(MidoriTraceable* src, const GarbageCollector& gc);
};
