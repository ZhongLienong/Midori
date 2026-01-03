#pragma once

#include "Common/Value/Value.h"

#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

class MidoriExecutable;

class TaskRunner
{
public:
	using GlobalVariables = std::vector<MidoriValue>;
	using Task = std::function<void()>;

	static TaskRunner& Instance();

	void SpawnTask(std::shared_ptr<const MidoriExecutable> executable, MidoriFuture* future, GlobalVariables globals);

	void Shutdown();

private:
	TaskRunner();
	~TaskRunner();

	TaskRunner(const TaskRunner&) = delete;
	TaskRunner& operator=(const TaskRunner&) = delete;

	void WorkerLoop();

	std::vector<std::jthread> m_workers;
	std::queue<Task> m_tasks;
	std::mutex m_mutex;
	std::condition_variable m_condition;
	std::atomic<bool> m_shutdown{false};
};
