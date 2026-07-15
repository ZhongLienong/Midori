#include "Worker.h"

#include "Common/Constant/Constant.h"
#include "Common/Printer/Printer.h"

#include <unordered_map>

namespace
{
	struct ProcedureNameParts
	{
		std::string_view m_base_name;
		std::string_view m_module_name;
	};

	ProcedureNameParts ParseProcedureName(const std::string& raw_name)
	{
		const std::string_view raw_name_view(raw_name);
		const size_t separator_index = raw_name_view.rfind(ModuleSeparator);
		if (separator_index == std::string_view::npos)
		{
			return ProcedureNameParts{ raw_name_view, {} };
		}

		return ProcedureNameParts
		{
			raw_name_view.substr(0u, separator_index),
			raw_name_view.substr(separator_index + 1u)
		};
	}

	std::expected<void, std::string> ExecuteWorkerInitializer(VirtualMachine& worker_vm, int proc_index)
	{
		worker_vm.PrepareWorkerCall(proc_index);
		VirtualMachine::ExecuteResult init_result = worker_vm.Execute();
		if (!init_result.has_value())
		{
			return std::unexpected(std::string(init_result.error().m_message));
		}

		return {};
	}

	std::expected<void, std::string> InitializeWorkerGlobals(VirtualMachine& worker_vm, const MidoriExecutable& executable)
	{
		std::string_view entry_module_name;
		if (!executable.m_procedure_names.empty())
		{
			entry_module_name = ParseProcedureName(executable.m_procedure_names[0u]).m_module_name;
		}

		for (int proc_index = 0; proc_index < executable.GetProcedureCount(); proc_index += 1)
		{
			const ProcedureNameParts parts = ParseProcedureName(executable.m_procedure_names[static_cast<size_t>(proc_index)]);
			if (parts.m_base_name != MAIN_PROCEDURE_PREFIX || parts.m_module_name.empty() || parts.m_module_name == entry_module_name)
			{
				continue;
			}

			std::expected<void, std::string> init_result = ExecuteWorkerInitializer(worker_vm, proc_index);
			if (!init_result.has_value())
			{
				return init_result;
			}
		}

		std::unordered_map<std::string, int> global_indices;
		global_indices.reserve(static_cast<size_t>(executable.GetGlobalVariableCount()));
		for (int global_index = 0; global_index < executable.GetGlobalVariableCount(); global_index += 1)
		{
			global_indices.emplace(executable.GetGlobalVariable(global_index), global_index);
		}

		for (int proc_index = 0; proc_index < executable.GetProcedureCount(); proc_index += 1)
		{
			const ProcedureNameParts parts = ParseProcedureName(executable.m_procedure_names[static_cast<size_t>(proc_index)]);
			if (parts.m_module_name != entry_module_name
				|| parts.m_base_name.empty()
				|| parts.m_base_name == MAIN_PROCEDURE_PREFIX
				|| parts.m_base_name == MODULE_BOOTSTRAP_PREFIX)
			{
				continue;
			}

			const std::unordered_map<std::string, int>::const_iterator global_it = global_indices.find(std::string(parts.m_base_name));
			if (global_it == global_indices.cend())
			{
				continue;
			}

			worker_vm.SetGlobalValue(global_it->second, worker_vm.MakeFunctionValue(proc_index));
		}

		return {};
	}
}

Worker::Worker(std::shared_ptr<const MidoriExecutable> executable, int proc_index, std::vector<SerializedValue> serialized_args)
	: m_executable(std::move(executable))
	, m_proc_index(proc_index)
	, m_serialized_args(std::move(serialized_args))
{
	m_thread = std::jthread([this](std::stop_token stop_token)
	{
		Execute(std::move(stop_token));
	});
}

Worker::~Worker()
{
	if (!m_joined.load())
	{
		if (m_thread.joinable())
		{
			m_thread.request_stop();
			m_thread.join();
		}

		if (m_had_error)
		{
			std::lock_guard<std::mutex> lock(m_result_mutex);
			Printer::PrintFormatted<Printer::Color::RED>("[Worker] Unjoined worker error: {}\n", m_error);
		}
	}
}

void Worker::Execute(std::stop_token stop_token)
{
	try
	{
		if (stop_token.stop_requested())
		{
			std::lock_guard<std::mutex> lock(m_result_mutex);
			m_error = "Worker cancelled before execution.";
			m_had_error = true;
			m_done.store(true);
			return;
		}

		VirtualMachine worker_vm(m_executable, 0, nullptr);
		worker_vm.SetStopToken(stop_token);

		std::expected<void, std::string> safety_check = worker_vm.GetDynamicFFIRegistry().ValidateWorkerSafety();
		if (!safety_check.has_value())
		{
			std::lock_guard<std::mutex> lock(m_result_mutex);
			m_error = safety_check.error();
			m_had_error = true;
			m_done.store(true);
			return;
		}

		std::expected<void, std::string> init_result = InitializeWorkerGlobals(worker_vm, *m_executable);
		if (!init_result.has_value())
		{
			std::lock_guard<std::mutex> lock(m_result_mutex);
			m_error = init_result.error();
			m_had_error = true;
			m_done.store(true);
			return;
		}

		worker_vm.PrepareWorkerCall(m_proc_index);
		for (const SerializedValue& serialized_arg : m_serialized_args)
		{
			std::expected<MidoriValue, std::string> deserialized_arg = ValueTransfer::Deserialize(serialized_arg, worker_vm);
			if (!deserialized_arg.has_value())
			{
				std::lock_guard<std::mutex> lock(m_result_mutex);
				m_error = deserialized_arg.error();
				m_had_error = true;
				m_done.store(true);
				return;
			}

			worker_vm.GetValueStackPointer()[0] = deserialized_arg.value();
			worker_vm.AdvanceValueStackPointer();
		}

		VirtualMachine::ExecuteResult execution_result = worker_vm.Execute();
		std::lock_guard<std::mutex> lock(m_result_mutex);
		if (!execution_result.has_value())
		{
			m_error = std::string(execution_result.error().m_message);
			m_had_error = true;
			m_done.store(true);
			return;
		}

		m_exit_code = execution_result.value();
		std::expected<SerializedValue, std::string> serialized_result = ValueTransfer::Serialize(worker_vm.PeekValue(), worker_vm);
		if (!serialized_result.has_value())
		{
			m_error = serialized_result.error();
			m_had_error = true;
			m_done.store(true);
			return;
		}

		m_result = std::move(serialized_result.value());
		m_had_error = false;
		m_done.store(true);
	}
	catch (const std::exception& exception)
	{
		std::lock_guard<std::mutex> lock(m_result_mutex);
		m_error = std::string("Unhandled worker exception: ") + exception.what();
		m_had_error = true;
		m_done.store(true);
	}
	catch (...)
	{
		std::lock_guard<std::mutex> lock(m_result_mutex);
		m_error = "Unhandled worker exception.";
		m_had_error = true;
		m_done.store(true);
	}
}

std::expected<SerializedValue, std::string> Worker::JoinValue()
{
	m_joined.store(true);
	if (m_thread.joinable())
	{
		m_thread.join();
	}

	std::lock_guard<std::mutex> lock(m_result_mutex);
	if (m_had_error)
	{
		return std::unexpected(m_error);
	}
	if (!m_result.has_value())
	{
		return std::unexpected(std::string("Worker completed without a result."));
	}
	return m_result.value();
}

bool Worker::IsDone() const
{
	return m_done.load();
}

bool Worker::Cancel()
{
	if (m_done.load())
	{
		return false;
	}

	return m_thread.request_stop();
}

WorkerRegistry& WorkerRegistry::GetInstance()
{
	static WorkerRegistry instance;
	return instance;
}

int WorkerRegistry::SpawnWorker(std::shared_ptr<const MidoriExecutable> executable, int proc_index, std::vector<SerializedValue> serialized_args)
{
	std::lock_guard<std::mutex> lock(m_mutex);
	const int worker_id = m_next_id;
	m_next_id += 1;
	m_workers.emplace(worker_id, std::make_unique<Worker>(std::move(executable), proc_index, std::move(serialized_args)));
	return worker_id;
}

std::expected<SerializedValue, std::string> WorkerRegistry::JoinWorkerValue(int worker_id)
{
	std::unique_ptr<Worker> worker;
	{
		std::lock_guard<std::mutex> lock(m_mutex);
		std::unordered_map<int, std::unique_ptr<Worker>>::iterator worker_it = m_workers.find(worker_id);
		if (worker_it == m_workers.end())
		{
			return std::unexpected("Worker not found: " + std::to_string(worker_id));
		}

		worker = std::move(worker_it->second);
		m_workers.erase(worker_it);
	}

	return worker->JoinValue();
}

bool WorkerRegistry::IsWorkerDone(int worker_id) const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	std::unordered_map<int, std::unique_ptr<Worker>>::const_iterator worker_it = m_workers.find(worker_id);
	if (worker_it == m_workers.end())
	{
		return true;
	}
	return worker_it->second->IsDone();
}

bool WorkerRegistry::CancelWorker(int worker_id)
{
	std::lock_guard<std::mutex> lock(m_mutex);
	std::unordered_map<int, std::unique_ptr<Worker>>::iterator worker_it = m_workers.find(worker_id);
	if (worker_it == m_workers.end())
	{
		return false;
	}
	return worker_it->second->Cancel();
}
