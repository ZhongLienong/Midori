#include "Executable.h"

BytecodeStream::iterator BytecodeStream::begin()
{
	return m_bytecode.begin();
}

BytecodeStream::iterator BytecodeStream::end()
{
	return m_bytecode.end();
}

BytecodeStream::const_iterator BytecodeStream::cbegin() const
{
	return m_bytecode.cbegin();
}

BytecodeStream::const_iterator BytecodeStream::cend() const
{
	return m_bytecode.cend();
}

BytecodeStream::reverse_iterator BytecodeStream::rbegin()
{
	return m_bytecode.rbegin();
}

BytecodeStream::reverse_iterator BytecodeStream::rend()
{
	return m_bytecode.rend();
}

BytecodeStream::const_reverse_iterator BytecodeStream::crbegin() const
{
	return m_bytecode.crbegin();
}

BytecodeStream::const_reverse_iterator BytecodeStream::crend() const
{
	return m_bytecode.crend();
}

OpCode BytecodeStream::ReadByteCode(int index) const
{
	return m_bytecode[static_cast<size_t>(index)];
}

void BytecodeStream::SetByteCode(int index, OpCode byte)
{
	m_bytecode[static_cast<size_t>(index)] = byte;
}

void BytecodeStream::AddByteCode(OpCode byte, int line)
{
	m_bytecode.push_back(byte);

	if (m_line_info.empty() || m_line_info.back().first != line)
	{
		m_line_info.emplace_back(line, 1);
	}
	else
	{
		m_line_info.back().second += 1;
	}
}

void BytecodeStream::PopByteCode(int line)
{
	if (m_bytecode.empty())
	{
		return;
	}
	m_bytecode.pop_back();

	if (m_line_info.empty() || m_line_info.back().first != line)
	{
		return;
	}
	m_line_info.back().second -= 1;
	if (m_line_info.back().second == 0)
	{
		m_line_info.pop_back();
	}
}

int BytecodeStream::GetByteCodeSize() const
{
	return static_cast<int>(m_bytecode.size());
}

bool BytecodeStream::IsByteCodeEmpty() const
{
	return m_bytecode.empty();
}

int BytecodeStream::GetLine(int index) const
{
	int cumulative_count = 0;
	for (const auto& [line, count] : m_line_info)
	{
		cumulative_count += count;
		if (index < cumulative_count)
		{
			return line;
		}
	}

	return -1; // TODO: Throw exception
}

void BytecodeStream::Append(BytecodeStream&& other)
{
	m_bytecode.insert(m_bytecode.end(), other.m_bytecode.begin(), other.m_bytecode.end());
	m_line_info.insert(m_line_info.end(), other.m_line_info.begin(), other.m_line_info.end());
}

const OpCode* BytecodeStream::operator[](int index) const
{
	return &m_bytecode[static_cast<size_t>(index)];
}

int MidoriExecutable::AddGlobalVariable(MidoriText&& name)
{
	m_globals.emplace_back(std::move(name));
	return static_cast<int>(m_globals.size()) - 1;
}

const MidoriText& MidoriExecutable::GetGlobalVariable(int index) const
{
	return m_globals[static_cast<size_t>(index)];
}

void MidoriExecutable::AttachProcedures(Procedures&& bytecode)
{
	m_procedures = std::move(bytecode);
}

void MidoriExecutable::AddStringPool(StringPool&& string_pool)
{
	m_string_pool = std::move(string_pool);
}

void MidoriExecutable::AttachProcedureNames(std::vector<MidoriText>&& procedure_names)
{
		m_procedure_names = std::move(procedure_names);
}

void MidoriExecutable::SetFileName(std::string&& file_name)
{
	m_file_name = std::move(file_name);
}

std::string_view MidoriExecutable::GetFileName() const
{
	return m_file_name;
}

void MidoriExecutable::SetHasAsync(bool has_async)
{
	m_has_async = has_async;
	m_execution_mode = has_async ? ExecutionMode::AsyncEnabled : ExecutionMode::SyncOnly;
}

bool MidoriExecutable::HasAsync() const
{
	return m_has_async;
}

void MidoriExecutable::SetExecutionMode(ExecutionMode execution_mode)
{
	m_execution_mode = execution_mode;
	m_has_async = execution_mode == ExecutionMode::AsyncEnabled;
}

ExecutionMode MidoriExecutable::GetExecutionMode() const
{
	return m_execution_mode;
}

int MidoriExecutable::GetLine(int instr_index, int proc_index) const
{
	return m_procedures[static_cast<size_t>(proc_index)].GetLine(instr_index);
}

const BytecodeStream& MidoriExecutable::GetBytecodeStream(int proc_index) const
{
	return m_procedures[static_cast<size_t>(proc_index)];
}

OpCode MidoriExecutable::ReadByteCode(int instr_index, int proc_index) const
{
	return m_procedures[static_cast<size_t>(proc_index)].ReadByteCode(instr_index);
}

int MidoriExecutable::GetByteCodeSize(int proc_index) const
{
	return m_procedures[static_cast<size_t>(proc_index)].GetByteCodeSize();
}

int MidoriExecutable::GetProcedureCount() const
{
	return static_cast<int>(m_procedures.size());
}

int MidoriExecutable::GetGlobalVariableCount() const
{
	return static_cast<int>(m_globals.size());
}

const MidoriExecutable::StringPool& MidoriExecutable::GetStringPool() const
{
	return m_string_pool;
}
