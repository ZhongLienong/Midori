#pragma once

#include "Common/Error/Error.h"
#include "Common/Executable/Executable.h"
#include "Interpreter/GarbageCollector/GarbageCollector.h"

// handle std library
#ifdef WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

class VirtualMachine
{
public:
    VirtualMachine(MidoriExecutable&& executable) noexcept;
    ~VirtualMachine();

private:
    static constexpr size_t s_value_stack_size = 10000u;
    static constexpr size_t s_call_stack_size = 10000u;
    static constexpr size_t s_garbage_collection_threshold = 512000u;

    using ValueStackPointer = MidoriValue*;
    using InstructionPointer = const OpCode*;
    using GlobalVariables = std::vector<MidoriValue>;

    using CallFrame = std::tuple<ValueStackPointer, ValueStackPointer, InstructionPointer, MidoriArray*>;
    using CallStackPointer = CallFrame*;

    MidoriExecutable m_executable;
    GlobalVariables m_global_vars;
    std::vector<MidoriCellValue*> m_cells_to_promote;
    std::vector<std::string> m_string_pool;
    GarbageCollector m_garbage_collector;

    MidoriArray* m_curr_environment = nullptr;
    InstructionPointer m_instruction_pointer = nullptr;
    ValueStackPointer m_value_stack_base_pointer = nullptr;
    ValueStackPointer m_value_stack_pointer = nullptr;
    ValueStackPointer m_value_stack_begin = nullptr;
    CallStackPointer m_call_stack_pointer = nullptr;
    CallStackPointer m_call_stack_begin = nullptr;

#ifdef WIN32
    HMODULE m_library_handle = nullptr;
#else
    void* m_library_handle = nullptr;
#endif

public:
    int Execute() noexcept;

private:

	int TerminateExecution(std::string_view message) noexcept;

	// only used for error reporting, efficiency is not a concern
	int GetLine() noexcept;

	OpCode ReadByte() noexcept;

	int ReadShort() noexcept;

	int ReadThreeBytes() noexcept;

	MidoriInteger ReadIntegerConstant() noexcept;

	MidoriFraction ReadFractionConstant() noexcept;

	MidoriValue ReadConstant(OpCode operand_length) noexcept;

	int ReadGlobalVariable() noexcept;

	std::string GenerateRuntimeError(std::string_view message, int line) noexcept;

    void PushCallFrame(ValueStackPointer return_bp, ValueStackPointer return_sp, InstructionPointer return_ip, MidoriArray* closure_ptr) noexcept;

    MidoriValue& Peek() noexcept;

    MidoriValue Pop() noexcept;

	void PromoteCells() noexcept;

	int CheckIndexBounds(const MidoriValue index, MidoriInteger size) noexcept;

	int CheckNewArraySize(MidoriInteger size) noexcept;

	int CheckArrayPopResult(const std::optional<MidoriValue>& result) noexcept;

	MidoriTraceable::GarbageCollectionRoots GetValueStackGarbageCollectionRoots() const noexcept;

	MidoriTraceable::GarbageCollectionRoots GetGlobalTableGarbageCollectionRoots() const noexcept;

	MidoriTraceable::GarbageCollectionRoots GetGarbageCollectionRoots() const noexcept;

	void CollectGarbage() noexcept;

	template<typename T>
        requires MidoriValueConstructible<T>
    void Push(T val) noexcept
    {
        *m_value_stack_pointer = val;
        ++m_value_stack_pointer;
	}
};
