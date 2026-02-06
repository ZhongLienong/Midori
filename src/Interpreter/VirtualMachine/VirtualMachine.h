#pragma once

#include "Common/Error/Error.h"
#include "Common/Executable/Executable.h"
#include "Interpreter/Allocator/MidoriAllocator.h"
#include "Interpreter/GarbageCollector/GarbageCollector.h"
#include "Library/MidoriBuiltinFFIRegistry/MidoriFFIRegistry.h"

#include <array>
#include <bit>
#include <cstdint>
#include <cstring>
#include <memory>
#include <unordered_map>
#include <vector>

class MidoriRuntime;

class VirtualMachine
{
public:
    using GlobalVariables = std::vector<MidoriValue>;

    VirtualMachine(MidoriRuntime& runtime) noexcept;

    VirtualMachine(MidoriRuntime& runtime, const MidoriClosure& entry_closure) noexcept;

    ~VirtualMachine();

    MidoriValue GetAsyncResult() const noexcept;

    GarbageCollector::GarbageCollectionRoots GetGarbageCollectionRoots() const noexcept;

    template<typename T>
    MIDORI_FORCE_INLINE MidoriTraceable* AllocateTraceable(T&& arg)
    {
        void* mem = m_allocator.Allocate(sizeof(MidoriTraceable));
        MidoriTraceable* traceable = new(mem) MidoriTraceable(std::forward<T>(arg));
        m_gc.RegisterObject(traceable);
        return traceable;
    }

private:
    static constexpr size_t s_value_stack_size = 10000u;
    static constexpr size_t s_call_stack_size = 10000u;
    static constexpr int s_max_stack_trace_depth = 20;

    using ValueStackPointer = MidoriValue*;
    using InstructionPointer = const OpCode*;

    struct CallFrame
	{
        ValueStackPointer return_bp;
        InstructionPointer return_ip;
        MidoriTuple* closure_ptr;
    };
    using CallStackPointer = CallFrame*;

    struct FFIArrayArgument
    {
        void* data;
        int length;
    };

    // Hot Pointers
    InstructionPointer m_instruction_pointer = nullptr;
    ValueStackPointer m_value_stack_pointer = nullptr;
    ValueStackPointer m_value_stack_base_pointer = nullptr;
    ValueStackPointer m_value_stack_begin = nullptr;
    CallStackPointer m_call_stack_pointer = nullptr;
    CallStackPointer m_call_stack_begin = nullptr;
    MidoriTraceable* m_curr_closure_traceable = nullptr;
    MidoriTuple* m_curr_environment = nullptr;

    // Infrastructure & Cold Data
    MidoriAllocator m_allocator;
    GarbageCollector m_gc;
    GarbageCollector::GarbageCollectionRoots m_gc_roots_scratch;

    MidoriRuntime* m_runtime = nullptr;
    const MidoriExecutable* m_executable = nullptr;
    GlobalVariables* m_global_vars = nullptr;
    std::vector<MidoriCellValue*> m_cells_to_promote;

    MidoriValue m_async_result;
    std::array<FFIFunction, MidoriFFIRegistry::BUILTIN_COUNT> m_ffi_table{};
    std::array<void*, UINT8_MAX> m_ffi_args{};
    std::vector<FFIArrayArgument> m_ffi_array_args;
    std::vector<InstructionPointer> m_proc_entry_cache;
    std::vector<MidoriTraceable*> m_string_literal_cache;
    std::unordered_map<std::string_view, MidoriTraceable*> m_small_string_pool;
    std::unordered_map<int, MidoriTraceable*> m_static_closure_cache;

#ifdef _WIN32
    void* m_value_stack_region = nullptr;
    void* m_call_stack_region = nullptr;
#endif


public:
    int Execute() noexcept;

    const GarbageCollector& GetGC() const noexcept { return m_gc; }

    MidoriTraceable* InternSmallString(const MidoriText& text) noexcept;

private:
	MIDORI_FORCE_INLINE void TryCollect() noexcept
	{
		if (m_gc.ShouldCollect())
		{
			BuildGarbageCollectionRoots(m_gc_roots_scratch);
			m_gc.ReclaimMemory(m_gc_roots_scratch, m_allocator);
		}
	}

	int ExecuteLoop() noexcept;

	int TerminateExecution(std::string_view message) noexcept;

	int GetLine() noexcept;

	MIDORI_FORCE_INLINE OpCode ReadByte() noexcept
	{
		return *m_instruction_pointer++;
	}

#if defined(MIDORI_LITTLE_ENDIAN)
	MIDORI_FORCE_INLINE int ReadShort() noexcept
	{
		const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
		const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
		int value = static_cast<int>(static_cast<uint16_t>(b0) | (static_cast<uint16_t>(b1) << 8));
		m_instruction_pointer += 2;
		return value;
	}

	MIDORI_FORCE_INLINE int ReadThreeBytes() noexcept
	{
		const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
		const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
		const uint8_t b2 = static_cast<uint8_t>(m_instruction_pointer[2u]);
		int value = static_cast<int>(static_cast<uint32_t>(b0) | (static_cast<uint32_t>(b1) << 8) | (static_cast<uint32_t>(b2) << 16));
		m_instruction_pointer += 3;
		return value;
	}
#elif defined(MIDORI_BIG_ENDIAN)
	MIDORI_FORCE_INLINE int ReadShort() noexcept
	{
		const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
		const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
		int value = static_cast<int>((static_cast<uint16_t>(b0) << 8) | static_cast<uint16_t>(b1));
		m_instruction_pointer += 2;
		return value;
	}

	MIDORI_FORCE_INLINE int ReadThreeBytes() noexcept
	{
		const uint8_t b0 = static_cast<uint8_t>(m_instruction_pointer[0u]);
		const uint8_t b1 = static_cast<uint8_t>(m_instruction_pointer[1u]);
		const uint8_t b2 = static_cast<uint8_t>(m_instruction_pointer[2u]);
		int value = static_cast<int>((static_cast<uint32_t>(b0) << 16) | (static_cast<uint32_t>(b1) << 8) | static_cast<uint32_t>(b2));
		m_instruction_pointer += 3;
		return value;
	}
#endif

	MIDORI_FORCE_INLINE MidoriInteger ReadIntegerConstant() noexcept
	{
		uint64_t bits = 0u;
		std::memcpy(&bits, m_instruction_pointer, sizeof(bits));
#if defined(MIDORI_BIG_ENDIAN)
		bits = std::byteswap(bits);
#endif
		m_instruction_pointer += sizeof(MidoriInteger);
		return static_cast<MidoriInteger>(bits);
	}

	MIDORI_FORCE_INLINE MidoriFloat ReadFloatConstant() noexcept
	{
		uint64_t bits = 0u;
		std::memcpy(&bits, m_instruction_pointer, sizeof(bits));
#if defined(MIDORI_BIG_ENDIAN)
		bits = std::byteswap(bits);
#endif
		m_instruction_pointer += sizeof(MidoriFloat);
		return std::bit_cast<MidoriFloat>(bits);
	}

	MIDORI_FORCE_INLINE MidoriByte ReadByteConstant() noexcept
	{
		MidoriByte value = static_cast<MidoriByte>(*m_instruction_pointer);
		m_instruction_pointer += sizeof(MidoriByte);
		return value;
	}

	MIDORI_FORCE_INLINE MidoriWord ReadWordConstant() noexcept
	{
		uint64_t bits = 0u;
		std::memcpy(&bits, m_instruction_pointer, sizeof(bits));
#if defined(MIDORI_BIG_ENDIAN)
		bits = std::byteswap(bits);
#endif
		m_instruction_pointer += sizeof(MidoriWord);
		return bits;
	}

	MIDORI_FORCE_INLINE int ReadGlobalVariable() noexcept
	{
		return static_cast<int>(ReadByte());
	}

	std::string GenerateRuntimeError(std::string_view message, int line) noexcept;

	std::string GenerateStackTrace() noexcept;

	int GetProcedureIndexFromIP(InstructionPointer ip) noexcept;

	int GetLineFromIP(InstructionPointer ip, int proc_index) noexcept;

	MIDORI_FORCE_INLINE InstructionPointer GetProcEntry(int proc_index) const noexcept
	{
		return m_proc_entry_cache[static_cast<size_t>(proc_index)];
	}

	MIDORI_FORCE_INLINE void PushCallFrame(ValueStackPointer return_bp, InstructionPointer return_ip, MidoriTuple* closure_ptr) noexcept
	{
		*m_call_stack_pointer = CallFrame{return_bp, return_ip, closure_ptr};
		++m_call_stack_pointer;
	}

    MIDORI_FORCE_INLINE MidoriValue& Peek() noexcept
    {
        return *(m_value_stack_pointer - 1);
    }

    MIDORI_FORCE_INLINE MidoriValue Pop() noexcept
    {
        return *(--m_value_stack_pointer);
    }

	void PromoteCells() noexcept;

	int CheckIndexBounds(const MidoriValue index, MidoriInteger size) noexcept;

	int CheckNewArraySize(MidoriInteger size) noexcept;

	int CheckArrayPopResult(const std::optional<MidoriValue>& result) noexcept;

    void BuildGarbageCollectionRoots(GarbageCollector::GarbageCollectionRoots& roots) const noexcept;

	void InitializeStacks() noexcept;

	void InitializeProcEntryCache() noexcept;

	template<typename T>
        requires MidoriValueConstructible<T>
	MIDORI_FORCE_INLINE void Push(T val) noexcept
    {
        *m_value_stack_pointer = val;
        ++m_value_stack_pointer;
	}
};
