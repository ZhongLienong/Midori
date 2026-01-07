#pragma once

#include <atomic>
#include <cassert>
#include <functional>
#include <list>
#include <optional>
#include <thread>
#include <unordered_set>
#include <variant>

#include "Common/BuildConfig/BuildConfig.h"

// Alignment must be power of 2 (8 bytes = 3 tag bits)
constexpr uintptr_t ALIGNMENT = 0b1000;
constexpr uintptr_t ALIGNMENT_MASK = ALIGNMENT - 1;
constexpr uintptr_t TAG_MASK = ~ALIGNMENT_MASK;
enum PointerTag : uint8_t
{
	TEXT = 0b000,
	ARRAY = 0b001,
	STRUCT = 0b010,
	UNION = 0b100,
	CELL = 0b011,
	FUNCTION = 0b110,
	RANGE = 0b101,
	FUTURE = 0b111,
};

class MidoriTraceable;
class MidoriText;

using MidoriInteger = int64_t;
using MidoriFloat = double;
using MidoriByte = uint8_t;
using MidoriWord = uint64_t;
using MidoriUnit = std::monostate;
using MidoriBool = bool;

class MidoriTaggedPointer 
{
private:
	uintptr_t m_raw_pointer;

	static uintptr_t Encode(MidoriTraceable* ptr, PointerTag tag)
	{
		assert((reinterpret_cast<uintptr_t>(ptr) & ALIGNMENT_MASK) == 0 && "Pointer not properly aligned");
		return reinterpret_cast<uintptr_t>(ptr) | static_cast<uintptr_t>(tag);
	}

	static MidoriTraceable* Decode(uintptr_t value)
	{
		return reinterpret_cast<MidoriTraceable*>(value & TAG_MASK);
	}

public:
	MidoriTaggedPointer(MidoriTraceable* ptr, PointerTag tag): m_raw_pointer(Encode(ptr, tag)) {}

	MidoriTraceable* operator->() const
	{
		return Decode(m_raw_pointer);
	}

	explicit operator MidoriTraceable* () const
	{
		return Decode(m_raw_pointer);
	}
};

class MidoriValue
{
public:
	constexpr static inline int DATA_BUFFER_SIZE = sizeof(double);
private:
	union
	{
		MidoriFloat m_float;
		MidoriInteger m_integer;
		MidoriBool m_bool;
		MidoriTraceable* m_pointer;
	} m_data;
#if MIDORI_DEBUG_INFO
	enum DebugTypeTag : int64_t
	{
		FLOAT = 0,
		INT,
		BYTE,
		WORD,
		BOOL,
		POINTER,
		UNIT,
		UNKNOWN,
	};

	DebugTypeTag m_tag;
#endif

public:
	MidoriValue() noexcept;

	MidoriValue(MidoriFloat d) noexcept;

	MidoriValue(MidoriInteger l) noexcept;

	MidoriValue(MidoriByte byte) noexcept;

	MidoriValue(MidoriWord word) noexcept;

	MidoriValue(MidoriBool b) noexcept;

	MidoriValue(MidoriTraceable* o) noexcept;

	MidoriValue(const MidoriValue& other) noexcept = default;

	MidoriValue(MidoriValue&& other) noexcept = default;

	MidoriValue& operator=(const MidoriValue& other) noexcept = default;

	MidoriValue& operator=(MidoriValue&& other) noexcept = default;

	MidoriFloat GetFloat() const noexcept;

	MidoriInteger GetInteger() const noexcept;

	MidoriByte GetByte() const noexcept;

	MidoriWord GetWord() const noexcept;

	MidoriUnit GetUnit() const noexcept;

	MidoriBool GetBool() const noexcept;

	MidoriTraceable* GetPointer() const noexcept;

#if MIDORI_DEBUG_INFO
	MidoriText ToText() const;

	bool IsPointer() const noexcept
	{
		return m_tag == POINTER;
	}

	DebugTypeTag GetTag() const noexcept
	{
		return m_tag;
	}
#endif
};

template<typename... Args>
concept MidoriValueConstructible = std::constructible_from<MidoriValue, Args...>;

template<typename T>
concept MidoriTraceableConstructible = std::constructible_from<MidoriTraceable, T>;

template <typename T>
concept MidoriNumeric = std::same_as<T, MidoriFloat> || std::same_as<T, MidoriInteger> || std::same_as<T, MidoriByte> || std::same_as<T, MidoriWord>;

class MidoriText
{
private:
	static constexpr int INLINE_THRESHOLD = 15;

	char m_small_buffer[INLINE_THRESHOLD + 1];
	char* m_data{ nullptr };
	int m_size{ 0 };
	int m_capacity{ 0 };
	mutable int m_length_cache{ -1 };

public:
	MidoriText();

	MidoriText(const char* str);

	MidoriText(const MidoriText& other);

	MidoriText(MidoriText&& other) noexcept;

	MidoriText& operator=(const MidoriText& other);

	MidoriText& operator=(MidoriText&& other) noexcept;

	~MidoriText();

	int GetLength() const noexcept;

	int GetByteLength() const noexcept;

	const char* GetCString() const noexcept;

	MidoriText& Pop();

	MidoriText& Append(const char* str);

	MidoriText& Append(char c);

	MidoriText& Append(const MidoriText& other);

	void Reserve(int capacity);

	MidoriText& Prepend(const char* str);

	MidoriText& Prepend(char c);

	MidoriText& Prepend(const MidoriText& other);

	char operator[](int index) const;

	bool operator==(const MidoriText& other) const;

	bool operator!=(const MidoriText& other) const;

	MidoriInteger ToInteger() const;

	MidoriFloat ToFloat() const;

	static MidoriText FromInteger(MidoriInteger value);

	static MidoriText FromFloat(MidoriFloat value);

	static MidoriText Concatenate(const MidoriText& a, const MidoriText& b);

	static MidoriText FromFFI(char* ffi_allocated_string);

private:
	void Expand(int new_size);

	bool IsInlined() const noexcept;
};

class MidoriArray
{
private:
	inline static constexpr int s_initial_capacity = 8;
	MidoriValue* m_data{ nullptr };
	int m_capacity{ 0 };
	int m_start{ 0 };
	int m_length{ 0 };

public:
	MidoriArray() = default;

	MidoriArray(int size);

	MidoriArray(const MidoriArray& other);

	MidoriArray(MidoriArray&& other) noexcept;

	MidoriArray& operator=(const MidoriArray& other);

	MidoriArray& operator=(MidoriArray&& other) noexcept;

	~MidoriArray();

	MidoriValue& operator[](int index);

	void AddBack(const MidoriValue& value);

	void AddFront(const MidoriValue& value);

	std::optional<MidoriValue> Pop();

	int GetLength() const;

	static MidoriArray Concatenate(const MidoriArray& a, const MidoriArray& b);

	static MidoriArray FromFFI(MidoriValue* ffi_allocated_data, int length);

private:
	void Expand();

	void Shrink();
};

class MidoriRange
{
private:
	MidoriValue m_start;
	MidoriValue m_end;
	MidoriValue m_step;
	bool m_is_float;

public:
	MidoriRange() = default;

	MidoriRange(MidoriValue start, MidoriValue end, MidoriValue step, bool is_float);

	MidoriValue GetStart() const;

	MidoriValue GetEnd() const;

	MidoriValue GetStep() const;

	bool IsFloat() const;
};

struct MidoriCellValue
{
	MidoriValue m_data;
	bool m_is_on_heap;

	MidoriCellValue(MidoriValue heap_value) noexcept;

	MidoriCellValue(MidoriValue* stack_ref) noexcept;

	MidoriValue& GetValue();

	MidoriValue* GetStackPointer();
};

struct MidoriClosure
{
	MidoriArray m_cell_values;
	int m_proc_index;
};

struct MidoriStruct
{
	MidoriArray m_values{};
};

struct MidoriUnion
{
	MidoriArray m_values{};
	int m_index{ 0 };
};

struct MidoriFuture
{
	MidoriClosure m_closure;
	MidoriValue m_result;
	std::atomic<bool> m_completed{false};
	std::atomic<bool> m_has_error{false};

	MidoriFuture(MidoriClosure&& closure);

	MidoriFuture(const MidoriFuture&) = delete;
	MidoriFuture& operator=(const MidoriFuture&) = delete;

	MidoriFuture(MidoriFuture&& other) noexcept;
	MidoriFuture& operator=(MidoriFuture&& other) noexcept;

	void SetResult(MidoriValue value);
	void SetError();
	MidoriValue Get();
	bool IsReady() const;
};

class MidoriTraceable
{
private:
	std::variant<MidoriText, MidoriArray, MidoriRange, MidoriStruct, MidoriUnion, MidoriCellValue, MidoriClosure, MidoriFuture> m_value;
	size_t m_size;
	bool m_is_marked = false;

public:

	template<typename T>
	constexpr bool IsTraceable()
	{
		return std::holds_alternative<T>(m_value);
	}

	template<typename T>
	constexpr T& GetTraceable()
	{
		return std::get<T>(m_value);
	}

	size_t GetSize() const;

	void Mark();

	void Unmark();

	bool IsMarked() const;

#if MIDORI_DEBUG_INFO
	MidoriText ToText();
#endif

	static void operator delete(void* object, size_t size) noexcept;

	static void* operator new(size_t size) noexcept;

private:
	MidoriTraceable() = delete;

	MidoriTraceable(const MidoriTraceable& other) = delete;

	MidoriTraceable(MidoriTraceable&& other) noexcept = delete;

	MidoriTraceable& operator=(const MidoriTraceable& other) = delete;

	MidoriTraceable& operator=(MidoriTraceable&& other) noexcept = delete;

	MidoriTraceable(MidoriText&& str) noexcept;

	MidoriTraceable(MidoriArray&& array) noexcept;

	MidoriTraceable(MidoriRange&& range) noexcept;

	MidoriTraceable(MidoriCellValue&& cell_value) noexcept;

	MidoriTraceable(MidoriClosure&& closure) noexcept;

	MidoriTraceable(MidoriStruct&& midori_struct) noexcept;

	MidoriTraceable(MidoriUnion&& midori_union) noexcept;

	MidoriTraceable(MidoriFuture&& midori_future) noexcept;

	friend class GarbageCollector;
};