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

class MidoriTraceable;
class MidoriText;

using MidoriInteger = int64_t;
using MidoriFloat = double;
using MidoriByte = uint8_t;
using MidoriWord = uint64_t;
using MidoriUnit = std::monostate;
using MidoriBool = bool;

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
#if MIDORI_DEBUG_FULL
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

	const void* GetRawDataPtr() const noexcept;

#if MIDORI_DEBUG_FULL
	MidoriText ToText() const;

	bool IsPointer() const noexcept;

	DebugTypeTag GetTag() const noexcept;
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
	static constexpr int SSO_CAPACITY = 22;

	struct LongLayout
	{
		char* m_ptr;
		int m_size;
		int m_capacity;
		mutable int m_length_cache;
		uint8_t m_padding[sizeof(void*) == 8 ? 3 : 7];
		uint8_t m_flag;
	};

	struct ShortLayout
	{
		char m_buffer[23];
		uint8_t m_size_flag;
	};

	union
	{
		LongLayout m_long;
		ShortLayout m_short;
	};

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

	size_t GetCapacity() const;

private:
	void Expand(int new_size);

	bool IsShort() const noexcept;

	void SetShortSize(int size);

	int GetShortSize() const;
};

class MidoriArray
{
private:
	static constexpr int s_initial_capacity = 8;
	static constexpr int SOO_CAPACITY = 2;

	struct LongLayout
	{
		MidoriValue* m_ptr;
		int m_size;
		int m_capacity;
		uint8_t m_flag;
	};

	struct ShortLayout
	{
		MidoriValue m_buffer[SOO_CAPACITY];
		uint8_t m_size_flag;
	};

	union
	{
		LongLayout m_long;
		ShortLayout m_short;
	};

public:
	MidoriArray();

	MidoriArray(int size);

	MidoriArray(const MidoriArray& other);

	MidoriArray(MidoriArray&& other) noexcept;

	MidoriArray& operator=(const MidoriArray& other);

	MidoriArray& operator=(MidoriArray&& other) noexcept;

	~MidoriArray();

	MidoriValue& operator[](int index);

	const MidoriValue& operator[](int index) const;

	void AddBack(const MidoriValue& value);

	void AddFront(const MidoriValue& value);

	void Extend(const MidoriArray& other);

	std::optional<MidoriValue> Pop();

	int GetLength() const;

	size_t GetCapacity() const;

	static MidoriArray Concatenate(const MidoriArray& a, const MidoriArray& b);

	static MidoriArray FromFFI(MidoriValue* ffi_allocated_data, int length);

private:
	void Expand(int new_capacity);

	bool IsShort() const noexcept;

	void SetShortSize(int size);

	int GetShortSize() const;
};

class MidoriTuple
{
private:
	static constexpr int SOO_CAPACITY = 2;

	struct LongLayout
	{
		MidoriValue* m_ptr;
		int m_size;
		int m_capacity;
		uint8_t m_flag;
	};

	struct ShortLayout
	{
		MidoriValue m_buffer[SOO_CAPACITY];
		uint8_t m_size_flag;
	};

	union
	{
		LongLayout m_long;
		ShortLayout m_short;
	};

public:
	MidoriTuple();

	MidoriTuple(int size);

	MidoriTuple(const MidoriTuple& other);

	MidoriTuple(MidoriTuple&& other) noexcept;

	MidoriTuple& operator=(const MidoriTuple& other);

	MidoriTuple& operator=(MidoriTuple&& other) noexcept;

	~MidoriTuple();

	MidoriValue& operator[](int index);

	const MidoriValue& operator[](int index) const;

	int GetLength() const;

	size_t GetCapacity() const;

private:
	bool IsShort() const noexcept;

	void SetShortSize(int size);

	int GetShortSize() const;
};

class MidoriIntRange
{
private:
	MidoriInteger m_start;
	MidoriInteger m_end;
	MidoriInteger m_step;

public:
	MidoriIntRange() = default;

	MidoriIntRange(MidoriInteger start, MidoriInteger end, MidoriInteger step);

	MidoriInteger GetStart() const;

	MidoriInteger GetEnd() const;

	MidoriInteger GetStep() const;
};

class MidoriFloatRange
{
private:
	MidoriFloat m_start;
	MidoriFloat m_end;
	MidoriFloat m_step;

public:
	MidoriFloatRange() = default;

	MidoriFloatRange(MidoriFloat start, MidoriFloat end, MidoriFloat step);

	MidoriFloat GetStart() const;

	MidoriFloat GetEnd() const;

	MidoriFloat GetStep() const;
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
	MidoriTuple m_cell_values;
	int m_proc_index;
};

struct MidoriStruct
{
	MidoriTuple m_values{};
};

struct MidoriUnion
{
	MidoriTuple m_values{};
	int m_index{ 0 };
};

struct MidoriFuture
{
	MidoriValue m_result;
	std::unique_ptr<MidoriClosure> m_closure;
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
public:
	enum class TraceableType : uint8_t
	{
		Text,
		Array,
		IntRange,
		FloatRange,
		Struct,
		Union,
		Cell,
		Closure,
		Future
	};

private:
	union
	{
		MidoriText m_text;
		MidoriArray m_array;
		MidoriIntRange m_int_range;
		MidoriFloatRange m_float_range;
		MidoriStruct m_struct;
		MidoriUnion m_union;
		MidoriCellValue m_cell;
		MidoriClosure m_closure;
		MidoriFuture m_future;
	};
	TraceableType m_type;
	bool m_is_marked = false;

	template<typename T>
	static constexpr TraceableType TypeToEnum()
	{
		if constexpr (std::is_same_v<T, MidoriText>)
		{
			return TraceableType::Text;
		}
		else if constexpr (std::is_same_v<T, MidoriArray>)
		{
			return TraceableType::Array;
		}
		else if constexpr (std::is_same_v<T, MidoriIntRange>)
		{
			return TraceableType::IntRange;
		}
		else if constexpr (std::is_same_v<T, MidoriFloatRange>)
		{
			return TraceableType::FloatRange;
		}
		else if constexpr (std::is_same_v<T, MidoriStruct>)
		{
			return TraceableType::Struct;
		}
		else if constexpr (std::is_same_v<T, MidoriUnion>)
		{
			return TraceableType::Union;
		}
		else if constexpr (std::is_same_v<T, MidoriCellValue>)
		{
			return TraceableType::Cell;
		}
		else if constexpr (std::is_same_v<T, MidoriClosure>)
		{
			return TraceableType::Closure;
		}
		else if constexpr (std::is_same_v<T, MidoriFuture>)
		{
			return TraceableType::Future;
		}
		else
		{
			static_assert(std::is_same_v<T, void>, "Invalid type for MidoriTraceable");
		}
	}

public:

	template<typename T>
	constexpr bool IsTraceable()
	{
		return m_type == TypeToEnum<T>();
	}

	template<typename T>
	constexpr T& GetTraceable()
	{
		if constexpr (std::is_same_v<T, MidoriText>)
		{
			return m_text;
		}
		else if constexpr (std::is_same_v<T, MidoriArray>)
		{
			return m_array;
		}
		else if constexpr (std::is_same_v<T, MidoriIntRange>)
		{
			return m_int_range;
		}
		else if constexpr (std::is_same_v<T, MidoriFloatRange>)
		{
			return m_float_range;
		}
		else if constexpr (std::is_same_v<T, MidoriStruct>)
		{
			return m_struct;
		}
		else if constexpr (std::is_same_v<T, MidoriUnion>)
		{
			return m_union;
		}
		else if constexpr (std::is_same_v<T, MidoriCellValue>)
		{
			return m_cell;
		}
		else if constexpr (std::is_same_v<T, MidoriClosure>)
		{
			return m_closure;
		}
		else if constexpr (std::is_same_v<T, MidoriFuture>)
		{
			return m_future;
		}
	}

	size_t GetSize() const;

	void Mark();
	void Unmark();
	bool IsMarked() const;

#if MIDORI_DEBUG_FULL
	MidoriText ToText();
#endif

	~MidoriTraceable();

	static void operator delete(void* object, size_t size) noexcept;
	static void operator delete(void* object, std::align_val_t al) noexcept;

	static void* operator new(size_t size) noexcept;
	static void* operator new(size_t size, std::align_val_t al) noexcept;
	static void* operator new(size_t, void* ptr) noexcept;
	static void* operator new(size_t, std::align_val_t, void* ptr) noexcept; 
	static void operator delete(void*, void*) noexcept;
	static void operator delete(void*, std::align_val_t, void*) noexcept;

	MidoriTraceable(MidoriText&& str) noexcept;
	MidoriTraceable(MidoriArray&& array) noexcept;
	MidoriTraceable(MidoriIntRange&& range) noexcept;
	MidoriTraceable(MidoriFloatRange&& range) noexcept;
	MidoriTraceable(MidoriCellValue&& cell_value) noexcept;
	MidoriTraceable(MidoriClosure&& closure) noexcept;
	MidoriTraceable(MidoriStruct&& midori_struct) noexcept;
	MidoriTraceable(MidoriUnion&& midori_union) noexcept;
	MidoriTraceable(MidoriFuture&& midori_future) noexcept;

private:
	MidoriTraceable() = delete;
	MidoriTraceable(const MidoriTraceable& other) = delete;
	MidoriTraceable(MidoriTraceable&& other) noexcept = delete;
	MidoriTraceable& operator=(const MidoriTraceable& other) = delete;
	MidoriTraceable& operator=(MidoriTraceable&& other) noexcept = delete;
};
