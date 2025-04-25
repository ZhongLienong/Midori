#pragma once

#include <cassert>
#include <functional>
#include <list>
#include <optional>
#include <unordered_set>
#include <variant>

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
	BOX = 0b101, 
};

class MidoriTraceable;
class MidoriText;

using MidoriInteger = int64_t;
using MidoriFloat = double;
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
	constexpr static inline int DATA_BUFFER_SIZE = 8;
private:
	alignas(DATA_BUFFER_SIZE) std::byte m_data[DATA_BUFFER_SIZE];
#ifdef DEBUG
	enum DebugTypeTag : int64_t
	{
		FLOAT = 0,
		INT,
		BOOL,
		POINTER,
		UNKNOWN,
	};

	DebugTypeTag m_tag;
#endif

public:
	MidoriValue() = default;

	MidoriValue(MidoriFloat d) noexcept;

	MidoriValue(MidoriInteger l) noexcept;

	MidoriValue(MidoriBool b) noexcept;

	MidoriValue(MidoriTraceable* o) noexcept;

	MidoriValue(const MidoriValue& other) noexcept = default;

	MidoriValue(MidoriValue&& other) noexcept = default;

	MidoriValue& operator=(const MidoriValue& other) noexcept = default;

	MidoriValue& operator=(MidoriValue&& other) noexcept = default;

	MidoriFloat GetFloat() const noexcept;

	MidoriInteger GetInteger() const noexcept;

	MidoriUnit GetUnit() const noexcept;

	MidoriBool GetBool() const noexcept;

	MidoriTraceable* GetPointer() const noexcept;

#ifdef DEBUG
	MidoriText ToText() const;
#endif
};

template<typename... Args>
concept MidoriValueConstructible = std::constructible_from<MidoriValue, Args...>;

template<typename T>
concept MidoriTraceableConstructible = std::constructible_from<MidoriTraceable, T>;

template <typename T>
concept MidoriNumeric = std::same_as<T, MidoriFloat> || std::same_as<T, MidoriInteger>;

class MidoriText
{
private:
	static constexpr int INLINE_THRESHOLD = 15;

	char m_small_buffer[INLINE_THRESHOLD + 1];
	char* m_data{ nullptr };
	int m_size{ 0 };
	int m_capacity{ 0 };

public:
	MidoriText();

	MidoriText(const char* str);

	MidoriText(const MidoriText& other);

	MidoriText(MidoriText&& other) noexcept;

	MidoriText& operator=(const MidoriText& other);

	MidoriText& operator=(MidoriText&& other) noexcept;

	~MidoriText();

	int GetLength() const noexcept;

	const char* GetCString() const noexcept;

	MidoriText& Pop();

	MidoriText& Append(const char* str);

	MidoriText& Append(char c);

	MidoriText& Append(const MidoriText& other);

	char operator[](int index) const;

	bool operator==(const MidoriText& other) const;

	bool operator!=(const MidoriText& other) const;

	MidoriInteger ToInteger() const;

	MidoriFloat ToFloat() const;

	static MidoriText FromInteger(MidoriInteger value);

	static MidoriText FromFloat(MidoriFloat value);

	static MidoriText Concatenate(const MidoriText& a, const MidoriText& b);

private:
	void Expand(int new_size);

	bool IsInlined() const noexcept;
};

class MidoriArray
{
private:
	inline static constexpr int s_initial_capacity = 8;
	MidoriValue* m_data{ nullptr };
	int m_size{ 0 };
	int m_end{ 0 };

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

private:
	void Expand();

	void Shrink();
};

struct MidoriCellValue
{
	union
	{
		MidoriValue m_heap_value;
		MidoriValue* m_stack_value_ref;
	} m_value;
	bool m_is_on_heap;

	MidoriCellValue(MidoriValue heap_value) noexcept;

	MidoriCellValue(MidoriValue* stack_ref) noexcept;


	MidoriValue& GetValue();
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

struct MidoriBox
{
	enum TypeTag : int64_t
	{
		FLOAT = 0,
		INT,
		BOOL,
		UNIT,
		POINTER,
	};

	MidoriValue m_inner_value{};
	TypeTag m_tag;
};

class MidoriTraceable
{
public:
	// Garbage collection utilities
	using GarbageCollectionRoots = std::unordered_set<MidoriTraceable*>;
	static inline size_t s_total_bytes_allocated;
	static inline size_t s_static_bytes_allocated;
	static inline std::unordered_set<MidoriTraceable*> s_traceables;

private:
	std::variant<MidoriText, MidoriArray, MidoriStruct, MidoriUnion, MidoriCellValue, MidoriClosure, MidoriBox> m_value;
	size_t m_size;
	bool m_is_marked = false;

public:

	~MidoriTraceable() = default;

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

	bool Marked() const;

	static void operator delete(void* object, size_t size) noexcept;

	MidoriText ToText();

	void Trace();

	std::optional<MidoriBox> UnBox();

	template<typename T>
	static MidoriTraceable* AllocateTraceable(T&& arg, PointerTag tag)
	{
		MidoriTaggedPointer tagged_pointer(new MidoriTraceable(std::forward<T>(arg)), tag);
		return (MidoriTraceable*)tagged_pointer;
	}

private:
	MidoriTraceable() = delete;

	MidoriTraceable(const MidoriTraceable& other) = delete;

	MidoriTraceable(MidoriTraceable&& other) noexcept = delete;

	MidoriTraceable& operator=(const MidoriTraceable& other) = delete;

	MidoriTraceable& operator=(MidoriTraceable&& other) noexcept = delete;

	MidoriTraceable(MidoriText&& str) noexcept;

	MidoriTraceable(MidoriArray&& array) noexcept;

	MidoriTraceable(MidoriCellValue&& cell_value) noexcept;

	MidoriTraceable(MidoriClosure&& closure) noexcept;

	MidoriTraceable(MidoriStruct&& midori_struct) noexcept;

	MidoriTraceable(MidoriUnion&& midori_union) noexcept;

	MidoriTraceable(MidoriBox&& box) noexcept;

	static void* operator new(size_t size) noexcept;
};