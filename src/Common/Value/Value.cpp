#include "Common/Printer/Printer.h"
#include "Value.h"

#include <algorithm>
#include <bit>
#include <execution>
#include <ranges>

namespace UTF8
{
	bool IsContinuationByte(char byte)
	{
		return (static_cast<unsigned char>(byte) & 0xC0u) == 0x80u;
	}

	int GetCharacterByteCount(const char* str)
	{
		unsigned char first_byte = static_cast<unsigned char>(*str);

		if ((first_byte & 0x80u) == 0x00u)
		{
			return 1; // ASCII: 0xxxxxxx
		}
		else if ((first_byte & 0xE0u) == 0xC0u)
		{
			return 2; // 110xxxxx
		}
		else if ((first_byte & 0xF0u) == 0xE0u)
		{
			return 3; // 1110xxxx
		}
		else if ((first_byte & 0xF8u) == 0xF0u)
		{
			return 4; // 11110xxx
		}
		else
		{
			return 1; // Invalid UTF-8, treat as single byte
		}
	}

	int CountCodePoints(const char* str, int byte_length)
	{
		int count = 0;
		for (int i = 0; i < byte_length;)
		{
			int char_bytes = GetCharacterByteCount(str + i);
			count += 1;
			i += char_bytes;
		}
		return count;
	}

	int GetByteOffsetOfCodePoint(const char* str, int byte_length, int code_point_index)
	{
		int count = 0;
		int offset = 0;
		while (offset < byte_length && count < code_point_index)
		{
			int char_bytes = GetCharacterByteCount(str + offset);
			offset += char_bytes;
			count += 1;
		}
		return offset;
	}

	int StepBackward(const char* str, int current_offset)
	{
		if (current_offset <= 0)
		{
			return 0;
		}

		int offset = current_offset - 1;
		while (offset > 0 && IsContinuationByte(str[offset]))
		{
			offset -= 1;
		}
		return offset;
	}
}

MidoriText ConvertToQuotedText(const MidoriText& input)
{
	MidoriText result("\"");

	// Iterate over bytes to handle escape sequences (which are ASCII)
	const char* str = input.GetCString();
	for (int i = 0; i < input.GetByteLength(); i += 1)
	{
		char c = str[i];
		switch (c)
		{
			case '\n':
			{
				result.Append("\\n");
				break;
			}
			case '\t':
			{
				result.Append("\\t");
				break;
			}
			case '\r':
			{
				result.Append("\\r");
				break;
			}
			case '\\':
			{
				result.Append("\\\\");
				break;
			}
			case '\"':
			{
				result.Append("\\\"");
				break;
			}
			default:
			{
				// For non-escape characters, just append the byte
				// UTF-8 multi-byte sequences will be passed through correctly
				result.Append(c);
			}
		}
	}

	result.Append('"');

	return result;
}

MidoriValue::MidoriValue() noexcept
	: m_data{.m_integer = 0}
#if MIDORI_DEBUG_INFO
	, m_tag(UNIT)
#endif
{
}

MidoriValue::MidoriValue(MidoriFloat midori_float) noexcept
	: m_data{.m_float = midori_float}
#if MIDORI_DEBUG_INFO
	, m_tag(FLOAT)
#endif
{
}

MidoriValue::MidoriValue(MidoriInteger integer) noexcept
	: m_data{.m_integer = integer}
#if MIDORI_DEBUG_INFO
	, m_tag(INT)
#endif
{
}

MidoriValue::MidoriValue(MidoriByte byte) noexcept
	: m_data{.m_integer = static_cast<MidoriInteger>(byte)}
#if MIDORI_DEBUG_INFO
	, m_tag(BYTE)
#endif
{
}

MidoriValue::MidoriValue(MidoriWord word) noexcept
	: m_data{.m_integer = static_cast<MidoriInteger>(word)}
#if MIDORI_DEBUG_INFO
	, m_tag(WORD)
#endif
{
}

MidoriValue::MidoriValue(MidoriBool b) noexcept
	: m_data{.m_bool = b}
#if MIDORI_DEBUG_INFO
	, m_tag(BOOL)
#endif
{
}

MidoriValue::MidoriValue(MidoriTraceable* tagged_pointer) noexcept
	: m_data{.m_pointer = tagged_pointer}
#if MIDORI_DEBUG_INFO
	, m_tag(POINTER)
#endif
{
}

MidoriFloat MidoriValue::GetFloat() const noexcept
{
	return m_data.m_float;
}

MidoriInteger MidoriValue::GetInteger() const noexcept
{
	return m_data.m_integer;
}

MidoriByte MidoriValue::GetByte() const noexcept
{
	return static_cast<MidoriByte>(m_data.m_integer & 0xFF);
}

MidoriWord MidoriValue::GetWord() const noexcept
{
	return static_cast<MidoriWord>(m_data.m_integer);
}

MidoriUnit MidoriValue::GetUnit() const noexcept
{
	return {};
}

MidoriBool MidoriValue::GetBool() const noexcept
{
	return m_data.m_bool;
}

MidoriTraceable* MidoriValue::GetPointer() const noexcept
{
	uintptr_t raw = reinterpret_cast<uintptr_t>(m_data.m_pointer);
	return reinterpret_cast<MidoriTraceable*>(raw & TAG_MASK);
}

#if MIDORI_DEBUG_INFO
MidoriText MidoriValue::ToText() const
{
	switch (m_tag)
	{
		case MidoriValue::FLOAT:
			return MidoriText::FromFloat(GetFloat());
		case MidoriValue::INT:
			return MidoriText::FromInteger(GetInteger());
		case MidoriValue::BYTE:
			return MidoriText::FromInteger(static_cast<MidoriInteger>(GetByte()));
		case MidoriValue::WORD:
			return MidoriText::FromInteger(static_cast<MidoriInteger>(GetWord()));
		case MidoriValue::BOOL:
			return GetBool() ? "true" : "false";
		case MidoriValue::UNIT:
			return "()";
		case MidoriValue::POINTER:
			return GetPointer()->ToText();
		default:
			return "!!!UNKNOWN!!!";
	}
}

bool MidoriValue::IsPointer() const noexcept
{
	return m_tag == POINTER;
}

MidoriValue::DebugTypeTag MidoriValue::GetTag() const noexcept
{
	return m_tag;
}
#endif

MidoriTraceable::MidoriTraceable(MidoriText&& str) noexcept : m_text(std::move(str)), m_type(TraceableType::Text)
{
}

MidoriTraceable::MidoriTraceable(MidoriArray&& array) noexcept : m_array(std::move(array)), m_type(TraceableType::Array)
{
}

MidoriTraceable::MidoriTraceable(MidoriIntRange&& range) noexcept : m_int_range(std::move(range)), m_type(TraceableType::IntRange)
{
}

MidoriTraceable::MidoriTraceable(MidoriFloatRange&& range) noexcept : m_float_range(std::move(range)), m_type(TraceableType::FloatRange)
{
}

MidoriTraceable::MidoriTraceable(MidoriCellValue&& cell_value) noexcept : m_cell(std::move(cell_value)), m_type(TraceableType::Cell)
{
}

MidoriTraceable::MidoriTraceable(MidoriClosure&& closure) noexcept : m_closure(std::move(closure)), m_type(TraceableType::Closure)
{
}

MidoriTraceable::MidoriTraceable(MidoriStruct&& midori_struct)noexcept : m_struct(std::move(midori_struct)), m_type(TraceableType::Struct)
{
}

MidoriTraceable::MidoriTraceable(MidoriUnion&& midori_union) noexcept : m_union(std::move(midori_union)), m_type(TraceableType::Union)
{
}

MidoriTraceable::MidoriTraceable(MidoriFuture&& midori_future) noexcept : m_future(std::move(midori_future)), m_type(TraceableType::Future)
{
}

MidoriTraceable::~MidoriTraceable()
{
	switch (m_type)
	{
	case TraceableType::Text:
		m_text.~MidoriText();
		break;
	case TraceableType::Array:
		m_array.~MidoriArray();
		break;
	case TraceableType::IntRange:
		m_int_range.~MidoriIntRange();
		break;
	case TraceableType::FloatRange:
		m_float_range.~MidoriFloatRange();
		break;
	case TraceableType::Struct:
		m_struct.~MidoriStruct();
		break;
	case TraceableType::Union:
		m_union.~MidoriUnion();
		break;
	case TraceableType::Cell:
		m_cell.~MidoriCellValue();
		break;
	case TraceableType::Closure:
		m_closure.~MidoriClosure();
		break;
	case TraceableType::Future:
		m_future.~MidoriFuture();
		break;
	}
}

#if MIDORI_DEBUG_INFO
MidoriText MidoriTraceable::ToText()
{
	switch (m_type)
	{
	case TraceableType::Text:
		return ConvertToQuotedText(m_text);
	case TraceableType::Array:
	{
		if (m_array.GetLength() == 0)
		{
			return MidoriText("[]");
		}

		MidoriText result("[");
		for (int idx : std::views::iota(0, m_array.GetLength()))
		{
			result.Append(m_array[idx].ToText()).Append(", ");
		}
		result.Pop().Pop().Append("]");
		return result;
	}
	case TraceableType::IntRange:
		return MidoriText("IntRange");
	case TraceableType::FloatRange:
		return MidoriText("FloatRange");
	case TraceableType::Cell:
		return MidoriText("Cell(").Append(m_cell.GetValue().ToText()).Append(")");
	case TraceableType::Closure:
	{
		char buffer[64];
		std::snprintf(buffer, sizeof(buffer), "<closure at: %p>", (void*)this);
		return MidoriText(buffer);
	}
	case TraceableType::Union:
	{
		if (m_union.m_values.GetLength() == 0)
		{
			return MidoriText("Union{}");
		}

		MidoriText union_val("Union{");
		for (int idx : std::views::iota(0, m_union.m_values.GetLength()))
		{
			union_val.Append(m_union.m_values[idx].ToText()).Append(", ");
		}
		return union_val.Pop().Pop().Append("}");
	}
	case TraceableType::Struct:
	{
		if (m_struct.m_values.GetLength() == 0)
		{
			return MidoriText("Struct{}");
		}

		MidoriText struct_val("Struct{");
		for (int idx : std::views::iota(0, m_struct.m_values.GetLength()))
		{
			struct_val.Append(m_struct.m_values[idx].ToText()).Append(", ");
		}
		return struct_val.Pop().Pop().Append("}");
	}
	case TraceableType::Future:
	{
		char buffer[64];
		std::snprintf(buffer, sizeof(buffer), "<future at: %p>", (void*)this);
		return MidoriText(buffer);
	}
	default:
		return MidoriText("Unknown MidoriTraceable");
	}
}
#endif

size_t MidoriTraceable::GetSize() const
{
	size_t dynamic_size = 0uz;
	switch (m_type)
	{
	case TraceableType::Text:
		dynamic_size = m_text.GetCapacity();
		break;
	case TraceableType::Array:
		dynamic_size = m_array.GetCapacity();
		break;
	case TraceableType::Closure:
		dynamic_size = m_closure.m_cell_values.GetCapacity();
		break;
	case TraceableType::Struct:
		dynamic_size = m_struct.m_values.GetCapacity();
		break;
	case TraceableType::Union:
		dynamic_size = m_union.m_values.GetCapacity();
		break;
	case TraceableType::Future:
		if (m_future.m_closure)
		{
			dynamic_size = sizeof(MidoriClosure) + m_future.m_closure->m_cell_values.GetCapacity();
		}
		break;
	default:
		break;
	}
	return sizeof(MidoriTraceable) + dynamic_size;
}

void MidoriTraceable::Mark()
{
	m_is_marked = true;
}

void MidoriTraceable::Unmark()
{
	m_is_marked = false;
}

bool MidoriTraceable::IsMarked() const
{
	return m_is_marked;
}

void* MidoriTraceable::operator new(size_t size) noexcept
{
	void* object = ::operator new(size);
	return object;
}

void* MidoriTraceable::operator new(size_t size, std::align_val_t al) noexcept
{
	void* object = ::operator new(size, al);
	return object;
}

void* MidoriTraceable::operator new(size_t, void* ptr) noexcept
{
	return ptr;
}

void* MidoriTraceable::operator new(size_t, std::align_val_t, void* ptr) noexcept
{
	return ptr;
}

void MidoriTraceable::operator delete(void*, void*) noexcept
{
}

void MidoriTraceable::operator delete(void*, std::align_val_t, void*) noexcept
{
}

void MidoriTraceable::operator delete(void* object, size_t size) noexcept
{
	MidoriTraceable* traceable = static_cast<MidoriTraceable*>(object);
	(void)traceable;  // Unused but needed for potential future debugging

	::operator delete(object, size);
}

void MidoriTraceable::operator delete(void* object, std::align_val_t al) noexcept
{
	::operator delete(object, al);
}

MidoriArray::MidoriArray(int size)
{
	m_capacity = size < 0 ? s_initial_capacity : size;
	m_length = m_capacity;
	m_start = 0;

	m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(m_capacity) * sizeof(MidoriValue)));
	if (!m_data)
	{
		std::exit(EXIT_FAILURE);
	}
}

MidoriArray::MidoriArray(const MidoriArray& other) : m_capacity(other.m_capacity), m_length(other.m_length), m_start(0)
{
	m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_capacity) * sizeof(MidoriValue)));
	if (!m_data)
	{
		std::exit(EXIT_FAILURE);
	}

	int tail_len = other.m_capacity - other.m_start;
	if (m_length <= tail_len)
	{
		std::memcpy(m_data, other.m_data + other.m_start, static_cast<size_t>(m_length) * sizeof(MidoriValue));
	}
	else
	{
		std::memcpy(m_data, other.m_data + other.m_start, static_cast<size_t>(tail_len) * sizeof(MidoriValue));
		std::memcpy(m_data + tail_len, other.m_data, static_cast<size_t>(m_length - tail_len) * sizeof(MidoriValue));
	}
}

MidoriArray::MidoriArray(MidoriArray&& other) noexcept : m_data(other.m_data), m_capacity(other.m_capacity), m_length(other.m_length), m_start(other.m_start)
{
	other.m_data = nullptr;
	other.m_capacity = 0;
	other.m_length = 0;
	other.m_start = 0;
}

MidoriArray& MidoriArray::operator=(const MidoriArray& other)
{
	if (this != &other)
	{
		MidoriValue* new_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_capacity) * sizeof(MidoriValue)));
		if (!new_data)
		{
			std::exit(EXIT_FAILURE);
		}

		int tail_len = other.m_capacity - other.m_start;
		int other_len = other.m_length;

		if (other_len <= tail_len)
		{
			std::memcpy(new_data, other.m_data + other.m_start, static_cast<size_t>(other_len) * sizeof(MidoriValue));
		}
		else
		{
			std::memcpy(new_data, other.m_data + other.m_start, static_cast<size_t>(tail_len) * sizeof(MidoriValue));
			std::memcpy(new_data + tail_len, other.m_data, static_cast<size_t>(other_len - tail_len) * sizeof(MidoriValue));
		}

		std::free(m_data);
		m_data = new_data;
		m_capacity = other.m_capacity;
		m_length = other.m_length;
		m_start = 0;
	}
	return *this;
}

MidoriArray& MidoriArray::operator=(MidoriArray&& other) noexcept
{
	if (this != &other)
	{
		std::free(m_data);
		m_data = other.m_data;
		m_capacity = other.m_capacity;
		m_length = other.m_length;
		m_start = other.m_start;

		other.m_data = nullptr;
		other.m_capacity = 0;
		other.m_length = 0;
		other.m_start = 0;
	}
	return *this;
}

MidoriArray::~MidoriArray()
{
	std::free(m_data);
}

MidoriValue& MidoriArray::operator[](int index)
{
	if (m_start == 0)
	{
		return m_data[index];
	}
	return m_data[(m_start + index) % m_capacity];
}

const MidoriValue& MidoriArray::operator[](int index) const
{
	if (m_start == 0)
	{
		return m_data[index];
	}
	return m_data[(m_start + index) % m_capacity];
}

void MidoriArray::Expand()
{
	size_t new_capacity = m_capacity == 0
		? s_initial_capacity
		: static_cast<size_t>(m_capacity) * 2u;

	MidoriValue* new_data = static_cast<MidoriValue*>(std::malloc(new_capacity * sizeof(MidoriValue)));
	if (!new_data)
	{
		std::exit(EXIT_FAILURE);
	}

	if (m_data)
	{
		int tail_len = m_capacity - m_start;
		if (m_length <= tail_len)
		{
			std::memcpy(new_data, m_data + m_start, static_cast<size_t>(m_length) * sizeof(MidoriValue));
		}
		else
		{
			std::memcpy(new_data, m_data + m_start, static_cast<size_t>(tail_len) * sizeof(MidoriValue));
			std::memcpy(new_data + tail_len, m_data, static_cast<size_t>(m_length - tail_len) * sizeof(MidoriValue));
		}
		std::free(m_data);
	}

	m_data = new_data;
	m_capacity = static_cast<int>(new_capacity);
	m_start = 0;
}

std::optional<MidoriValue> MidoriArray::Pop()
{
	if (m_length > 0)
	{
		int idx = (m_start + m_length - 1) % m_capacity;
		MidoriValue val = m_data[idx];
		m_length -= 1;

		if (m_length < m_capacity / 2)
		{
			Shrink();
		}

		return std::optional<MidoriValue>(val);
	}
	else
	{
		return std::nullopt;
	}
}

void MidoriArray::AddFront(const MidoriValue& value)
{
	if (m_length >= m_capacity)
	{
		Expand();
	}

	m_start = (m_start - 1 + m_capacity) % m_capacity;
	m_data[m_start] = value;
	m_length += 1;
}

void MidoriArray::Shrink()
{
	if (m_capacity <= s_initial_capacity)
	{
		return;
	}

	size_t new_capacity = static_cast<size_t>(m_capacity) / 2u;
	if (new_capacity < static_cast<size_t>(m_length))
	{
		new_capacity = m_length;
	}
	if (new_capacity == 0)
	{
		new_capacity = s_initial_capacity;
	}

	MidoriValue* new_data = static_cast<MidoriValue*>(std::malloc(new_capacity * sizeof(MidoriValue)));
	if (!new_data)
	{
		std::exit(EXIT_FAILURE);
	}

	int tail_len = m_capacity - m_start;
	if (m_length <= tail_len)
	{
		std::memcpy(new_data, m_data + m_start, static_cast<size_t>(m_length) * sizeof(MidoriValue));
	}
	else
	{
		std::memcpy(new_data, m_data + m_start, static_cast<size_t>(tail_len) * sizeof(MidoriValue));
		std::memcpy(new_data + tail_len, m_data, static_cast<size_t>(m_length - tail_len) * sizeof(MidoriValue));
	}

	std::free(m_data);
	m_data = new_data;
	m_capacity = static_cast<int>(new_capacity);
	m_start = 0;
}

void MidoriArray::AddBack(const MidoriValue& value)
{
	if (m_length >= m_capacity)
	{
		Expand();
	}

	m_data[(m_start + m_length) % m_capacity] = value;
	m_length += 1;
}

int MidoriArray::GetLength() const
{
	return m_length;
}

size_t MidoriArray::GetCapacity() const
{
	return static_cast<size_t>(m_capacity) * sizeof(MidoriValue);
}

MidoriArray MidoriArray::Concatenate(const MidoriArray& a, const MidoriArray& b)
{
	int a_len = a.GetLength();
	int b_len = b.GetLength();
	int total_len = a_len + b_len;
	int new_capacity = total_len + (total_len >> 1);

	MidoriArray result;
	result.m_capacity = new_capacity;
	result.m_length = total_len;
	result.m_start = 0;
	result.m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(new_capacity) * sizeof(MidoriValue)));
	if (!result.m_data)
	{
		std::exit(EXIT_FAILURE);
	}

	if (a.m_start == 0)
	{
		std::memcpy(result.m_data, a.m_data, static_cast<size_t>(a_len) * sizeof(MidoriValue));
	}
	else
	{
		for (int i = 0; i < a_len; i += 1)
		{
			result.m_data[i] = a.m_data[(a.m_start + i) % a.m_capacity];
		}
	}

	if (b.m_start == 0)
	{
		std::memcpy(result.m_data + a_len, b.m_data, static_cast<size_t>(b_len) * sizeof(MidoriValue));
	}
	else
	{
		for (int i = 0; i < b_len; i += 1)
		{
			result.m_data[a_len + i] = b.m_data[(b.m_start + i) % b.m_capacity];
		}
	}

	return result;
}

MidoriArray MidoriArray::FromFFI(MidoriValue* ffi_allocated_data, int length)
{
	MidoriArray result;
	if (ffi_allocated_data == nullptr || length <= 0)
	{
		std::free(ffi_allocated_data);
		return result;
	}

	result.m_capacity = (length < s_initial_capacity) ? s_initial_capacity : length;
	result.m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(result.m_capacity) * sizeof(MidoriValue)));
	result.m_length = length;
	result.m_start = 0;

	std::memcpy(result.m_data, ffi_allocated_data, static_cast<size_t>(length) * sizeof(MidoriValue));
	std::free(ffi_allocated_data);

	return result;
}

MidoriTuple::MidoriTuple(int size)
{
	m_size = size;
	if (size > 0)
	{
		m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(size) * sizeof(MidoriValue)));
		if (!m_data)
		{
			std::exit(EXIT_FAILURE);
		}
	}
}

MidoriTuple::MidoriTuple(const MidoriTuple& other) : m_size(other.m_size)
{
	if (m_size > 0)
	{
		m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(m_size) * sizeof(MidoriValue)));
		if (!m_data)
		{
			std::exit(EXIT_FAILURE);
		}
		std::memcpy(m_data, other.m_data, static_cast<size_t>(m_size) * sizeof(MidoriValue));
	}
}

MidoriTuple::MidoriTuple(MidoriTuple&& other) noexcept : m_data(other.m_data), m_size(other.m_size)
{
	other.m_data = nullptr;
	other.m_size = 0;
}

MidoriTuple& MidoriTuple::operator=(const MidoriTuple& other)
{
	if (this != &other)
	{
		if (m_size != other.m_size)
		{
			std::free(m_data);
			m_size = other.m_size;
			if (m_size > 0)
			{
				m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(m_size) * sizeof(MidoriValue)));
				if (!m_data)
				{
					std::exit(EXIT_FAILURE);
				}
			}
			else
			{
				m_data = nullptr;
			}
		}
		
		if (m_size > 0)
		{
			std::memcpy(m_data, other.m_data, static_cast<size_t>(m_size) * sizeof(MidoriValue));
		}
	}
	return *this;
}

MidoriTuple& MidoriTuple::operator=(MidoriTuple&& other) noexcept
{
	if (this != &other)
	{
		std::free(m_data);
		m_data = other.m_data;
		m_size = other.m_size;

		other.m_data = nullptr;
		other.m_size = 0;
	}
	return *this;
}

MidoriTuple::~MidoriTuple()
{
	std::free(m_data);
}

MidoriValue& MidoriTuple::operator[](int index)
{
	return m_data[index];
}

const MidoriValue& MidoriTuple::operator[](int index) const
{
	return m_data[index];
}

int MidoriTuple::GetLength() const
{
	return m_size;
}

size_t MidoriTuple::GetCapacity() const
{
	return static_cast<size_t>(m_size) * sizeof(MidoriValue);
}

MidoriIntRange::MidoriIntRange(MidoriInteger start, MidoriInteger end, MidoriInteger step)
	: m_start(start), m_end(end), m_step(step)
{
}

MidoriInteger MidoriIntRange::GetStart() const
{
	return m_start;
}

MidoriInteger MidoriIntRange::GetEnd() const
{
	return m_end;
}

MidoriInteger MidoriIntRange::GetStep() const
{
	return m_step;
}

MidoriFloatRange::MidoriFloatRange(MidoriFloat start, MidoriFloat end, MidoriFloat step)
	: m_start(start), m_end(end), m_step(step)
{
}

MidoriFloat MidoriFloatRange::GetStart() const
{
	return m_start;
}

MidoriFloat MidoriFloatRange::GetEnd() const
{
	return m_end;
}

MidoriFloat MidoriFloatRange::GetStep() const
{
	return m_step;
}

MidoriText::MidoriText()
{
	m_short.m_buffer[0] = '\0';
	SetShortSize(0);
}

MidoriText::MidoriText(const char* str)
{
	if (!str)
	{
		m_short.m_buffer[0] = '\0';
		SetShortSize(0);
	}
	else
	{
		int size = static_cast<int>(std::strlen(str));
		if (size <= SSO_CAPACITY)
		{
			std::memcpy(m_short.m_buffer, str, size);
			m_short.m_buffer[size] = '\0';
			SetShortSize(size);
		}
		else
		{
			m_long.m_ptr = static_cast<char*>(std::malloc(size + 1));
			if (!m_long.m_ptr)
			{
				std::exit(EXIT_FAILURE);
			}
			std::memcpy(m_long.m_ptr, str, size);
			m_long.m_ptr[size] = '\0';
			m_long.m_size = size;
			m_long.m_capacity = size;
			m_long.m_length_cache = -1;
			m_long.m_flag = 0; // Long mode (even)
		}
	}
}

MidoriText::MidoriText(const MidoriText& other)
{
	if (other.IsShort())
	{
		// Copy short layout directly (24 bytes)
		std::memcpy(this, &other, sizeof(MidoriText));
	}
	else
	{
		m_long.m_ptr = static_cast<char*>(std::malloc(other.m_long.m_size + 1));
		if (!m_long.m_ptr)
		{
			std::exit(EXIT_FAILURE);
		}
		std::memcpy(m_long.m_ptr, other.m_long.m_ptr, other.m_long.m_size + 1);
		m_long.m_size = other.m_long.m_size;
		m_long.m_capacity = other.m_long.m_size; // Tight copy
		m_long.m_length_cache = other.m_long.m_length_cache;
		m_long.m_flag = 0;
	}
}

MidoriText::MidoriText(MidoriText&& other) noexcept
{
	std::memcpy(this, &other, sizeof(MidoriText));
	// Reset other to empty short string
	other.m_short.m_buffer[0] = '\0';
	other.SetShortSize(0);
}

MidoriText& MidoriText::operator=(const MidoriText& other)
{
	if (this == &other)
	{
		return *this;
	}

	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}

	if (other.IsShort())
	{
		std::memcpy(this, &other, sizeof(MidoriText));
	}
	else
	{
		m_long.m_ptr = static_cast<char*>(std::malloc(other.m_long.m_size + 1));
		if (!m_long.m_ptr)
		{
			std::exit(EXIT_FAILURE);
		}
		std::memcpy(m_long.m_ptr, other.m_long.m_ptr, other.m_long.m_size + 1);
		m_long.m_size = other.m_long.m_size;
		m_long.m_capacity = other.m_long.m_size;
		m_long.m_length_cache = other.m_long.m_length_cache;
		m_long.m_flag = 0;
	}
	return *this;
}

MidoriText& MidoriText::operator=(MidoriText&& other) noexcept
{
	if (this == &other)
	{
		return *this;
	}

	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}

	std::memcpy(this, &other, sizeof(MidoriText));
	
	other.m_short.m_buffer[0] = '\0';
	other.SetShortSize(0);
	return *this;
}

MidoriText::~MidoriText()
{
	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}
}

int MidoriText::GetLength() const noexcept
{
	if (IsShort())
	{
		return UTF8::CountCodePoints(m_short.m_buffer, GetShortSize());
	}
	else
	{
		if (m_long.m_length_cache == -1)
		{
			m_long.m_length_cache = UTF8::CountCodePoints(m_long.m_ptr, m_long.m_size);
		}
		return m_long.m_length_cache;
	}
}

int MidoriText::GetByteLength() const noexcept
{
	return IsShort() ? GetShortSize() : m_long.m_size;
}

const char* MidoriText::GetCString() const noexcept
{
	return IsShort() ? m_short.m_buffer : m_long.m_ptr;
}

MidoriText& MidoriText::Pop()
{
	if (IsShort())
	{
		int size = GetShortSize();
		if (size > 0)
		{
			int new_size = UTF8::StepBackward(m_short.m_buffer, size);
			m_short.m_buffer[new_size] = '\0';
			SetShortSize(new_size);
		}
	}
	else
	{
		if (m_long.m_size > 0)
		{
			int new_size = UTF8::StepBackward(m_long.m_ptr, m_long.m_size);
			m_long.m_size = new_size;
			m_long.m_ptr[new_size] = '\0';
			if (m_long.m_length_cache > 0)
			{
				m_long.m_length_cache -= 1;
			}
		}
	}
	return *this;
}

MidoriText& MidoriText::Append(const char* str)
{
	if (!str)
	{
		return *this;
	}

	int len = static_cast<int>(std::strlen(str));
	if (len == 0)
	{
		return *this;
	}

	int current_size = GetByteLength();
	int new_size = current_size + len;

	if (IsShort())
	{
		if (new_size <= SSO_CAPACITY)
		{
			std::memcpy(m_short.m_buffer + current_size, str, len);
			m_short.m_buffer[new_size] = '\0';
			SetShortSize(new_size);
		}
		else
		{
			Expand(new_size);
			std::memcpy(m_long.m_ptr + current_size, str, len);
			m_long.m_ptr[new_size] = '\0';
			m_long.m_size = new_size;
			m_long.m_length_cache = -1;
		}
	}
	else
	{
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data)
			{
				std::exit(EXIT_FAILURE);
			}
			m_long.m_ptr = new_data;
			m_long.m_capacity = new_capacity;
		}
		std::memcpy(m_long.m_ptr + current_size, str, len);
		m_long.m_ptr[new_size] = '\0';
		m_long.m_size = new_size;
		m_long.m_length_cache = -1;
	}
	return *this;
}

MidoriText& MidoriText::Append(char c)
{
	int current_size = GetByteLength();
	int new_size = current_size + 1;

	if (IsShort())
	{
		if (new_size <= SSO_CAPACITY)
		{
			m_short.m_buffer[current_size] = c;
			m_short.m_buffer[new_size] = '\0';
			SetShortSize(new_size);
		}
		else
		{
			Expand(new_size);
			m_long.m_ptr[current_size] = c;
			m_long.m_ptr[new_size] = '\0';
			m_long.m_size = new_size;
			m_long.m_length_cache = -1; // Reset cache on expansion/conversion
		}
	}
	else
	{
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data)
			{
				std::exit(EXIT_FAILURE);
			}
			m_long.m_ptr = new_data;
			m_long.m_capacity = new_capacity;
		}
		m_long.m_ptr[current_size] = c;
		m_long.m_ptr[new_size] = '\0';
		m_long.m_size = new_size;
		
		if (m_long.m_length_cache != -1)
		{
			if ((static_cast<unsigned char>(c) & 0xC0u) != 0x80u)
			{
				m_long.m_length_cache += 1;
			}
		}
	}
	return *this;
}

MidoriText& MidoriText::Append(const MidoriText& other)
{
	int other_byte_len = other.GetByteLength();
	if (other_byte_len == 0)
	{
		return *this;
	}

	int current_size = GetByteLength();
	int new_size = current_size + other_byte_len;
	const char* other_str = other.GetCString();

	if (IsShort())
	{
		if (new_size <= SSO_CAPACITY)
		{
			std::memcpy(m_short.m_buffer + current_size, other_str, other_byte_len);
			m_short.m_buffer[new_size] = '\0';
			SetShortSize(new_size);
		}
		else
		{
			Expand(new_size);
			std::memcpy(m_long.m_ptr + current_size, other_str, other_byte_len);
			m_long.m_ptr[new_size] = '\0';
			m_long.m_size = new_size;
			m_long.m_length_cache = -1;
		}
	}
	else
	{
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data)
			{
				std::exit(EXIT_FAILURE);
			}
			m_long.m_ptr = new_data;
			m_long.m_capacity = new_capacity;
		}
		std::memcpy(m_long.m_ptr + current_size, other_str, other_byte_len);
		m_long.m_ptr[new_size] = '\0';
		m_long.m_size = new_size;

		if (m_long.m_length_cache != -1 && !other.IsShort() && other.m_long.m_length_cache != -1)
		{
			m_long.m_length_cache += other.m_long.m_length_cache;
		}
		else
		{
			m_long.m_length_cache = -1;
		}
	}
	return *this;
}

void MidoriText::Reserve(int capacity)
{
	if (IsShort())
	{
		if (capacity > SSO_CAPACITY)
		{
			Expand(capacity);
		}
	}
	else
	{
		if (capacity > m_long.m_capacity)
		{
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, capacity + 1));
			if (!new_data) std::exit(EXIT_FAILURE);
			m_long.m_ptr = new_data;
			m_long.m_capacity = capacity;
		}
	}
}

MidoriText& MidoriText::Prepend(const char* str)
{
	if (!str) return *this;
	int len = static_cast<int>(std::strlen(str));
	if (len == 0) return *this;

	int current_size = GetByteLength();
	int new_size = current_size + len;

	if (IsShort())
	{
		if (new_size <= SSO_CAPACITY)
		{
			std::memmove(m_short.m_buffer + len, m_short.m_buffer, current_size);
			std::memcpy(m_short.m_buffer, str, len);
			m_short.m_buffer[new_size] = '\0';
			SetShortSize(new_size);
		}
		else
		{
			// Convert to Long
			char* new_data = static_cast<char*>(std::malloc(std::max(new_size, current_size * 2) + 1));
			if (!new_data) std::exit(EXIT_FAILURE);
			
			std::memcpy(new_data, str, len);
			std::memcpy(new_data + len, m_short.m_buffer, current_size);
			new_data[new_size] = '\0';
			
			// Initialize Long
			m_long.m_ptr = new_data;
			m_long.m_size = new_size;
			m_long.m_capacity = std::max(new_size, current_size * 2);
			m_long.m_length_cache = -1;
			m_long.m_flag = 0;
		}
	}
	else
	{
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data) std::exit(EXIT_FAILURE);
			m_long.m_ptr = new_data;
			m_long.m_capacity = new_capacity;
		}
		std::memmove(m_long.m_ptr + len, m_long.m_ptr, current_size);
		std::memcpy(m_long.m_ptr, str, len);
		m_long.m_ptr[new_size] = '\0';
		m_long.m_size = new_size;
		m_long.m_length_cache = -1;
	}
	return *this;
}

MidoriText& MidoriText::Prepend(char c)
{
	int current_size = GetByteLength();
	int new_size = current_size + 1;

	if (IsShort())
	{
		if (new_size <= SSO_CAPACITY)
		{
			std::memmove(m_short.m_buffer + 1, m_short.m_buffer, current_size);
			m_short.m_buffer[0] = c;
			m_short.m_buffer[new_size] = '\0';
			SetShortSize(new_size);
		}
		else
		{
			// Convert to Long
			char* new_data = static_cast<char*>(std::malloc(std::max(new_size, current_size * 2) + 1));
			if (!new_data) std::exit(EXIT_FAILURE);
			
			new_data[0] = c;
			std::memcpy(new_data + 1, m_short.m_buffer, current_size);
			new_data[new_size] = '\0';
			
			m_long.m_ptr = new_data;
			m_long.m_size = new_size;
			m_long.m_capacity = std::max(new_size, current_size * 2);
			m_long.m_length_cache = -1;
			m_long.m_flag = 0;
		}
	}
	else
	{
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data) std::exit(EXIT_FAILURE);
			m_long.m_ptr = new_data;
			m_long.m_capacity = new_capacity;
		}
		std::memmove(m_long.m_ptr + 1, m_long.m_ptr, current_size);
		m_long.m_ptr[0] = c;
		m_long.m_ptr[new_size] = '\0';
		m_long.m_size = new_size;
		m_long.m_length_cache = -1;
	}
	return *this;
}

MidoriText& MidoriText::Prepend(const MidoriText& other)
{
	return Prepend(other.GetCString());
}

char MidoriText::operator[](int index) const
{
	int byte_offset = UTF8::GetByteOffsetOfCodePoint(GetCString(), GetByteLength(), index);
	return GetCString()[byte_offset];
}

bool MidoriText::operator==(const MidoriText& other) const
{
	int len = GetByteLength();
	return (len == other.GetByteLength()) && (len == 0 || std::memcmp(GetCString(), other.GetCString(), len) == 0);
}

bool MidoriText::operator!=(const MidoriText& other) const
{
	return !(*this == other);
}

MidoriInteger MidoriText::ToInteger() const
{
	return std::atoll(GetCString());
}

MidoriFloat MidoriText::ToFloat() const
{
	return std::atof(GetCString());
}

MidoriText MidoriText::FromInteger(MidoriInteger value)
{
	char buffer[21];
	std::snprintf(buffer, 21, "%lld", value);
	return MidoriText(buffer);
}

MidoriText MidoriText::FromFloat(MidoriFloat value)
{
	char buffer[32];
	std::snprintf(buffer, 32, "%f", value);
	return MidoriText(buffer);
}

MidoriText MidoriText::Concatenate(const MidoriText& a, const MidoriText& b)
{
	// Concatenate delegates to constructor logic via raw buffer, then optimizing.
	// But it's better to construct explicitly.
	
	int byte_len_a = a.GetByteLength();
	int byte_len_b = b.GetByteLength();
	int total_byte_len = byte_len_a + byte_len_b;

	MidoriText result;
	if (total_byte_len <= SSO_CAPACITY)
	{
		std::memcpy(result.m_short.m_buffer, a.GetCString(), byte_len_a);
		std::memcpy(result.m_short.m_buffer + byte_len_a, b.GetCString(), byte_len_b);
		result.m_short.m_buffer[total_byte_len] = '\0';
		result.SetShortSize(total_byte_len);
	}
	else
	{
		int new_capacity = total_byte_len + (total_byte_len >> 1);
		result.m_long.m_ptr = static_cast<char*>(std::malloc(new_capacity + 1));
		result.m_long.m_size = total_byte_len;
		result.m_long.m_capacity = new_capacity;
		std::memcpy(result.m_long.m_ptr, a.GetCString(), byte_len_a);
		std::memcpy(result.m_long.m_ptr + byte_len_a, b.GetCString(), byte_len_b);
		result.m_long.m_ptr[total_byte_len] = '\0';
		result.m_long.m_length_cache = -1;
		result.m_long.m_flag = 0;

		if (!a.IsShort() && !b.IsShort() && a.m_long.m_length_cache != -1 && b.m_long.m_length_cache != -1)
		{
			result.m_long.m_length_cache = a.m_long.m_length_cache + b.m_long.m_length_cache;
		}
	}
	return result;
}

MidoriText MidoriText::FromFFI(char* ffi_allocated_string)
{
	// FromFFI adopts the string.
	// If the string is short, we copy it to SSO and free the FFI string.
	// If long, we adopt it.
	
	if (!ffi_allocated_string) return MidoriText();
	
	int size = static_cast<int>(std::strlen(ffi_allocated_string));
	if (size <= SSO_CAPACITY)
	{
		MidoriText result(ffi_allocated_string); // This copies to SSO
		std::free(ffi_allocated_string); // Free original
		return result;
	}
	else
	{
		MidoriText result;
		// Reset result to Long
		result.m_long.m_ptr = ffi_allocated_string;
		result.m_long.m_size = size;
		result.m_long.m_capacity = size;
		result.m_long.m_length_cache = -1;
		result.m_long.m_flag = 0;

		return result;
	}
}

size_t MidoriText::GetCapacity() const
{
	if (IsShort())
	{
		return 0uz;
	}
	return m_long.m_capacity;
}

void MidoriText::Expand(int new_size)
{
	// Convert Short to Long or Expand Long
	if (IsShort())
	{
		int current_size = GetShortSize();
		int new_capacity = std::max(new_size, current_size * 2);
		char* new_data = static_cast<char*>(std::malloc(new_capacity + 1));
		if (!new_data)
		{
			std::exit(EXIT_FAILURE);
		}
		
		std::memcpy(new_data, m_short.m_buffer, current_size + 1); // Copy data + null
		
		m_long.m_ptr = new_data;
		m_long.m_size = current_size;
		m_long.m_capacity = new_capacity;
		m_long.m_length_cache = -1;
		m_long.m_flag = 0;
	}
	else
	{
		// Should be handled by caller usually, but helper for generic expand
		// This method is private.
		// If already long, realloc.
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data)
			{
				std::exit(EXIT_FAILURE);
			}

			m_long.m_ptr = new_data;
			m_long.m_capacity = new_capacity;
		}
	}
}

bool MidoriText::IsShort() const noexcept
{
	return (m_short.m_size_flag & 1) != 0;
}

void MidoriText::SetShortSize(int size)
{
	m_short.m_size_flag = static_cast<uint8_t>((size << 1) | 1);
}

int MidoriText::GetShortSize() const
{
	return m_short.m_size_flag >> 1;
}

MidoriCellValue::MidoriCellValue(MidoriValue heap_value) noexcept
{
	m_data = heap_value;
	m_is_on_heap = true;
}

MidoriCellValue::MidoriCellValue(MidoriValue* stack_ref) noexcept
{
	std::memcpy(&m_data, &stack_ref, sizeof(void*));
	m_is_on_heap = false;
}

MidoriValue& MidoriCellValue::GetValue()
{
	if (m_is_on_heap)
	{
		return m_data;
	}
	else
	{
		return *GetStackPointer();
	}
}

MidoriValue* MidoriCellValue::GetStackPointer()
{
	MidoriValue* ptr;
	std::memcpy(&ptr, &m_data, sizeof(void*));
	return ptr;
}

MidoriFuture::MidoriFuture(MidoriClosure&& closure)
	: m_closure(std::make_unique<MidoriClosure>(std::move(closure)))
{
}

MidoriFuture::MidoriFuture(MidoriFuture&& other) noexcept
	: m_closure(std::move(other.m_closure)),
	m_result(other.m_result),
	m_completed(other.m_completed.load(std::memory_order_acquire)),
	m_has_error(other.m_has_error.load(std::memory_order_acquire))
{
}

MidoriFuture& MidoriFuture::operator=(MidoriFuture&& other) noexcept
{
	if (this != &other)
	{
		m_closure = std::move(other.m_closure);
		m_result = other.m_result;
		m_completed.store(other.m_completed.load(std::memory_order_acquire), std::memory_order_release);
		m_has_error.store(other.m_has_error.load(std::memory_order_acquire), std::memory_order_release);
	}
	return *this;
}

void MidoriFuture::SetResult(MidoriValue value)
{
	m_result = value;
	m_completed.store(true, std::memory_order_release);
}

void MidoriFuture::SetError()
{
	m_has_error.store(true, std::memory_order_release);
	m_completed.store(true, std::memory_order_release);
}

MidoriValue MidoriFuture::Get()
{
	while (!m_completed.load(std::memory_order_acquire))
	{
		std::this_thread::yield();
	}
	return m_result;
}

bool MidoriFuture::IsReady() const
{
	return m_completed.load(std::memory_order_acquire);
}

uintptr_t MidoriTaggedPointer::Encode(MidoriTraceable* ptr, PointerTag tag)
{
	assert((reinterpret_cast<uintptr_t>(ptr) & ALIGNMENT_MASK) == 0 && "Pointer not properly aligned");
	return reinterpret_cast<uintptr_t>(ptr) | static_cast<uintptr_t>(tag);
}

MidoriTraceable* MidoriTaggedPointer::Decode(uintptr_t value)
{
	return reinterpret_cast<MidoriTraceable*>(value & TAG_MASK);
}

MidoriTaggedPointer::MidoriTaggedPointer(MidoriTraceable* ptr, PointerTag tag)
	: m_raw_pointer(Encode(ptr, tag))
{
}

MidoriTraceable* MidoriTaggedPointer::operator->() const
{
	return Decode(m_raw_pointer);
}

MidoriTaggedPointer::operator MidoriTraceable* () const
{
	return Decode(m_raw_pointer);
}