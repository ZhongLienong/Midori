#include "Common/Printer/Printer.h"
#include "Value.h"
#include "Common/Error/Error.h"

#include <algorithm>
#include <bit>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <charconv>
#include <execution>
#include <ranges>
#include <string>

namespace
{
	[[noreturn]] void FatalOutOfMemory(const char* context, size_t bytes) noexcept
	{
		std::string message = "Out of memory while allocating " + std::to_string(bytes) + " bytes";
		if (context && *context != '\0')
		{
			message.append(" (").append(context).append(")");
		}
		message.push_back('.');

		std::string rendered = MidoriError::GenerateRuntimeError(message, 0);
		std::fputs(rendered.c_str(), stderr);
		std::fputc('\n', stderr);
		std::fflush(stderr);
		std::exit(EXIT_FAILURE);
	}
}

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
	const int byte_len = input.GetByteLength();
	MidoriText result;
	result.Reserve(byte_len + 2);
	result.Append('\"');

	// Iterate over bytes to handle escape sequences (which are ASCII)
	const char* str = input.GetCString();
	for (int i = 0; i < byte_len; i += 1)
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

	result.Append('\"');

	return result;
}

MidoriValue::MidoriValue() noexcept
	: m_data{.m_integer = 0}
#if MIDORI_DEBUG_FULL
	, m_tag(UNIT)
#endif
{
}

MidoriValue::MidoriValue(MidoriFloat midori_float) noexcept
	: m_data{.m_float = midori_float}
#if MIDORI_DEBUG_FULL
	, m_tag(FLOAT)
#endif
{
}

MidoriValue::MidoriValue(MidoriInteger integer) noexcept
	: m_data{.m_integer = integer}
#if MIDORI_DEBUG_FULL
	, m_tag(INT)
#endif
{
}

MidoriValue::MidoriValue(MidoriByte byte) noexcept
	: m_data{.m_integer = static_cast<MidoriInteger>(byte)}
#if MIDORI_DEBUG_FULL
	, m_tag(BYTE)
#endif
{
}

MidoriValue::MidoriValue(MidoriWord word) noexcept
	: m_data{.m_integer = static_cast<MidoriInteger>(word)}
#if MIDORI_DEBUG_FULL
	, m_tag(WORD)
#endif
{
}

MidoriValue::MidoriValue(MidoriBool b) noexcept
	: m_data{.m_bool = b}
#if MIDORI_DEBUG_FULL
	, m_tag(BOOL)
#endif
{
}

MidoriValue::MidoriValue(MidoriTraceable* tagged_pointer) noexcept
	: m_data{.m_pointer = tagged_pointer}
#if MIDORI_DEBUG_FULL
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
	return m_data.m_pointer;
}

const void* MidoriValue::GetRawDataPtr() const noexcept
{
	return &m_data;
}

#if MIDORI_DEBUG_FULL
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

#if MIDORI_DEBUG_FULL
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

		const int len = m_array.GetLength();
		MidoriText result("[");
		result.Append(m_array[0].ToText());
		for (int idx = 1; idx < len; idx += 1)
		{
			result.Append(", ");
			result.Append(m_array[idx].ToText());
		}
		result.Append("]");
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

		const int len = m_union.m_values.GetLength();
		MidoriText union_val("Union{");
		union_val.Append(m_union.m_values[0].ToText());
		for (int idx = 1; idx < len; idx += 1)
		{
			union_val.Append(", ");
			union_val.Append(m_union.m_values[idx].ToText());
		}
		union_val.Append("}");
		return union_val;
	}
	case TraceableType::Struct:
	{
		if (m_struct.m_values.GetLength() == 0)
		{
			return MidoriText("Struct{}");
		}

		const int len = m_struct.m_values.GetLength();
		MidoriText struct_val("Struct{");
		struct_val.Append(m_struct.m_values[0].ToText());
		for (int idx = 1; idx < len; idx += 1)
		{
			struct_val.Append(", ");
			struct_val.Append(m_struct.m_values[idx].ToText());
		}
		struct_val.Append("}");
		return struct_val;
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

MidoriArray::MidoriArray()
{
	std::memset(this, 0, sizeof(MidoriArray));
	SetShortSize(0);
}

MidoriArray::MidoriArray(int size)
{
	if (size <= SOO_CAPACITY)
	{
		std::memset(this, 0, sizeof(MidoriArray));
		SetShortSize(size);
		for (int i = 0; i < size; i += 1)
		{
			new (&m_short.m_buffer[i]) MidoriValue();
		}
	}
	else
	{
		m_long.m_ptr = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(size) * sizeof(MidoriValue)));
		if (!m_long.m_ptr)
		{
			FatalOutOfMemory("MidoriArray::MidoriArray", static_cast<size_t>(size) * sizeof(MidoriValue));
		}
		m_long.m_size = size;
		m_long.m_capacity = size;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
		for (int i = 0; i < size; i += 1)
		{
			new (&m_long.m_ptr[i]) MidoriValue();
		}
	}
}

MidoriArray::MidoriArray(const MidoriArray& other)
{
	if (other.IsShort())
	{
		std::memcpy(this, &other, sizeof(MidoriArray));
	}
	else
	{
		m_long.m_ptr = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_long.m_capacity) * sizeof(MidoriValue)));
		if (!m_long.m_ptr)
		{
			FatalOutOfMemory("MidoriArray::MidoriArray copy", static_cast<size_t>(other.m_long.m_capacity) * sizeof(MidoriValue));
		}
		std::memcpy(m_long.m_ptr, other.m_long.m_ptr, static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue));
		m_long.m_size = other.m_long.m_size;
		m_long.m_capacity = other.m_long.m_capacity;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
	}
}

MidoriArray::MidoriArray(MidoriArray&& other) noexcept
{
	std::memcpy(this, &other, sizeof(MidoriArray));
	std::memset(&other, 0, sizeof(MidoriArray));
	other.SetShortSize(0);
}

MidoriArray& MidoriArray::operator=(const MidoriArray& other)
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
		std::memcpy(this, &other, sizeof(MidoriArray));
	}
	else
	{
		m_long.m_ptr = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_long.m_capacity) * sizeof(MidoriValue)));
		if (!m_long.m_ptr)
		{
			FatalOutOfMemory("MidoriArray::operator= copy", static_cast<size_t>(other.m_long.m_capacity) * sizeof(MidoriValue));
		}
		std::memcpy(m_long.m_ptr, other.m_long.m_ptr, static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue));
		m_long.m_size = other.m_long.m_size;
		m_long.m_capacity = other.m_long.m_capacity;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
	}
	return *this;
}

MidoriArray& MidoriArray::operator=(MidoriArray&& other) noexcept
{
	if (this == &other)
	{
		return *this;
	}

	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}

	std::memcpy(this, &other, sizeof(MidoriArray));
	std::memset(&other, 0, sizeof(MidoriArray));
	other.SetShortSize(0);
	return *this;
}

MidoriArray::~MidoriArray()
{
	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}
}

MidoriValue& MidoriArray::operator[](int index)
{
	return IsShort() ? m_short.m_buffer[index] : m_long.m_ptr[index];
}

const MidoriValue& MidoriArray::operator[](int index) const
{
	return IsShort() ? m_short.m_buffer[index] : m_long.m_ptr[index];
}

void MidoriArray::Expand(int new_capacity)
{
	if (IsShort())
	{
		int current_size = GetShortSize();
		int capacity = new_capacity > 0 ? new_capacity : (current_size < s_initial_capacity ? s_initial_capacity : current_size * 2);

		MidoriValue* new_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(capacity) * sizeof(MidoriValue)));
		if (!new_data)
		{
			FatalOutOfMemory("MidoriArray::Expand", static_cast<size_t>(capacity) * sizeof(MidoriValue));
		}

		std::memcpy(new_data, m_short.m_buffer, static_cast<size_t>(current_size) * sizeof(MidoriValue));

		m_long.m_ptr = new_data;
		m_long.m_size = current_size;
		m_long.m_capacity = capacity;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
	}
	else
	{
		int capacity = new_capacity > 0 ? new_capacity : (m_long.m_capacity == 0 ? s_initial_capacity : m_long.m_capacity * 2);
		MidoriValue* new_data = static_cast<MidoriValue*>(std::realloc(m_long.m_ptr, static_cast<size_t>(capacity) * sizeof(MidoriValue)));
		if (!new_data)
		{
			FatalOutOfMemory("MidoriArray::Expand", static_cast<size_t>(capacity) * sizeof(MidoriValue));
		}
		m_long.m_ptr = new_data;
		m_long.m_capacity = capacity;
	}
}

std::optional<MidoriValue> MidoriArray::Pop()
{
	int len = GetLength();
	if (len > 0)
	{
		MidoriValue val;
		if (IsShort())
		{
			val = m_short.m_buffer[len - 1];
			SetShortSize(len - 1);
		}
		else
		{
			val = m_long.m_ptr[len - 1];
			m_long.m_size -= 1;
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
	int len = GetLength();
	
	bool needs_expand = false;
	if (IsShort())
	{
		if (len >= SOO_CAPACITY) needs_expand = true;
	}
	else
	{
		if (len >= m_long.m_capacity) needs_expand = true;
	}

	if (needs_expand)
	{
		Expand(0);
	}

	// Move elements
	if (IsShort())
	{
		std::memmove(m_short.m_buffer + 1, m_short.m_buffer, static_cast<size_t>(len) * sizeof(MidoriValue));
		m_short.m_buffer[0] = value;
		SetShortSize(len + 1);
	}
	else
	{
		std::memmove(m_long.m_ptr + 1, m_long.m_ptr, static_cast<size_t>(len) * sizeof(MidoriValue));
		m_long.m_ptr[0] = value;
		m_long.m_size += 1;
	}
}

void MidoriArray::AddBack(const MidoriValue& value)
{
	if (IsShort())
	{
		int len = GetShortSize();
		if (len < SOO_CAPACITY)
		{
			m_short.m_buffer[len] = value;
			SetShortSize(len + 1);
		}
		else
		{
			Expand(0);
			m_long.m_ptr[m_long.m_size] = value;
			m_long.m_size += 1;
		}
	}
	else
	{
		if (m_long.m_size >= m_long.m_capacity)
		{
			Expand(0);
		}
		m_long.m_ptr[m_long.m_size] = value;
		m_long.m_size += 1;
	}
}

void MidoriArray::Extend(const MidoriArray& other)
{
	int other_len = other.GetLength();
	if (other_len == 0)
	{
		return;
	}

	int current_len = GetLength();
	int new_len = current_len + other_len;

	if (IsShort())
	{
		if (new_len <= SOO_CAPACITY)
		{
			const MidoriValue* other_data = other.IsShort() ? other.m_short.m_buffer : other.m_long.m_ptr;
			std::memcpy(m_short.m_buffer + current_len, other_data, static_cast<size_t>(other_len) * sizeof(MidoriValue));
			SetShortSize(new_len);
			return;
		}

		int new_capacity = std::max(new_len, s_initial_capacity);
		Expand(new_capacity);
	}
	else if (new_len > m_long.m_capacity)
	{
		int new_capacity = std::max(new_len, m_long.m_capacity * 2);
		Expand(new_capacity);
	}

	const MidoriValue* other_data = other.IsShort() ? other.m_short.m_buffer : other.m_long.m_ptr;
	std::memcpy(m_long.m_ptr + current_len, other_data, static_cast<size_t>(other_len) * sizeof(MidoriValue));
	m_long.m_size = new_len;
}

int MidoriArray::GetLength() const
{
	return IsShort() ? GetShortSize() : m_long.m_size;
}

size_t MidoriArray::GetCapacity() const
{
	return IsShort() ? static_cast<size_t>(SOO_CAPACITY) * sizeof(MidoriValue) : static_cast<size_t>(m_long.m_capacity) * sizeof(MidoriValue);
}

MidoriArray MidoriArray::Concatenate(const MidoriArray& a, const MidoriArray& b)
{
	int a_len = a.GetLength();
	int b_len = b.GetLength();
	int total_len = a_len + b_len;

	MidoriArray result;
	if (total_len <= SOO_CAPACITY)
	{
		result.SetShortSize(total_len);
		if (a.IsShort())
		{
			std::memcpy(result.m_short.m_buffer, a.m_short.m_buffer, a_len * sizeof(MidoriValue));
		}
		else
		{
			std::memcpy(result.m_short.m_buffer, a.m_long.m_ptr, a_len * sizeof(MidoriValue));
		}

		if (b.IsShort())
		{
			std::memcpy(result.m_short.m_buffer + a_len, b.m_short.m_buffer, b_len * sizeof(MidoriValue));
		}
		else
		{
			std::memcpy(result.m_short.m_buffer + a_len, b.m_long.m_ptr, b_len * sizeof(MidoriValue));
		}
	}
	else
	{
		result.Expand(total_len); 
		result.m_long.m_size = total_len;

		MidoriValue* dest = result.m_long.m_ptr;
		if (a.IsShort())
			std::memcpy(dest, a.m_short.m_buffer, a_len * sizeof(MidoriValue));
		else
			std::memcpy(dest, a.m_long.m_ptr, a_len * sizeof(MidoriValue));

		if (b.IsShort())
			std::memcpy(dest + a_len, b.m_short.m_buffer, b_len * sizeof(MidoriValue));
		else
			std::memcpy(dest + a_len, b.m_long.m_ptr, b_len * sizeof(MidoriValue));
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

	if (length <= SOO_CAPACITY)
	{
		result.SetShortSize(length);
		std::memcpy(result.m_short.m_buffer, ffi_allocated_data, static_cast<size_t>(length) * sizeof(MidoriValue));
		std::free(ffi_allocated_data);
	}
	else
	{
		result.m_long.m_ptr = ffi_allocated_data;
		result.m_long.m_size = length;
		result.m_long.m_capacity = length;
		result.m_long.m_flag = 0;
	}

	return result;
}

bool MidoriArray::IsShort() const noexcept
{
	return (m_short.m_size_flag & 1) != 0;
}

void MidoriArray::SetShortSize(int size)
{
	m_short.m_size_flag = static_cast<uint8_t>((size << 1) | 1);
}

int MidoriArray::GetShortSize() const
{
	return m_short.m_size_flag >> 1;
}

MidoriTuple::MidoriTuple()
{
	std::memset(this, 0, sizeof(MidoriTuple));
	SetShortSize(0);
}

MidoriTuple::MidoriTuple(int size)
{
	if (size <= SOO_CAPACITY)
	{
		std::memset(this, 0, sizeof(MidoriTuple));
		SetShortSize(size);
		for (int i = 0; i < size; i += 1)
		{
			new (&m_short.m_buffer[i]) MidoriValue();
		}
	}
	else
	{
		m_long.m_ptr = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(size) * sizeof(MidoriValue)));
		if (!m_long.m_ptr)
		{
			FatalOutOfMemory("MidoriTuple::MidoriTuple", static_cast<size_t>(size) * sizeof(MidoriValue));
		}
		m_long.m_size = size;
		m_long.m_capacity = size;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
	}
}

MidoriTuple::MidoriTuple(const MidoriTuple& other)
{
	if (other.IsShort())
	{
		std::memcpy(this, &other, sizeof(MidoriTuple));
	}
	else
	{
		m_long.m_ptr = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue)));
		if (!m_long.m_ptr)
		{
			FatalOutOfMemory("MidoriTuple::MidoriTuple copy", static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue));
		}
		std::memcpy(m_long.m_ptr, other.m_long.m_ptr, static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue));
		m_long.m_size = other.m_long.m_size;
		m_long.m_capacity = other.m_long.m_size;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
	}
}

MidoriTuple::MidoriTuple(MidoriTuple&& other) noexcept
{
	std::memcpy(this, &other, sizeof(MidoriTuple));
	std::memset(&other, 0, sizeof(MidoriTuple));
	other.SetShortSize(0);
}

MidoriTuple& MidoriTuple::operator=(const MidoriTuple& other)
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
		std::memcpy(this, &other, sizeof(MidoriTuple));
	}
	else
	{
		m_long.m_ptr = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue)));
		if (!m_long.m_ptr)
		{
			FatalOutOfMemory("MidoriTuple::operator= copy", static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue));
		}
		std::memcpy(m_long.m_ptr, other.m_long.m_ptr, static_cast<size_t>(other.m_long.m_size) * sizeof(MidoriValue));
		m_long.m_size = other.m_long.m_size;
		m_long.m_capacity = other.m_long.m_size;
		m_long.m_flag = 0;
		m_short.m_size_flag = 0;
	}
	return *this;
}

MidoriTuple& MidoriTuple::operator=(MidoriTuple&& other) noexcept
{
	if (this == &other)
	{
		return *this;
	}

	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}

	std::memcpy(this, &other, sizeof(MidoriTuple));
	std::memset(&other, 0, sizeof(MidoriTuple));
	other.SetShortSize(0);
	return *this;
}

MidoriTuple::~MidoriTuple()
{
	if (!IsShort())
	{
		std::free(m_long.m_ptr);
	}
}

MidoriValue& MidoriTuple::operator[](int index)
{
	return IsShort() ? m_short.m_buffer[index] : m_long.m_ptr[index];
}

const MidoriValue& MidoriTuple::operator[](int index) const
{
	return IsShort() ? m_short.m_buffer[index] : m_long.m_ptr[index];
}

int MidoriTuple::GetLength() const
{
	return IsShort() ? GetShortSize() : m_long.m_size;
}

size_t MidoriTuple::GetCapacity() const
{
	if (IsShort())
	{
		return 0uz;
	}
	return static_cast<size_t>(m_long.m_capacity) * sizeof(MidoriValue);
}

bool MidoriTuple::IsShort() const noexcept
{
	return (m_short.m_size_flag & 1) != 0;
}

void MidoriTuple::SetShortSize(int size)
{
	m_short.m_size_flag = static_cast<uint8_t>((size << 1) | 1);
}

int MidoriTuple::GetShortSize() const
{
	return m_short.m_size_flag >> 1;
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
				FatalOutOfMemory("MidoriText::MidoriText", static_cast<size_t>(size) + 1);
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
			FatalOutOfMemory("MidoriText::MidoriText copy", static_cast<size_t>(other.m_long.m_size) + 1);
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
			FatalOutOfMemory("MidoriText::operator= copy", static_cast<size_t>(other.m_long.m_size) + 1);
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
				FatalOutOfMemory("MidoriText::Append const char*", static_cast<size_t>(new_capacity) + 1);
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
				FatalOutOfMemory("MidoriText::Append char", static_cast<size_t>(new_capacity) + 1);
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
				FatalOutOfMemory("MidoriText::Append text", static_cast<size_t>(new_capacity) + 1);
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
			if (!new_data)
			{
				FatalOutOfMemory("MidoriText::Reserve", static_cast<size_t>(capacity) + 1);
			}
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
			int new_capacity = std::max(new_size, current_size * 2);
			char* new_data = static_cast<char*>(std::malloc(new_capacity + 1));
			if (!new_data)
			{
				FatalOutOfMemory("MidoriText::Prepend const char*", static_cast<size_t>(new_capacity) + 1);
			}
			
			std::memcpy(new_data, str, len);
			std::memcpy(new_data + len, m_short.m_buffer, current_size);
			new_data[new_size] = '\0';
			
			// Initialize Long
			m_long.m_ptr = new_data;
			m_long.m_size = new_size;
			m_long.m_capacity = new_capacity;
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
			if (!new_data)
			{
				FatalOutOfMemory("MidoriText::Prepend const char*", static_cast<size_t>(new_capacity) + 1);
			}
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
			int new_capacity = std::max(new_size, current_size * 2);
			char* new_data = static_cast<char*>(std::malloc(new_capacity + 1));
			if (!new_data)
			{
				FatalOutOfMemory("MidoriText::Prepend char", static_cast<size_t>(new_capacity) + 1);
			}
			
			new_data[0] = c;
			std::memcpy(new_data + 1, m_short.m_buffer, current_size);
			new_data[new_size] = '\0';
			
			m_long.m_ptr = new_data;
			m_long.m_size = new_size;
			m_long.m_capacity = new_capacity;
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
			if (!new_data)
			{
				FatalOutOfMemory("MidoriText::Prepend char", static_cast<size_t>(new_capacity) + 1);
			}
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
	char buffer[32];
	std::to_chars_result result = std::to_chars(std::begin(buffer), std::end(buffer), value);
	if (result.ec != std::errc())
	{
		return MidoriText();
	}
	*result.ptr = '\0';
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
		result.m_long.m_ptr = static_cast<char*>(std::malloc(total_byte_len + 1));
		result.m_long.m_size = total_byte_len;
		result.m_long.m_capacity = total_byte_len;
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
	return static_cast<size_t>(m_long.m_capacity) + 1uz;
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
			FatalOutOfMemory("MidoriText::Expand", static_cast<size_t>(new_capacity) + 1);
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
		if (new_size > m_long.m_capacity)
		{
			int new_capacity = std::max(new_size, m_long.m_capacity * 2);
			char* new_data = static_cast<char*>(std::realloc(m_long.m_ptr, new_capacity + 1));
			if (!new_data)
			{
				FatalOutOfMemory("MidoriText::Expand", static_cast<size_t>(new_capacity) + 1);
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
#if defined(__cpp_lib_atomic_wait) && (__cpp_lib_atomic_wait >= 201907L)
	m_completed.notify_all();
#endif
}

void MidoriFuture::SetError()
{
	m_has_error.store(true, std::memory_order_release);
	m_completed.store(true, std::memory_order_release);
#if defined(__cpp_lib_atomic_wait) && (__cpp_lib_atomic_wait >= 201907L)
	m_completed.notify_all();
#endif
}

MidoriValue MidoriFuture::Get()
{
#if defined(__cpp_lib_atomic_wait) && (__cpp_lib_atomic_wait >= 201907L)
	while (!m_completed.load(std::memory_order_acquire))
	{
		m_completed.wait(false, std::memory_order_acquire);
	}
#else
	while (!m_completed.load(std::memory_order_acquire))
	{
		std::this_thread::yield();
	}
#endif
	return m_result;
}

bool MidoriFuture::IsReady() const
{
	return m_completed.load(std::memory_order_acquire);
}
