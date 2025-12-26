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
	: m_data{ .m_integer = 0 }
#if MIDORI_DEBUG_INFO
	, m_tag(UNIT)
#endif
{
}

MidoriValue::MidoriValue(MidoriFloat midori_float) noexcept
	: m_data{ .m_float = midori_float }
#if MIDORI_DEBUG_INFO
	, m_tag(FLOAT)
#endif
{
}

MidoriValue::MidoriValue(MidoriInteger integer) noexcept
	: m_data{ .m_integer = integer }
#if MIDORI_DEBUG_INFO
	, m_tag(INT)
#endif
{
}

MidoriValue::MidoriValue(MidoriByte byte) noexcept
	: m_data{ .m_integer = static_cast<MidoriInteger>(byte) }
#if MIDORI_DEBUG_INFO
	, m_tag(BYTE)
#endif
{
}

MidoriValue::MidoriValue(MidoriWord word) noexcept
	: m_data{ .m_integer = static_cast<MidoriInteger>(word) }
#if MIDORI_DEBUG_INFO
	, m_tag(WORD)
#endif
{
}

MidoriValue::MidoriValue(MidoriBool b) noexcept
	: m_data{ .m_bool = b }
#if MIDORI_DEBUG_INFO
	, m_tag(BOOL)
#endif
{
}

MidoriValue::MidoriValue(MidoriTraceable* tagged_pointer) noexcept
	: m_data{ .m_pointer = tagged_pointer }
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
#endif

MidoriTraceable::MidoriTraceable(MidoriText&& str) noexcept : m_value(std::move(str))
{
}

MidoriTraceable::MidoriTraceable(MidoriArray&& array) noexcept : m_value(std::move(array))
{
}

MidoriTraceable::MidoriTraceable(MidoriRange&& range) noexcept : m_value(std::move(range))
{
}

MidoriTraceable::MidoriTraceable(MidoriCellValue&& cell_value) noexcept : m_value(std::move(cell_value))
{
}

MidoriTraceable::MidoriTraceable(MidoriClosure&& closure) noexcept : m_value(std::move(closure))
{
}

MidoriTraceable::MidoriTraceable(MidoriStruct&& midori_struct)noexcept : m_value(std::move(midori_struct))
{
}

MidoriTraceable::MidoriTraceable(MidoriUnion&& midori_union) noexcept : m_value(std::move(midori_union))
{
}

#if MIDORI_DEBUG_INFO
MidoriText MidoriTraceable::ToText()
{
	return std::visit([](auto&& arg) -> MidoriText
		{
			using T = std::decay_t<decltype(arg)>;
			if constexpr (std::is_same_v<T, MidoriText>)
			{
				return ConvertToQuotedText(arg);
			}
			else if constexpr (std::is_same_v<T, MidoriArray>)
			{
				if (arg.GetLength() == 0)
				{
					return MidoriText("[]");
				}

				MidoriText result("[");
				for (int idx : std::views::iota(0, arg.GetLength()))
				{
					result.Append(arg[idx].ToText()).Append(", ");
				}
				result.Pop().Pop().Append("]");
				return result;
			}
			else if constexpr (std::is_same_v<T, MidoriCellValue>)
			{
				return MidoriText("Cell(").Append(arg.GetValue().ToText()).Append(")");
			}
			else if constexpr (std::is_same_v<T, MidoriClosure>)
			{
				char buffer[64];
				std::snprintf(buffer, sizeof(buffer), "<closure at: %p>", (void*)std::addressof(arg));

				return MidoriText(buffer);
			}
			else if constexpr (std::is_same_v<T, MidoriUnion>)
			{
				if (arg.m_values.GetLength() == 0)
				{
					return MidoriText("Union{}");
				}

				MidoriText union_val("Union{");
				for (int idx : std::views::iota(0, arg.m_values.GetLength()))
				{
					union_val.Append(arg.m_values[idx].ToText()).Append(", ");
				}
				return union_val.Pop().Pop().Append("}");
			}
			else if constexpr (std::is_same_v<T, MidoriStruct>)
			{
				if (arg.m_values.GetLength() == 0)
				{
					return MidoriText("Struct{}");
				}

				MidoriText struct_val("Struct{");
				for (int idx : std::views::iota(0, arg.m_values.GetLength()))
				{
					struct_val.Append(arg.m_values[idx].ToText()).Append(", ");
				}
				return struct_val.Pop().Pop().Append("}");
			}
			else
			{
				return MidoriText("Unknown MidoriTraceable");
			}
		}, m_value);
}
#endif

size_t MidoriTraceable::GetSize() const
{
	return m_size;
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
	MidoriTraceable* traceable = static_cast<MidoriTraceable*>(object);

	traceable->m_size = size;

	return static_cast<void*>(traceable);
}

void MidoriTraceable::operator delete(void* object, size_t size) noexcept
{
	MidoriTraceable* traceable = static_cast<MidoriTraceable*>(object);
	(void)traceable;  // Unused but needed for potential future debugging

	::operator delete(object, size);
}

MidoriArray::MidoriArray(int size)
{
	m_size = size < 0 ? s_initial_capacity : size;
	m_end = m_size;

	m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(size) * sizeof(MidoriValue)));
	if (!m_data)
	{
		std::exit(EXIT_FAILURE);
	}
}

MidoriArray::MidoriArray(const MidoriArray& other) : m_size(other.m_size), m_end(other.m_end)
{
	m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_size) * sizeof(MidoriValue)));
	if (!m_data)
	{
		std::exit(EXIT_FAILURE);
	}
	std::memcpy(m_data, other.m_data, static_cast<size_t>(other.m_size) * sizeof(MidoriValue));
}

MidoriArray::MidoriArray(MidoriArray&& other) noexcept : m_data(other.m_data), m_size(other.m_size), m_end(other.m_end)
{
	other.m_data = nullptr;
	other.m_size = 0;
	other.m_end = 0;
}

MidoriArray& MidoriArray::operator=(const MidoriArray& other)
{
	if (this != &other)
	{
		MidoriValue* new_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(other.m_size) * sizeof(MidoriValue)));
		if (!new_data)
		{
			std::exit(EXIT_FAILURE);
		}
		std::memcpy(new_data, other.m_data, static_cast<size_t>(other.m_size) * sizeof(MidoriValue));

		std::free(m_data);
		m_data = new_data;
		m_size = other.m_size;
		m_end = other.m_end;
	}
	return *this;
}

MidoriArray& MidoriArray::operator=(MidoriArray&& other) noexcept
{
	if (this != &other)
	{
		std::free(m_data);
		m_data = other.m_data;
		m_size = other.m_size;
		m_end = other.m_end;

		other.m_data = nullptr;
		other.m_size = 0;
		other.m_end = 0;
	}
	return *this;
}

MidoriArray::~MidoriArray()
{
	std::free(m_data);
}

MidoriValue& MidoriArray::operator[](int index)
{
	return m_data[static_cast<size_t>(index)];
}

void MidoriArray::Expand()
{
	size_t new_size = m_size == 0u
		? s_initial_capacity
		: static_cast<size_t>(m_size) * 2u;
	MidoriValue* new_data = static_cast<MidoriValue*>(std::realloc(m_data, new_size * sizeof(MidoriValue)));
	if (!new_data)
	{
		std::exit(EXIT_FAILURE);
	}
	m_data = new_data;
	m_size = static_cast<int>(new_size);
}

std::optional<MidoriValue> MidoriArray::Pop()
{
	if (m_end > 0)
	{
		m_end -= 1;

		if (m_end < m_size / 2)
		{
			Shrink();
		}

		return std::optional<MidoriValue>(m_data[m_end]);
	}
	else
	{
		return std::nullopt;
	}
}

void MidoriArray::AddFront(const MidoriValue& value)
{
	if (m_end >= m_size)
	{
		Expand();
	}

	for (int i = m_end; i > 0; i -= 1)
	{
		size_t idx = static_cast<size_t>(i);
		m_data[idx] = m_data[idx - 1u];
	}

	m_data[0u] = value;
	m_end += 1;
}

void MidoriArray::Shrink()
{
	size_t new_size = static_cast<size_t>(m_size) / 2u;
	MidoriValue* new_data = static_cast<MidoriValue*>(std::realloc(m_data, new_size * sizeof(MidoriValue)));
	if (!new_data)
	{
		std::exit(EXIT_FAILURE);
	}
	m_data = new_data;
	m_size = static_cast<int>(new_size);
}

void MidoriArray::AddBack(const MidoriValue& value)
{
	if (m_end >= m_size)
	{
		Expand();
	}

	m_data[m_end] = value;
	m_end += 1;
}

int MidoriArray::GetLength() const
{
	return m_end;
}

MidoriArray MidoriArray::Concatenate(const MidoriArray& a, const MidoriArray& b)
{
	MidoriArray result(a.GetLength() + b.GetLength());
	std::memcpy(result.m_data, a.m_data, static_cast<size_t>(a.GetLength()) * sizeof(MidoriValue));
	std::memcpy(result.m_data + a.GetLength(), b.m_data, static_cast<size_t>(b.GetLength()) * sizeof(MidoriValue));
	result.m_end = a.GetLength() + b.GetLength();
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

	result.m_size = (length < s_initial_capacity) ? s_initial_capacity : length;
	result.m_data = static_cast<MidoriValue*>(std::malloc(static_cast<size_t>(result.m_size) * sizeof(MidoriValue)));
	result.m_end = length;

	std::memcpy(result.m_data, ffi_allocated_data, static_cast<size_t>(length) * sizeof(MidoriValue));
	std::free(ffi_allocated_data);

	return result;
}

MidoriRange::MidoriRange(MidoriValue start, MidoriValue end, MidoriValue step, bool is_float)
	: m_start(start), m_end(end), m_step(step), m_is_float(is_float)
{
}

MidoriValue MidoriRange::GetStart() const
{
	return m_start;
}

MidoriValue MidoriRange::GetEnd() const
{
	return m_end;
}

MidoriValue MidoriRange::GetStep() const
{
	return m_step;
}

bool MidoriRange::IsFloat() const
{
	return m_is_float;
}

MidoriText::MidoriText()
	: m_data(m_small_buffer),
	m_size(0),
	m_capacity(INLINE_THRESHOLD)
{
	m_small_buffer[0u] = '\0';
}

MidoriText::MidoriText(const char* str)
{
	if (!str)
	{
		m_data = m_small_buffer;
		m_size = 0;
		m_capacity = INLINE_THRESHOLD;
		m_small_buffer[0u] = '\0';
	}
	else
	{
		m_size = static_cast<int>(std::strlen(str));
		if (m_size <= INLINE_THRESHOLD)
		{
			m_data = m_small_buffer;
			m_capacity = INLINE_THRESHOLD;
		}
		else
		{
			m_capacity = m_size;
			m_data = static_cast<char*>(std::malloc(m_capacity + 1));
		}
		std::memcpy(m_data, str, sizeof(char) * m_size);
		m_data[m_size] = '\0';
	}
}

MidoriText::MidoriText(const MidoriText& other)
	: m_size(other.m_size)
{
	if (other.IsInlined())
	{
		m_data = m_small_buffer;
		m_capacity = INLINE_THRESHOLD;
	}
	else
	{
		m_capacity = other.m_capacity;
		m_data = static_cast<char*>(std::malloc(m_capacity + 1));
	}
	std::memcpy(m_data, other.m_data, sizeof(char) * m_size);
	m_data[m_size] = '\0';
}

MidoriText::MidoriText(MidoriText&& other) noexcept
	: m_size(other.m_size),
	m_capacity(other.m_capacity)
{
	if (other.IsInlined())
	{
		m_data = m_small_buffer;
		m_capacity = INLINE_THRESHOLD;
		std::memcpy(m_small_buffer, other.m_small_buffer, sizeof(char) * (m_size + 1));
	}
	else
	{
		m_data = other.m_data;
		other.m_data = other.m_small_buffer;
		other.m_capacity = INLINE_THRESHOLD;
	}
	other.m_size = 0;
	other.m_small_buffer[0u] = '\0';
}

MidoriText& MidoriText::operator=(const MidoriText& other)
{
	if (this == &other)
	{
		return *this;
	}

	m_size = other.m_size;
	if (other.IsInlined())
	{
		if (!IsInlined())
		{
			std::free(m_data);
		}
		m_data = m_small_buffer;
		m_capacity = INLINE_THRESHOLD;
	}
	else
	{
		if (other.m_size > m_capacity)
		{
			if (!IsInlined())
			{
				std::free(m_data);
			}
			m_capacity = other.m_capacity;
			m_data = static_cast<char*>(std::malloc(m_capacity + 1));
		}
	}
	std::memcpy(m_data, other.m_data, sizeof(char) * m_size);
	m_data[m_size] = '\0';
	return *this;
}

MidoriText& MidoriText::operator=(MidoriText&& other) noexcept
{
	if (this == &other)
	{
		return *this;
	}

	if (!IsInlined())
	{
		std::free(m_data);
	}

	m_size = other.m_size;
	m_capacity = other.m_capacity;
	if (other.IsInlined())
	{
		m_data = m_small_buffer;
		m_capacity = INLINE_THRESHOLD;
		std::memcpy(m_small_buffer, other.m_small_buffer, sizeof(char) * (m_size + 1));
	}
	else
	{
		m_data = other.m_data;
		other.m_data = other.m_small_buffer;
		other.m_capacity = INLINE_THRESHOLD;
	}
	other.m_size = 0;
	other.m_small_buffer[0u] = '\0';
	return *this;
}

MidoriText::~MidoriText()
{
	if (!IsInlined() && m_data != nullptr)
	{
		std::free(m_data);
	}
}

int MidoriText::GetLength() const noexcept
{
	return UTF8::CountCodePoints(m_data, m_size);
}

int MidoriText::GetByteLength() const noexcept
{
	return m_size;
}

const char* MidoriText::GetCString() const noexcept
{
	return m_data;
}

MidoriText& MidoriText::Pop()
{
	if (m_size > 0)
	{
		int new_size = UTF8::StepBackward(m_data, m_size);
		m_size = new_size;
		m_data[static_cast<size_t>(m_size)] = '\0';
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

	int new_size = m_size + len;
	if (new_size > m_capacity) 
	{
		int new_capacity = std::max(new_size, m_capacity * 2);
		Expand(new_capacity);
	}
	std::memcpy(m_data + m_size, str, sizeof(char) * len);
	m_size = new_size;
	m_data[static_cast<size_t>(m_size)] = '\0';
	return *this;
}

MidoriText& MidoriText::Append(char c) 
{
	int new_size = m_size + 1;
	if (new_size > m_capacity) 
	{
		int new_capacity = std::max(new_size, m_capacity * 2);
		Expand(new_capacity);
	}
	m_data[static_cast<size_t>(m_size)] = c;
	m_size = new_size;
	m_data[static_cast<size_t>(m_size)] = '\0';
	return *this;
}

MidoriText& MidoriText::Append(const MidoriText& other)
{
	return Append(other.GetCString());
}

MidoriText& MidoriText::Prepend(const char* str)
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

	int new_size = m_size + len;
	if (new_size > m_capacity)
	{
		int new_capacity = std::max(new_size, m_capacity * 2);
		Expand(new_capacity);
	}
 
	std::memmove(m_data + len, m_data, sizeof(char) * m_size); 	// Shift existing data to the right
	std::memcpy(m_data, str, sizeof(char) * len); 	// Copy new data at the beginning
	m_size = new_size;
	m_data[static_cast<size_t>(m_size)] = '\0';
	return *this;
}

MidoriText& MidoriText::Prepend(char c)
{
	int new_size = m_size + 1;
	if (new_size > m_capacity)
	{
		int new_capacity = std::max(new_size, m_capacity * 2);
		Expand(new_capacity);
	}


	std::memmove(m_data + 1, m_data, sizeof(char) * m_size);	// Shift existing data to the right
	m_data[0] = c; 	// Put new character at the beginning
	m_size = new_size;
	m_data[static_cast<size_t>(m_size)] = '\0';
	return *this;
}

MidoriText& MidoriText::Prepend(const MidoriText& other)
{
	return Prepend(other.GetCString());
}

char MidoriText::operator[](int index) const
{
	// Find the byte offset of the nth code point
	int byte_offset = UTF8::GetByteOffsetOfCodePoint(m_data, m_size, index);
	return m_data[static_cast<size_t>(byte_offset)];
}

bool MidoriText::operator==(const MidoriText& other) const 
{
	return (m_size == other.m_size) && (m_size == 0 || std::memcmp(m_data, other.m_data, sizeof(char) * m_size) == 0);
}

bool MidoriText::operator!=(const MidoriText& other) const
{
	return !(*this == other);
}

MidoriInteger MidoriText::ToInteger() const
{
	return std::atoll(m_data);
}

MidoriFloat MidoriText::ToFloat() const
{
	return std::atof(m_data);
}

MidoriText MidoriText::FromInteger(MidoriInteger value)
{
	// 21 characters is the maximum length of a 64-bit integer
	char buffer[21];
	std::snprintf(buffer, 21, "%lld", value);
	return MidoriText(buffer);
}

MidoriText MidoriText::FromFloat(MidoriFloat value)
{
	// 32 characters is the maximum length of a 64-bit floating point number
	char buffer[32];
	std::snprintf(buffer, 32, "%f", value);
	return MidoriText(buffer);
}

MidoriText MidoriText::Concatenate(const MidoriText& a, const MidoriText& b)
{
	int byte_len_a = a.GetByteLength();
	int byte_len_b = b.GetByteLength();
	int total_byte_len = byte_len_a + byte_len_b;

	MidoriText result;
	if (total_byte_len > INLINE_THRESHOLD)
	{
		result.m_data = static_cast<char*>(std::malloc(total_byte_len + 1));
		result.m_capacity = total_byte_len;
	}

	std::memcpy(result.m_data, a.GetCString(), sizeof(char) * byte_len_a);
	std::memcpy(result.m_data + byte_len_a, b.GetCString(), sizeof(char) * byte_len_b);
	result.m_data[total_byte_len] = '\0';
	result.m_size = total_byte_len;
	return result;
}

MidoriText MidoriText::FromFFI(char* ffi_allocated_string)
{
	if (ffi_allocated_string == nullptr)
	{
		return MidoriText();
	}

	MidoriText result(ffi_allocated_string);
	std::free(ffi_allocated_string);
	return result;
}

void MidoriText::Expand(int new_size) 
{
	if (new_size <= m_capacity) 
	{
		return;
	}

	char* new_data = static_cast<char*>(std::realloc(IsInlined() ? nullptr : m_data, new_size + 1));
	if (!new_data) 
	{
		std::exit(EXIT_FAILURE);
	}

	if (IsInlined()) 
	{
		std::memcpy(new_data, m_data, sizeof(char) * m_size);
	}

	m_data = new_data;
	m_capacity = new_size;
}

bool MidoriText::IsInlined() const noexcept
{
	return m_data == m_small_buffer;
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
