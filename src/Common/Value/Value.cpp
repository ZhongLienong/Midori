#include "Common/Printer/Printer.h"
#include "Value.h"

#include <algorithm>
#include <bit>
#include <execution>
#include <ranges>

MidoriText ConvertToQuotedText(const MidoriText& input)
{
	MidoriText result("\"");

	for (int i = 0; i < input.GetLength(); i += 1)
	{
		char c = input[i];
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
			result.Append(c);
		}
		}
	}

	result.Append('"');

	return result;
}

MidoriValue::MidoriValue(MidoriFloat midori_float) noexcept
{
#ifdef DEBUG
	m_tag = FLOAT;
#endif
	std::memcpy(m_data, &midori_float, sizeof(void*));
}

MidoriValue::MidoriValue(MidoriInteger integer) noexcept
{
#ifdef DEBUG
	m_tag = INT;
#endif
	std::memcpy(m_data, &integer, sizeof(void*));
}

MidoriValue::MidoriValue(MidoriBool b) noexcept
{
#ifdef DEBUG
	m_tag = BOOL;
#endif
	std::memset(m_data, b ? 1 : 0, sizeof(void*));
}

MidoriValue::MidoriValue(MidoriTraceable* tagged_pointer) noexcept
{
#ifdef DEBUG
	m_tag = POINTER;
#endif
	std::memcpy(m_data, &tagged_pointer, sizeof(void*));
}

MidoriFloat MidoriValue::GetFloat() const noexcept
{
	return std::bit_cast<MidoriFloat>(m_data);
}

MidoriInteger MidoriValue::GetInteger() const noexcept
{
	return std::bit_cast<MidoriInteger>(m_data);
}

MidoriUnit MidoriValue::GetUnit() const noexcept
{
	return {};
}

MidoriBool MidoriValue::GetBool() const noexcept
{
	return std::bit_cast<MidoriInteger>(m_data) != 0;
}

MidoriTraceable* MidoriValue::GetPointer() const noexcept
{
	return (MidoriTraceable*)(*reinterpret_cast<const MidoriTaggedPointer*>(m_data));
}

#ifdef DEBUG
MidoriText MidoriValue::ToText() const
{
	switch (m_tag)
	{
	case MidoriValue::FLOAT:
		return MidoriText::FromFloat(GetFloat());
	case MidoriValue::INT:
		return MidoriText::FromInteger(GetInteger());
	case MidoriValue::BOOL:
		return GetBool() ? "true" : "false";
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

MidoriTraceable::MidoriTraceable(MidoriBox&& box) noexcept : m_value(std::move(box))
{
}

#ifdef DEBUG
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
				union_val.Append("{");

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
				struct_val.Append("{");
				for (int idx : std::views::iota(0, arg.m_values.GetLength()))
				{
					struct_val.Append(arg.m_values[idx].ToText()).Append(", ");
				}
				return struct_val.Pop().Pop().Append("}");
			}
			else if constexpr (std::is_same_v<T, MidoriBox>)
			{
				MidoriText result("Box(");

				switch (arg.m_tag)
				{
				case MidoriBox::FLOAT:
					return result.Append(MidoriText::FromFloat(arg.m_inner_value.GetFloat())).Append(")");
				case MidoriBox::INT:
					return result.Append(MidoriText::FromInteger(arg.m_inner_value.GetInteger())).Append(")");
				case MidoriBox::BOOL:
					return result.Append(arg.m_inner_value.GetBool() ? "true" : "false").Append(")");
				case MidoriBox::UNIT:
					return result.Append("()").Append(")");
				default:
					return "";
				}
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

	::operator delete(object, size);
}

std::optional<MidoriBox> MidoriTraceable::UnBox()
{
	if (IsTraceable<MidoriBox>())
	{
		return GetTraceable<MidoriBox>();
	}
	else
	{
		return std::nullopt;
	}
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
	if (!IsInlined()) 
	{
		std::free(m_data);
	}
}

int MidoriText::GetLength() const noexcept
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
		m_size -= 1;
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

char MidoriText::operator[](int index) const
{
	return m_data[static_cast<size_t>(index)];
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
	int len_a = a.GetLength();
	int len_b = b.GetLength();
	int total_len = len_a + len_b;

	MidoriText result;
	if (total_len > INLINE_THRESHOLD) 
	{
		result.m_data = static_cast<char*>(std::malloc(total_len + 1));
		result.m_capacity = total_len;
	}

	std::memcpy(result.m_data, a.GetCString(), sizeof(char) * len_a);
	std::memcpy(result.m_data + len_a, b.GetCString(), sizeof(char) * len_b);
	result.m_data[total_len] = '\0';
	result.m_size = total_len;
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
	m_value.m_heap_value = heap_value;
	m_is_on_heap = true;
}

MidoriCellValue::MidoriCellValue(MidoriValue* stack_ref) noexcept
{
	m_value.m_stack_value_ref = stack_ref;
	m_is_on_heap = false;
}

MidoriValue& MidoriCellValue::GetValue()
{
	return m_is_on_heap ? m_value.m_heap_value : *m_value.m_stack_value_ref;
}
