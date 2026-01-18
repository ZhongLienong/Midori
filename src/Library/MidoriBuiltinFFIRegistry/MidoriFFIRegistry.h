#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <string_view>

using FFIFunction = void(*)(void** args, void* ret);

struct FFIEntry
{
	const char* m_name;
	FFIFunction m_function;
};

class MidoriFFIRegistry
{
public:
	static constexpr size_t BUILTIN_COUNT = 74u;

	static const FFIEntry& GetEntry(size_t index);
	static std::optional<size_t> FindIndex(std::string_view name);
	static constexpr size_t GetTableSize();
	static const std::array<FFIEntry, BUILTIN_COUNT>& GetTable();

private:
	static const std::array<FFIEntry, BUILTIN_COUNT> s_entries;
};
