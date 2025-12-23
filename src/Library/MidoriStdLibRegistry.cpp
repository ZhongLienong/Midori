#ifdef __EMSCRIPTEN__

#include "Library/MidoriStdLib.h"
#include "Library/MidoriStdLibExports.h"

#include <algorithm>
#include <array>
#include <ranges>
#include <string_view>

#define MIDORI_FFI_ENTRY(name) FunctionEntry{ "MIDORI_FFI_" #name, &::MIDORI_FFI_FUNC(name) }

namespace MidoriStdLib
{
	struct FunctionEntry
	{
		std::string_view m_name;
		ForeignFunction m_function;
	};

	ForeignFunction GetFunction(const char* name) noexcept
	{
		const std::string_view target(name == nullptr ? "" : name);

		static constexpr std::array<FunctionEntry, 9u> functions =
		{ 
			{
				MIDORI_FFI_ENTRY(Print),
				MIDORI_FFI_ENTRY(PrintError),
				MIDORI_FFI_ENTRY(ReadInput),
				MIDORI_FFI_ENTRY(ReadLine),
				MIDORI_FFI_ENTRY(ReadFile),
				MIDORI_FFI_ENTRY(WriteFile),
				MIDORI_FFI_ENTRY(AppendToFile),
				MIDORI_FFI_ENTRY(SquareRoot),
				MIDORI_FFI_ENTRY(GetTime)
			} 
		};

		const decltype(functions)::const_iterator it it = std::ranges::find_if(functions, [target](const FunctionEntry& entry) { return entry.m_name == target; });

		return it != functions.cend() ? it->m_function : nullptr;
	}
}

#endif // __EMSCRIPTEN__
