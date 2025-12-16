#ifdef __EMSCRIPTEN__

#include "Library/MidoriStdLib.h"
#include "Library/MidoriStdLibExports.h"

#include <algorithm>
#include <array>
#include <ranges>
#include <string_view>

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

		static constexpr std::array<FunctionEntry, 13u> functions =
		{ {
			FunctionEntry{ "Print", &::Print },
			FunctionEntry{ "OverwriteToFile", &::OverwriteToFile },
			FunctionEntry{ "AppendToFile", &::AppendToFile },
			FunctionEntry{ "FileExists", &::FileExists },
			FunctionEntry{ "DeleteFile", &::DeleteFile },
			FunctionEntry{ "OpenWriteFile", &::OpenWriteFile },
			FunctionEntry{ "OpenAppendFile", &::OpenAppendFile },
			FunctionEntry{ "CloseFileHandle", &::CloseFileHandle },
			FunctionEntry{ "FlushFileHandle", &::FlushFileHandle },
			FunctionEntry{ "WriteToFileHandle", &::WriteToFileHandle },
			FunctionEntry{ "WriteLineToFileHandle", &::WriteLineToFileHandle },
			FunctionEntry{ "SquareRoot", &::SquareRoot },
			FunctionEntry{ "GetTime", &::GetTime }
		} };

		const std::array<FunctionEntry, 13u>::const_iterator it =
			std::ranges::find_if(functions, [target](const FunctionEntry& entry) { return entry.m_name == target; });

		return it != functions.cend() ? it->m_function : nullptr;
	}
}

#endif // __EMSCRIPTEN__
