#pragma once

#include "Library/MidoriStdLibExports.h"

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
private:
	inline static constexpr std::array s_entries = 
	{
		FFIEntry{ "MIDORI_FFI_Print", &MIDORI_FFI_Print },
		FFIEntry{ "MIDORI_FFI_PrintError", &MIDORI_FFI_PrintError },
		FFIEntry{ "MIDORI_FFI_ReadInput", &MIDORI_FFI_ReadInput },
		FFIEntry{ "MIDORI_FFI_ReadLine", &MIDORI_FFI_ReadLine },
		FFIEntry{ "MIDORI_FFI_ReadFile", &MIDORI_FFI_ReadFile },
		FFIEntry{ "MIDORI_FFI_WriteFile", &MIDORI_FFI_WriteFile },
		FFIEntry{ "MIDORI_FFI_AppendToFile", &MIDORI_FFI_AppendToFile },
		FFIEntry{ "MIDORI_FFI_ReadBinaryFile", &MIDORI_FFI_ReadBinaryFile },
		FFIEntry{ "MIDORI_FFI_WriteBinaryFile", &MIDORI_FFI_WriteBinaryFile },
		FFIEntry{ "MIDORI_FFI_FileExists", &MIDORI_FFI_FileExists },
		FFIEntry{ "MIDORI_FFI_DeleteFile", &MIDORI_FFI_DeleteFile },
		FFIEntry{ "MIDORI_FFI_RenameFile", &MIDORI_FFI_RenameFile },
		FFIEntry{ "MIDORI_FFI_GetFileSize", &MIDORI_FFI_GetFileSize },
		FFIEntry{ "MIDORI_FFI_SquareRoot", &MIDORI_FFI_SquareRoot },
		FFIEntry{ "MIDORI_FFI_Abs", &MIDORI_FFI_Abs },
		FFIEntry{ "MIDORI_FFI_Pow", &MIDORI_FFI_Pow },
		FFIEntry{ "MIDORI_FFI_Exp", &MIDORI_FFI_Exp },
		FFIEntry{ "MIDORI_FFI_Log", &MIDORI_FFI_Log },
		FFIEntry{ "MIDORI_FFI_Log10", &MIDORI_FFI_Log10 },
		FFIEntry{ "MIDORI_FFI_Log2", &MIDORI_FFI_Log2 },
		FFIEntry{ "MIDORI_FFI_Sin", &MIDORI_FFI_Sin },
		FFIEntry{ "MIDORI_FFI_Cos", &MIDORI_FFI_Cos },
		FFIEntry{ "MIDORI_FFI_Tan", &MIDORI_FFI_Tan },
		FFIEntry{ "MIDORI_FFI_Asin", &MIDORI_FFI_Asin },
		FFIEntry{ "MIDORI_FFI_Acos", &MIDORI_FFI_Acos },
		FFIEntry{ "MIDORI_FFI_Atan", &MIDORI_FFI_Atan },
		FFIEntry{ "MIDORI_FFI_Atan2", &MIDORI_FFI_Atan2 },
		FFIEntry{ "MIDORI_FFI_Sinh", &MIDORI_FFI_Sinh },
		FFIEntry{ "MIDORI_FFI_Cosh", &MIDORI_FFI_Cosh },
		FFIEntry{ "MIDORI_FFI_Tanh", &MIDORI_FFI_Tanh },
		FFIEntry{ "MIDORI_FFI_Floor", &MIDORI_FFI_Floor },
		FFIEntry{ "MIDORI_FFI_Ceil", &MIDORI_FFI_Ceil },
		FFIEntry{ "MIDORI_FFI_Round", &MIDORI_FFI_Round },
		FFIEntry{ "MIDORI_FFI_Trunc", &MIDORI_FFI_Trunc },
		FFIEntry{ "MIDORI_FFI_Min", &MIDORI_FFI_Min },
		FFIEntry{ "MIDORI_FFI_Max", &MIDORI_FFI_Max },
		FFIEntry{ "MIDORI_FFI_Clamp", &MIDORI_FFI_Clamp },
		FFIEntry{ "MIDORI_FFI_Sign", &MIDORI_FFI_Sign },
		FFIEntry{ "MIDORI_FFI_Fmod", &MIDORI_FFI_Fmod },
		FFIEntry{ "MIDORI_FFI_Hypot", &MIDORI_FFI_Hypot },
		FFIEntry{ "MIDORI_FFI_Random", &MIDORI_FFI_Random },
		FFIEntry{ "MIDORI_FFI_RandomInt", &MIDORI_FFI_RandomInt },
		FFIEntry{ "MIDORI_FFI_RandomFloat", &MIDORI_FFI_RandomFloat },
		FFIEntry{ "MIDORI_FFI_ToRadians", &MIDORI_FFI_ToRadians },
		FFIEntry{ "MIDORI_FFI_ToDegrees", &MIDORI_FFI_ToDegrees },
		FFIEntry{ "MIDORI_FFI_IsNaN", &MIDORI_FFI_IsNaN },
		FFIEntry{ "MIDORI_FFI_IsInf", &MIDORI_FFI_IsInf },
		FFIEntry{ "MIDORI_FFI_IsFinite", &MIDORI_FFI_IsFinite },
		FFIEntry{ "MIDORI_FFI_GetTime", &MIDORI_FFI_GetTime },
		FFIEntry{ "MIDORI_FFI_GetYear", &MIDORI_FFI_GetYear },
		FFIEntry{ "MIDORI_FFI_GetMonth", &MIDORI_FFI_GetMonth },
		FFIEntry{ "MIDORI_FFI_GetDay", &MIDORI_FFI_GetDay },
		FFIEntry{ "MIDORI_FFI_GetHour", &MIDORI_FFI_GetHour },
		FFIEntry{ "MIDORI_FFI_GetMinute", &MIDORI_FFI_GetMinute },
		FFIEntry{ "MIDORI_FFI_GetSecond", &MIDORI_FFI_GetSecond },
		FFIEntry{ "MIDORI_FFI_GetDayOfWeek", &MIDORI_FFI_GetDayOfWeek },
		FFIEntry{ "MIDORI_FFI_GetDayOfYear", &MIDORI_FFI_GetDayOfYear },
		FFIEntry{ "MIDORI_FFI_FormatTime", &MIDORI_FFI_FormatTime },
		FFIEntry{ "MIDORI_FFI_GetTimezoneOffset", &MIDORI_FFI_GetTimezoneOffset },
		FFIEntry{ "MIDORI_FFI_GetUtcYear", &MIDORI_FFI_GetUtcYear },
		FFIEntry{ "MIDORI_FFI_GetUtcMonth", &MIDORI_FFI_GetUtcMonth },
		FFIEntry{ "MIDORI_FFI_GetUtcDay", &MIDORI_FFI_GetUtcDay },
		FFIEntry{ "MIDORI_FFI_GetUtcHour", &MIDORI_FFI_GetUtcHour },
		FFIEntry{ "MIDORI_FFI_GetUtcMinute", &MIDORI_FFI_GetUtcMinute },
		FFIEntry{ "MIDORI_FFI_GetUtcSecond", &MIDORI_FFI_GetUtcSecond },
		FFIEntry{ "MIDORI_FFI_Exit", &MIDORI_FFI_Exit },
		FFIEntry{ "MIDORI_FFI_GetEnv", &MIDORI_FFI_GetEnv },
		FFIEntry{ "MIDORI_FFI_SetEnv", &MIDORI_FFI_SetEnv },
		FFIEntry{ "MIDORI_FFI_Sleep", &MIDORI_FFI_Sleep },
		FFIEntry{ "MIDORI_FFI_GetCurrentDirectory", &MIDORI_FFI_GetCurrentDirectory },
		FFIEntry{ "MIDORI_FFI_SetCurrentDirectory", &MIDORI_FFI_SetCurrentDirectory },
		FFIEntry{ "MIDORI_FFI_Execute", &MIDORI_FFI_Execute },
		FFIEntry{ "MIDORI_FFI_GetPlatform", &MIDORI_FFI_GetPlatform },
		FFIEntry{ "MIDORI_FFI_GetProcessId", &MIDORI_FFI_GetProcessId },
		FFIEntry{ "MIDORI_FFI_TextLength", &MIDORI_FFI_TextLength },
		FFIEntry{ "MIDORI_FFI_HashText", &MIDORI_FFI_HashText },
		FFIEntry{ "MIDORI_FFI_HashFloat", &MIDORI_FFI_HashFloat },
	};

public:
	static constexpr size_t BUILTIN_COUNT = s_entries.size();

	static const FFIEntry& GetEntry(size_t index);
	static std::optional<size_t> FindIndex(std::string_view name);
	static constexpr size_t GetTableSize();
	static const std::array<FFIEntry, BUILTIN_COUNT>& GetTable();
};
