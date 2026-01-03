#include "Library/MidoriFFIRegistry.h"
#include "Library/MidoriStdLibExports.h"

const std::array<FFIEntry, MidoriFFIRegistry::BUILTIN_COUNT> MidoriFFIRegistry::s_entries = {{
	// IO - Console
	{"MIDORI_FFI_Print", &MIDORI_FFI_Print},
	{"MIDORI_FFI_PrintError", &MIDORI_FFI_PrintError},
	{"MIDORI_FFI_ReadInput", &MIDORI_FFI_ReadInput},
	{"MIDORI_FFI_ReadLine", &MIDORI_FFI_ReadLine},
	// IO - File Operations
	{"MIDORI_FFI_ReadFile", &MIDORI_FFI_ReadFile},
	{"MIDORI_FFI_WriteFile", &MIDORI_FFI_WriteFile},
	{"MIDORI_FFI_AppendToFile", &MIDORI_FFI_AppendToFile},
	{"MIDORI_FFI_ReadBinaryFile", &MIDORI_FFI_ReadBinaryFile},
	{"MIDORI_FFI_WriteBinaryFile", &MIDORI_FFI_WriteBinaryFile},
	{"MIDORI_FFI_FileExists", &MIDORI_FFI_FileExists},
	{"MIDORI_FFI_DeleteFile", &MIDORI_FFI_DeleteFile},
	{"MIDORI_FFI_RenameFile", &MIDORI_FFI_RenameFile},
	{"MIDORI_FFI_GetFileSize", &MIDORI_FFI_GetFileSize},
	// Math - Basic
	{"MIDORI_FFI_SquareRoot", &MIDORI_FFI_SquareRoot},
	{"MIDORI_FFI_Abs", &MIDORI_FFI_Abs},
	{"MIDORI_FFI_Pow", &MIDORI_FFI_Pow},
	{"MIDORI_FFI_Exp", &MIDORI_FFI_Exp},
	{"MIDORI_FFI_Log", &MIDORI_FFI_Log},
	{"MIDORI_FFI_Log10", &MIDORI_FFI_Log10},
	{"MIDORI_FFI_Log2", &MIDORI_FFI_Log2},
	// Math - Trigonometric
	{"MIDORI_FFI_Sin", &MIDORI_FFI_Sin},
	{"MIDORI_FFI_Cos", &MIDORI_FFI_Cos},
	{"MIDORI_FFI_Tan", &MIDORI_FFI_Tan},
	{"MIDORI_FFI_Asin", &MIDORI_FFI_Asin},
	{"MIDORI_FFI_Acos", &MIDORI_FFI_Acos},
	{"MIDORI_FFI_Atan", &MIDORI_FFI_Atan},
	{"MIDORI_FFI_Atan2", &MIDORI_FFI_Atan2},
	// Math - Hyperbolic
	{"MIDORI_FFI_Sinh", &MIDORI_FFI_Sinh},
	{"MIDORI_FFI_Cosh", &MIDORI_FFI_Cosh},
	{"MIDORI_FFI_Tanh", &MIDORI_FFI_Tanh},
	// Math - Rounding
	{"MIDORI_FFI_Floor", &MIDORI_FFI_Floor},
	{"MIDORI_FFI_Ceil", &MIDORI_FFI_Ceil},
	{"MIDORI_FFI_Round", &MIDORI_FFI_Round},
	{"MIDORI_FFI_Trunc", &MIDORI_FFI_Trunc},
	// Math - Other
	{"MIDORI_FFI_Min", &MIDORI_FFI_Min},
	{"MIDORI_FFI_Max", &MIDORI_FFI_Max},
	{"MIDORI_FFI_Clamp", &MIDORI_FFI_Clamp},
	{"MIDORI_FFI_Sign", &MIDORI_FFI_Sign},
	{"MIDORI_FFI_Fmod", &MIDORI_FFI_Fmod},
	{"MIDORI_FFI_Hypot", &MIDORI_FFI_Hypot},
	// Math - Random
	{"MIDORI_FFI_Random", &MIDORI_FFI_Random},
	{"MIDORI_FFI_RandomInt", &MIDORI_FFI_RandomInt},
	{"MIDORI_FFI_RandomFloat", &MIDORI_FFI_RandomFloat},
	// Math - Conversion
	{"MIDORI_FFI_ToRadians", &MIDORI_FFI_ToRadians},
	{"MIDORI_FFI_ToDegrees", &MIDORI_FFI_ToDegrees},
	// Math - Special
	{"MIDORI_FFI_IsNaN", &MIDORI_FFI_IsNaN},
	{"MIDORI_FFI_IsInf", &MIDORI_FFI_IsInf},
	{"MIDORI_FFI_IsFinite", &MIDORI_FFI_IsFinite},
	// DateTime
	{"MIDORI_FFI_GetTime", &MIDORI_FFI_GetTime},
	{"MIDORI_FFI_GetYear", &MIDORI_FFI_GetYear},
	{"MIDORI_FFI_GetMonth", &MIDORI_FFI_GetMonth},
	{"MIDORI_FFI_GetDay", &MIDORI_FFI_GetDay},
	{"MIDORI_FFI_GetHour", &MIDORI_FFI_GetHour},
	{"MIDORI_FFI_GetMinute", &MIDORI_FFI_GetMinute},
	{"MIDORI_FFI_GetSecond", &MIDORI_FFI_GetSecond},
	{"MIDORI_FFI_GetDayOfWeek", &MIDORI_FFI_GetDayOfWeek},
	{"MIDORI_FFI_GetDayOfYear", &MIDORI_FFI_GetDayOfYear},
	{"MIDORI_FFI_FormatTime", &MIDORI_FFI_FormatTime},
	{"MIDORI_FFI_GetTimezoneOffset", &MIDORI_FFI_GetTimezoneOffset},
	{"MIDORI_FFI_GetUtcYear", &MIDORI_FFI_GetUtcYear},
	{"MIDORI_FFI_GetUtcMonth", &MIDORI_FFI_GetUtcMonth},
	{"MIDORI_FFI_GetUtcDay", &MIDORI_FFI_GetUtcDay},
	{"MIDORI_FFI_GetUtcHour", &MIDORI_FFI_GetUtcHour},
	{"MIDORI_FFI_GetUtcMinute", &MIDORI_FFI_GetUtcMinute},
	{"MIDORI_FFI_GetUtcSecond", &MIDORI_FFI_GetUtcSecond},
	// System
	{"MIDORI_FFI_Exit", &MIDORI_FFI_Exit},
	{"MIDORI_FFI_GetEnv", &MIDORI_FFI_GetEnv},
	{"MIDORI_FFI_SetEnv", &MIDORI_FFI_SetEnv},
	{"MIDORI_FFI_Sleep", &MIDORI_FFI_Sleep},
	{"MIDORI_FFI_GetCurrentDirectory", &MIDORI_FFI_GetCurrentDirectory},
	{"MIDORI_FFI_SetCurrentDirectory", &MIDORI_FFI_SetCurrentDirectory},
	{"MIDORI_FFI_Execute", &MIDORI_FFI_Execute},
	{"MIDORI_FFI_GetPlatform", &MIDORI_FFI_GetPlatform},
	{"MIDORI_FFI_GetProcessId", &MIDORI_FFI_GetProcessId},
}};

const FFIEntry& MidoriFFIRegistry::GetEntry(size_t index)
{
	return s_entries[index];
}

std::optional<size_t> MidoriFFIRegistry::FindIndex(std::string_view name)
{
	for (size_t i = 0u; i < s_entries.size(); ++i)
	{
		if (s_entries[i].m_name == name)
		{
			return i;
		}
	}
	return std::nullopt;
}

const std::array<FFIEntry, MidoriFFIRegistry::BUILTIN_COUNT>& MidoriFFIRegistry::GetTable()
{
	return s_entries;
}
