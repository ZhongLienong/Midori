#pragma once

#include <cstdint>

#if defined(_WIN32) || defined(_WIN64)
#define MIDORI_STDLIB_API __declspec(dllexport)
#else
#define MIDORI_STDLIB_API
#endif

extern "C"
{
	MIDORI_STDLIB_API void Print(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void OverwriteToFile(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void AppendToFile(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void FileExists(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void DeleteFile(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void OpenWriteFile(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void OpenAppendFile(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void CloseFileHandle(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void FlushFileHandle(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void WriteToFileHandle(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void WriteLineToFileHandle(void** args, void* ret) noexcept;

	MIDORI_STDLIB_API void SquareRoot(void** args, void* ret) noexcept;

	MIDORI_STDLIB_API void GetTime(void** args, void* ret) noexcept;
}
