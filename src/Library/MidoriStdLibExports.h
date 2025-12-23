#pragma once

#include <cstdint>

#if defined(_WIN32) || defined(_WIN64)
#define MIDORI_STDLIB_API __declspec(dllexport)
#else
#define MIDORI_STDLIB_API
#endif

#define MIDORI_FFI_FUNC(name) MIDORI_FFI_##name

extern "C"
{
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(Print)(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(PrintError)(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadInput)(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadLine)(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadFile)(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(WriteFile)(void** args, void* ret) noexcept;
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(AppendToFile)(void** args, void* ret) noexcept;

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(SquareRoot)(void** args, void* ret) noexcept;

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(GetTime)(void** args, void* ret) noexcept;
}
