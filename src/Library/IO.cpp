#include "Library/MidoriStdLibExports.h"

#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <new>

#ifdef __EMSCRIPTEN__
#include <iostream>
#endif

namespace
{
	char* AllocateString(const std::string& str)
	{
		const size_t size = str.size() + 1u;
		char* buffer = new (std::nothrow) char[size];
		if (buffer != nullptr)
		{
			std::memcpy(buffer, str.c_str(), size);
		}
		return buffer;
	}

	char* AllocateEmptyString()
	{
		char* buffer = new (std::nothrow) char[1u];
		if (buffer != nullptr)
		{
			buffer[0u] = '\0';
		}
		return buffer;
	}
}

extern "C"
{
	// Console Output
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(Print)(void** args, void* ret) noexcept
	{
		const char* str = reinterpret_cast<const char*>(args[0u]);

		#ifdef __EMSCRIPTEN__
		std::cout << str;
		std::cout.flush();
		#else
		std::printf("%s", str);
		std::fflush(stdout);
		#endif

		std::memset(ret, 0, sizeof(double));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(PrintError)(void** args, void* ret) noexcept
	{
		const char* str = reinterpret_cast<const char*>(args[0u]);

		#ifdef __EMSCRIPTEN__
		std::cerr << str;
		std::cerr.flush();
		#else
		std::fprintf(stderr, "%s", str);
		std::fflush(stderr);
		#endif

		std::memset(ret, 0, sizeof(double));
	}

	// Console Input
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadInput)(void** args, void* ret) noexcept
	{
		std::ostringstream buffer;
		buffer << std::cin.rdbuf();
		char* result = AllocateString(buffer.str());

		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(double));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadLine)(void** args, void* ret) noexcept
	{
		std::string line;
		if (std::getline(std::cin, line))
		{
			char* result = AllocateString(line);
			const int64_t ptr = reinterpret_cast<int64_t>(result);
			std::memcpy(ret, &ptr, sizeof(double));
		}
		else
		{
			char* empty = AllocateEmptyString();
			const int64_t ptr = reinterpret_cast<int64_t>(empty);
			std::memcpy(ret, &ptr, sizeof(double));
		}
	}

	// Simple File Operations
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadFile)(void** args, void* ret) noexcept
	{
		const char* file_path = reinterpret_cast<const char*>(args[0u]);

		std::ifstream file(file_path, std::ios::in | std::ios::binary);
		if (!file.is_open())
		{
			char* empty = AllocateEmptyString();
			const int64_t ptr = reinterpret_cast<int64_t>(empty);
			std::memcpy(ret, &ptr, sizeof(double));
			return;
		}

		std::ostringstream buffer;
		buffer << file.rdbuf();
		char* result = AllocateString(buffer.str());

		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(double));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(WriteFile)(void** args, void* ret) noexcept
	{
		const char* file_name = reinterpret_cast<const char*>(args[0u]);
		const char* text = reinterpret_cast<const char*>(args[1u]);

		std::ofstream file(file_name, std::ios::out | std::ios::binary);
		if (!file.is_open())
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		file.write(text, static_cast<std::streamsize>(std::strlen(text)));
		file.close();
		const bool success = !file.fail();
		std::memset(ret, 0, sizeof(double));
		*reinterpret_cast<bool*>(ret) = success;
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(AppendToFile)(void** args, void* ret) noexcept
	{
		const char* file_name = reinterpret_cast<const char*>(args[0u]);
		const char* text = reinterpret_cast<const char*>(args[1u]);

		std::ofstream file(file_name, std::ios::out | std::ios::app | std::ios::binary);
		if (!file.is_open())
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		file.write(text, static_cast<std::streamsize>(std::strlen(text)));
		file.close();
		const bool success = !file.fail();
		std::memset(ret, 0, sizeof(double));
		*reinterpret_cast<bool*>(ret) = success;
	}
}
