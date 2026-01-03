#include "Library/MidoriStdLibExports.h"

#include <chrono>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <string>
#include <thread>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <unistd.h>
#endif

namespace
{
	char* AllocateString(const std::string& str)
	{
		const size_t size = str.size() + 1u;
		char* buffer = static_cast<char*>(std::malloc(size));
		if (buffer != nullptr)
		{
			std::memcpy(buffer, str.c_str(), size);
		}
		return buffer;
	}

	char* AllocateEmptyString()
	{
		char* buffer = static_cast<char*>(std::malloc(1u));
		if (buffer != nullptr)
		{
			buffer[0u] = '\0';
		}
		return buffer;
	}
}

extern "C"
{
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(Exit)(void** args, void*) noexcept
	{
		int64_t exit_code = 0;
		std::memcpy(&exit_code, &args[0u], sizeof(int64_t));
		std::exit(static_cast<int>(exit_code));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(GetEnv)(void** args, void* ret) noexcept
	{
		const char* var_name = reinterpret_cast<const char*>(args[0u]);

		#ifdef _MSC_VER
		char* value = nullptr;
		size_t len = 0u;
		const errno_t err = _dupenv_s(&value, &len, var_name);
		if (err != 0 || value == nullptr)
		{
			char* empty = AllocateEmptyString();
			const int64_t ptr = reinterpret_cast<int64_t>(empty);
			std::memcpy(ret, &ptr, sizeof(int64_t));
			return;
		}
		char* result = AllocateString(value);
		std::free(value);
		#else
		const char* value = std::getenv(var_name);
		char* result = (value != nullptr) ? AllocateString(value) : AllocateEmptyString();
		#endif

		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(int64_t));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(Sleep)(void** args, void* ret) noexcept
	{
		int64_t milliseconds = 0;
		std::memcpy(&milliseconds, &args[0u], sizeof(int64_t));
		std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
		std::memset(ret, 0, sizeof(double));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(GetCurrentDirectory)(void**, void* ret) noexcept
	{
		std::error_code ec;
		const std::filesystem::path cwd = std::filesystem::current_path(ec);
		char* result = ec ? AllocateEmptyString() : AllocateString(cwd.string());
		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(int64_t));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(SetCurrentDirectory)(void** args, void* ret) noexcept
	{
		const char* path = reinterpret_cast<const char*>(args[0u]);
		std::error_code ec;
		std::filesystem::current_path(path, ec);
		const int64_t success = ec ? 0 : 1;
		std::memcpy(ret, &success, sizeof(int64_t));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(SetEnv)(void** args, void* ret) noexcept
	{
		const char* name = reinterpret_cast<const char*>(args[0u]);
		const char* value = reinterpret_cast<const char*>(args[1u]);
		int result = 0;

#ifdef _MSC_VER
		result = _putenv_s(name, value) == 0 ? 1 : 0;
#else
		result = setenv(name, value, 1) == 0 ? 1 : 0;
#endif

		const int64_t success = result;
		std::memcpy(ret, &success, sizeof(int64_t));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(Execute)(void** args, void* ret) noexcept
	{
		const char* command = reinterpret_cast<const char*>(args[0u]);
		const int exit_code = std::system(command);
		const int64_t result = static_cast<int64_t>(exit_code);
		std::memcpy(ret, &result, sizeof(int64_t));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(GetPlatform)(void**, void* ret) noexcept
	{
#if defined(_WIN32)
		char* result = AllocateString("Windows");
#elif defined(__APPLE__)
		char* result = AllocateString("macOS");
#elif defined(__linux__)
		char* result = AllocateString("Linux");
#elif defined(__EMSCRIPTEN__)
		char* result = AllocateString("WebAssembly");
#else
		char* result = AllocateString("Unknown");
#endif
		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(int64_t));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(GetProcessId)(void**, void* ret) noexcept
	{
#ifdef _WIN32
		const int64_t pid = static_cast<int64_t>(GetCurrentProcessId());
#else
		const int64_t pid = static_cast<int64_t>(getpid());
#endif
		std::memcpy(ret, &pid, sizeof(int64_t));
	}
}
