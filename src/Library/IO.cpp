#include "Library/MidoriStdLibExports.h"

#include <filesystem>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <new>

#ifdef __EMSCRIPTEN__
#include <iostream>
#endif

extern "C"
{
	MIDORI_STDLIB_API void Print(void** args, void* ret) noexcept
	{
		const char* str = reinterpret_cast<const char*>(args[0u]);

		#ifdef __EMSCRIPTEN__
		std::cout << str;
		std::cout.flush();
		#else
		std::printf("%s", str);
		#endif

		std::memset(ret, 0, sizeof(double));
	}

	MIDORI_STDLIB_API void OverwriteToFile(void** args, void* ret) noexcept
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
		std::memset(ret, 0b1, sizeof(double));
	}

	MIDORI_STDLIB_API void AppendToFile(void** args, void* ret) noexcept
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
		std::memset(ret, 0b1, sizeof(double));
	}

	MIDORI_STDLIB_API void FileExists(void** args, void* ret) noexcept
	{
		const char* path = reinterpret_cast<const char*>(args[0u]);
		std::error_code ec;
		const bool exists = std::filesystem::exists(path, ec) && !ec;
		std::memset(ret, exists ? 0b1 : 0, sizeof(double));
	}

	MIDORI_STDLIB_API void DeleteFile(void** args, void* ret) noexcept
	{
		const char* path = reinterpret_cast<const char*>(args[0u]);
		std::error_code ec;
		const bool removed = std::filesystem::remove(path, ec) && !ec;
		std::memset(ret, removed ? 0b1 : 0, sizeof(double));
	}

	MIDORI_STDLIB_API void OpenWriteFile(void** args, void* ret) noexcept
	{
		const char* path = reinterpret_cast<const char*>(args[0u]);
		std::ofstream* handle = new (std::nothrow) std::ofstream(path, std::ios::out | std::ios::binary | std::ios::trunc);
		if (handle == nullptr || !handle->is_open())
		{
			delete handle;
			handle = nullptr;
		}

		const int64_t handle_value = static_cast<int64_t>(reinterpret_cast<uintptr_t>(handle));
		std::memcpy(ret, &handle_value, sizeof(double));
	}

	MIDORI_STDLIB_API void OpenAppendFile(void** args, void* ret) noexcept
	{
		const char* path = reinterpret_cast<const char*>(args[0u]);
		std::ofstream* handle = new (std::nothrow) std::ofstream(path, std::ios::out | std::ios::binary | std::ios::app);
		if (handle == nullptr || !handle->is_open())
		{
			delete handle;
			handle = nullptr;
		}

		const int64_t handle_value = static_cast<int64_t>(reinterpret_cast<uintptr_t>(handle));
		std::memcpy(ret, &handle_value, sizeof(double));
	}

	MIDORI_STDLIB_API void CloseFileHandle(void** args, void* ret) noexcept
	{
		std::ofstream* handle = reinterpret_cast<std::ofstream*>(args[0u]);
		if (handle == nullptr)
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		if (handle->is_open())
		{
			handle->flush();
			handle->close();
		}
		const bool ok = !handle->fail();
		delete handle;
		std::memset(ret, ok ? 0b1 : 0, sizeof(double));
	}

	MIDORI_STDLIB_API void FlushFileHandle(void** args, void* ret) noexcept
	{
		std::ofstream* handle = reinterpret_cast<std::ofstream*>(args[0u]);
		if (handle == nullptr || !handle->is_open())
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		handle->flush();
		const bool ok = handle->good();
		std::memset(ret, ok ? 0b1 : 0, sizeof(double));
	}

	MIDORI_STDLIB_API void WriteToFileHandle(void** args, void* ret) noexcept
	{
		std::ofstream* handle = reinterpret_cast<std::ofstream*>(args[0u]);
		const char* text = reinterpret_cast<const char*>(args[1u]);

		if (handle == nullptr || !handle->is_open() || text == nullptr)
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		handle->write(text, static_cast<std::streamsize>(std::strlen(text)));
		const bool ok = handle->good();
		std::memset(ret, ok ? 0b1 : 0, sizeof(double));
	}

	MIDORI_STDLIB_API void WriteLineToFileHandle(void** args, void* ret) noexcept
	{
		std::ofstream* handle = reinterpret_cast<std::ofstream*>(args[0u]);
		const char* text = reinterpret_cast<const char*>(args[1u]);

		if (handle == nullptr || !handle->is_open() || text == nullptr)
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		handle->write(text, static_cast<std::streamsize>(std::strlen(text)));
		handle->put('\n');
		const bool ok = handle->good();
		std::memset(ret, ok ? 0b1 : 0, sizeof(double));
	}
}
