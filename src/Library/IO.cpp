#include "Library/MidoriStdLibExports.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#ifdef __EMSCRIPTEN__
#include <iostream>
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
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadInput)(void**, void* ret) noexcept
	{
		std::ostringstream buffer;
		buffer << std::cin.rdbuf();
		char* result = AllocateString(buffer.str());

		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(double));
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadLine)(void**, void* ret) noexcept
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

	// Binary File Operations
	// Returns pointer to struct { void* data; int length; }
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(ReadBinaryFile)(void** args, void* ret) noexcept
	{
		struct FFIArray
		{
			void* data;
			int length;
		};

		const char* file_path = reinterpret_cast<const char*>(args[0u]);

		std::ifstream file(file_path, std::ios::in | std::ios::binary);
		if (!file.is_open())
		{
			const int64_t null_ptr = 0;
			std::memcpy(ret, &null_ptr, sizeof(int64_t));
			return;
		}

		file.seekg(0, std::ios::end);
		const std::streamsize file_size = file.tellg();
		file.seekg(0, std::ios::beg);

		if (file_size <= 0)
		{
			const int64_t null_ptr = 0;
			std::memcpy(ret, &null_ptr, sizeof(int64_t));
			return;
		}

		double* array_data = static_cast<double*>(std::malloc(static_cast<size_t>(file_size) * sizeof(double)));
		if (array_data == nullptr)
		{
			const int64_t null_ptr = 0;
			std::memcpy(ret, &null_ptr, sizeof(int64_t));
			return;
		}

		std::vector<char> buffer(static_cast<size_t>(file_size));
		file.read(buffer.data(), file_size);

		for (std::streamsize i = 0; i < file_size; i += 1)
		{
			const uint8_t byte_value = static_cast<uint8_t>(buffer[static_cast<size_t>(i)]);
			const int64_t value = static_cast<int64_t>(byte_value);
			std::memcpy(&array_data[i], &value, sizeof(double));
		}

		FFIArray* result = static_cast<FFIArray*>(std::malloc(sizeof(FFIArray)));
		if (result == nullptr)
		{
			std::free(array_data);
			const int64_t null_ptr = 0;
			std::memcpy(ret, &null_ptr, sizeof(int64_t));
			return;
		}

		result->data = array_data;
		result->length = static_cast<int>(file_size);

		const int64_t ptr = reinterpret_cast<int64_t>(result);
		std::memcpy(ret, &ptr, sizeof(int64_t));
	}

	// Takes Array<Byte> and writes to file
	// Array is passed as struct { void* data; int length; }
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(WriteBinaryFile)(void** args, void* ret) noexcept
	{
		const char* file_name = reinterpret_cast<const char*>(args[0u]);

		struct ArrayArgument
		{
			void* data;
			int length;
		};

		ArrayArgument* array_arg = reinterpret_cast<ArrayArgument*>(args[1u]);
		if (array_arg == nullptr || array_arg->data == nullptr || array_arg->length <= 0)
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		std::ofstream file(file_name, std::ios::out | std::ios::binary);
		if (!file.is_open())
		{
			std::memset(ret, 0, sizeof(double));
			return;
		}

		double* array_data = reinterpret_cast<double*>(array_arg->data);
		std::vector<char> buffer(static_cast<size_t>(array_arg->length));

		for (int i = 0; i < array_arg->length; i += 1)
		{
			int64_t byte_value = 0;
			std::memcpy(&byte_value, &array_data[i], sizeof(double));
			buffer[static_cast<size_t>(i)] = static_cast<char>(byte_value & 0xFF);
		}

		file.write(buffer.data(), static_cast<std::streamsize>(array_arg->length));
		file.close();

		const bool success = !file.fail();
		std::memset(ret, 0, sizeof(double));
		*reinterpret_cast<bool*>(ret) = success;
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(FileExists)(void** args, void* ret) noexcept
	{
		const char* file_path = reinterpret_cast<const char*>(args[0u]);
		const bool exists = std::filesystem::exists(file_path);
		std::memset(ret, 0, sizeof(double));
		*reinterpret_cast<bool*>(ret) = exists;
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(DeleteFile)(void** args, void* ret) noexcept
	{
		const char* file_path = reinterpret_cast<const char*>(args[0u]);
		std::error_code ec;
		const bool success = std::filesystem::remove(file_path, ec);
		std::memset(ret, 0, sizeof(double));
		*reinterpret_cast<bool*>(ret) = success && !ec;
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(RenameFile)(void** args, void* ret) noexcept
	{
		const char* old_path = reinterpret_cast<const char*>(args[0u]);
		const char* new_path = reinterpret_cast<const char*>(args[1u]);
		std::error_code ec;
		std::filesystem::rename(old_path, new_path, ec);
		std::memset(ret, 0, sizeof(double));
		*reinterpret_cast<bool*>(ret) = !ec;
	}

	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(GetFileSize)(void** args, void* ret) noexcept
	{
		const char* file_path = reinterpret_cast<const char*>(args[0u]);
		std::error_code ec;
		const std::uintmax_t size = std::filesystem::file_size(file_path, ec);
		const int64_t result = ec ? -1 : static_cast<int64_t>(size);
		std::memcpy(ret, &result, sizeof(int64_t));
	}
}
