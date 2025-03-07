#include <chrono>
#include <cmath>
#include <fstream>

#if defined(_WIN32) || defined(_WIN64)
#define MIDORI_API __declspec(dllexport)
#else
#define MIDORI_API
#endif

extern "C"
{
	/**
	 * 
	 *	IO
	 * 
	 */
	MIDORI_API void Print(void** args, void* ret) noexcept
	{
		const char* str = reinterpret_cast<const char*>(args[0u]);
		std::printf("%s", str);
		std::memset(ret, 0, sizeof(uintptr_t));
	}

	MIDORI_API void OverwriteToFile(void** args, void* ret) noexcept
	{
		const char* file_name = reinterpret_cast<const char*>(args[0u]);
		const char* text = reinterpret_cast<const char*>(args[1u]);

		std::ofstream file(file_name, std::ios::out | std::ios::binary);
		if (!file.is_open())
		{
			std::memset(ret, 0, sizeof(uintptr_t));
		}
		else
		{
			file.write(text, static_cast<std::streamsize>(std::strlen(text)));
			std::memset(ret, 0b1, sizeof(uintptr_t));
		}
	}

	MIDORI_API void AppendToFile(void** args, void* ret) noexcept
	{
		const char* file_name = reinterpret_cast<const char*>(args[0u]);
		const char* text = reinterpret_cast<const char*>(args[1u]);
		std::ofstream file(file_name, std::ios::out | std::ios::app | std::ios::binary);
		if (!file.is_open())
		{
			std::memset(ret, 0, sizeof(uintptr_t));
		}
		else
		{
			file.write(text, static_cast<std::streamsize>(std::strlen(text)));
			std::memset(ret, 0b1, sizeof(uintptr_t));
		}
	}


	/**
	 * 
	 * Math
	 * 
	 */
	MIDORI_API void SquareRoot(void** args, void* ret) noexcept
	{
		double data = std::bit_cast<double>(args[0u]);
		double result = std::sqrt(data);

		std::memcpy(ret, &result, sizeof(double));
	}

	/**
	 * 
	 * DateTime
	 * 
	 */
	MIDORI_API void GetTime(void**, void* ret) noexcept
	{
		std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
		std::chrono::time_point now_ms = std::chrono::time_point_cast<std::chrono::milliseconds>(now);
		std::chrono::milliseconds value = now_ms.time_since_epoch();
		std::chrono::milliseconds duration = std::chrono::duration_cast<std::chrono::milliseconds>(value);
		double val = static_cast<double>(duration.count());
		std::memcpy(ret, &val, sizeof(double));
	}
}
