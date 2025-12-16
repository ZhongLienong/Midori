#include "Library/MidoriStdLibExports.h"

#include <chrono>
#include <cstring>

extern "C"
{
	MIDORI_STDLIB_API void GetTime(void**, void* ret) noexcept
	{
		const std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
		const std::chrono::time_point now_ms = std::chrono::time_point_cast<std::chrono::milliseconds>(now);
		const std::chrono::milliseconds value = now_ms.time_since_epoch();
		const std::chrono::milliseconds duration = std::chrono::duration_cast<std::chrono::milliseconds>(value);

		const double val = static_cast<double>(duration.count());
		std::memcpy(ret, &val, sizeof(double));
	}
}

