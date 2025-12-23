#include "Library/MidoriStdLibExports.h"

#include <cmath>
#include <cstring>

extern "C"
{
	MIDORI_STDLIB_API void MIDORI_FFI_FUNC(SquareRoot)(void** args, void* ret) noexcept
	{
		double data;
		std::memcpy(&data, args, sizeof(double));

		const double result = std::sqrt(data);
		std::memcpy(ret, &result, sizeof(double));
	}
}

