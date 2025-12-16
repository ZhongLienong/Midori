#pragma once

#ifdef __EMSCRIPTEN__

namespace MidoriStdLib
{
	using ForeignFunction = void(*)(void**, void*);

	ForeignFunction GetFunction(const char* name) noexcept;
}

#endif // __EMSCRIPTEN__

