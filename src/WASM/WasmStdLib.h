#pragma once

#ifdef __EMSCRIPTEN__

namespace WasmStdLib
{
	using ForeignFunction = void(*)(void**, void*);

	ForeignFunction GetFunction(const char* name);
}

#endif // __EMSCRIPTEN__
