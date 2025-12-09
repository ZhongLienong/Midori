#ifdef __EMSCRIPTEN__

#include <iostream>
#include <cmath>
#include <cstring>
#include <chrono>
#include <unordered_map>
#include <string>

// WASM-compatible standard library functions
// These replace the DLL functions for browser environment

namespace WasmStdLib
{
	// IO Functions
	void Print(void** args, void* ret) noexcept
	{
		const char* str = reinterpret_cast<const char*>(args[0u]);
		// Use std::cout instead of printf so output can be captured by rdbuf redirection
		std::cout << str;
		std::cout.flush();
		std::memset(ret, 0, sizeof(uintptr_t));
	}

	void OverwriteToFile(void** args, void* ret) noexcept
	{
		// File operations not supported in browser
		std::memset(ret, 0, sizeof(uintptr_t));
	}

	void AppendToFile(void** args, void* ret) noexcept
	{
		// File operations not supported in browser
		std::memset(ret, 0, sizeof(uintptr_t));
	}

	// Math Functions
	void SquareRoot(void** args, void* ret) noexcept
	{
		double data;
		std::memcpy(&data, args, sizeof(double));
		double result = std::sqrt(data);
		std::memcpy(ret, &result, sizeof(double));
	}

	// DateTime Functions
	void GetTime(void**, void* ret) noexcept
	{
		std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
		std::chrono::time_point now_ms = std::chrono::time_point_cast<std::chrono::milliseconds>(now);
		std::chrono::milliseconds value = now_ms.time_since_epoch();
		std::chrono::milliseconds duration = std::chrono::duration_cast<std::chrono::milliseconds>(value);
		double val = static_cast<double>(duration.count());
		std::memcpy(ret, &val, sizeof(double));
	}

	// Function registry for WASM
	using ForeignFunction = void(*)(void**, void*);

	std::unordered_map<std::string, ForeignFunction>& GetFunctionRegistry()
	{
		static std::unordered_map<std::string, ForeignFunction> registry = {
			{"Print", Print},
			{"OverwriteToFile", OverwriteToFile},
			{"AppendToFile", AppendToFile},
			{"SquareRoot", SquareRoot},
			{"GetTime", GetTime}
		};
		return registry;
	}

	ForeignFunction GetFunction(const char* name)
	{
		std::unordered_map<std::string, ForeignFunction>& registry = GetFunctionRegistry();
		std::unordered_map<std::string, ForeignFunction>::iterator it = registry.find(name);
		if (it != registry.end())
		{
			return it->second;
		}
		return nullptr;
	}
}

#endif // __EMSCRIPTEN__
