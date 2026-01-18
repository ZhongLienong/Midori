#include "DynamicFFIRegistry.h"
#include "Common/Printer/Printer.h"

#ifdef _WIN32
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #undef LoadLibrary
    #undef UnloadLibrary
#else
    #include <dlfcn.h>
#endif

DynamicFFIRegistry& DynamicFFIRegistry::GetInstance()
{
	static DynamicFFIRegistry instance;
	return instance;
}

bool DynamicFFIRegistry::LoadLibrary(const std::filesystem::path& libraryPath, const std::string& packageName)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_libraries.contains(packageName))
	{
		Printer::PrintFormatted<Printer::Color::YELLOW>("[DynamicFFI] Library for package '{}' is already loaded\n", packageName);
		return false;
	}

	if (!std::filesystem::exists(libraryPath))
	{
		Printer::PrintFormatted<Printer::Color::RED>("[DynamicFFI] Library file not found: {}\n", libraryPath.string());
		return false;
	}

	void* handle = LoadPlatformLibrary(libraryPath);
	if (!handle)
	{
		return false;
	}

	LibraryHandle* libHandle = new LibraryHandle{handle, packageName, libraryPath};
	m_libraries[packageName] = std::unique_ptr<LibraryHandle>(libHandle);

	Printer::PrintFormatted<Printer::Color::GREEN>("[DynamicFFI] Successfully loaded library: {} for package: {}\n", libraryPath.string(), packageName);
	return true;
}

bool DynamicFFIRegistry::LoadLibraryWithFunctions(const std::filesystem::path& libraryPath, const std::string& packageName, const std::unordered_map<std::string, std::string>& functionMappings)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_libraries.contains(packageName))
	{
		Printer::PrintFormatted<Printer::Color::YELLOW>("[DynamicFFI] Library for package '{}' is already loaded\n", packageName);
		return false;
	}

	if (!std::filesystem::exists(libraryPath))
	{
		Printer::PrintFormatted<Printer::Color::RED>("[DynamicFFI] Library file not found: {}\n", libraryPath.string());
		return false;
	}

	void* handle = LoadPlatformLibrary(libraryPath);
	if (!handle)
	{
		return false;
	}

	LibraryHandle* libHandle = new LibraryHandle{handle, packageName, libraryPath};
	m_libraries[packageName] = std::unique_ptr<LibraryHandle>(libHandle);

	int registered_count = 0;
	for (const std::pair<const std::string, std::string>& func_pair : functionMappings)
	{
		const std::string& midori_name = func_pair.first;
		const std::string& native_name = func_pair.second;

		void* func_ptr = GetPlatformFunction(handle, native_name);
		if (func_ptr)
		{
			m_functions[midori_name] = reinterpret_cast<FFIFunction>(func_ptr);
			registered_count++;
		}
		else
		{
			Printer::PrintFormatted<Printer::Color::YELLOW>("[DynamicFFI] Warning: Could not find function '{}' in library\n", native_name);
		}
	}

	Printer::PrintFormatted<Printer::Color::GREEN>("[DynamicFFI] Successfully loaded library: {} for package: {} ({}/{} functions registered)\n", libraryPath.string(), packageName, registered_count, functionMappings.size());

	return true;
}

bool DynamicFFIRegistry::UnloadLibrary(const std::string& packageName)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	const std::unordered_map<std::string, std::unique_ptr<LibraryHandle>>::iterator it = m_libraries.find(packageName);
	if (it == m_libraries.end())
	{
		Printer::PrintFormatted<Printer::Color::RED>("[DynamicFFI] Package '{}' not found\n", packageName);
		return false;
	}

	for (std::unordered_map<std::string, FFIFunction>::iterator funcIt = m_functions.begin(); funcIt != m_functions.end();)
	{
		if (funcIt->first.starts_with(packageName + "::"))
		{
			funcIt = m_functions.erase(funcIt);
		}
		else
		{
			++funcIt;
		}
	}

	UnloadPlatformLibrary(it->second->m_handle);
	m_libraries.erase(it);

	Printer::PrintFormatted<Printer::Color::GREEN>("[DynamicFFI] Unloaded library for package: {}\n", packageName);
	return true;
}

bool DynamicFFIRegistry::RegisterFunction(const std::string& functionName, FFIFunction function)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_functions.contains(functionName))
	{
		Printer::PrintFormatted<Printer::Color::YELLOW>("[DynamicFFI] Function '{}' is already registered\n", functionName);
		return false;
	}

	m_functions[functionName] = function;
	return true;
}

std::optional<FFIFunction> DynamicFFIRegistry::FindFunction(std::string_view functionName) const
{
	std::lock_guard<std::mutex> lock(m_mutex);

	const std::unordered_map<std::string, FFIFunction>::const_iterator it = m_functions.find(std::string(functionName));
	if (it != m_functions.end())
	{
		return it->second;
	}

	return std::nullopt;
}

bool DynamicFFIRegistry::IsLibraryLoaded(const std::string& packageName) const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	return m_libraries.contains(packageName);
}

void DynamicFFIRegistry::Clear()
{
	std::lock_guard<std::mutex> lock(m_mutex);

	for (const std::pair<const std::string, std::unique_ptr<LibraryHandle>>& lib : m_libraries)
	{
		UnloadPlatformLibrary(lib.second->m_handle);
	}

	m_libraries.clear();
	m_functions.clear();
}

DynamicFFIRegistry::~DynamicFFIRegistry()
{
	std::lock_guard<std::mutex> lock(m_mutex);

	for (const std::pair<const std::string, std::unique_ptr<LibraryHandle>>& lib : m_libraries)
	{
		UnloadPlatformLibrary(lib.second->m_handle);
	}
}

void* DynamicFFIRegistry::LoadPlatformLibrary(const std::filesystem::path& path)
{
#ifdef _WIN32
	void* handle = LoadLibraryW(path.c_str());
	if (!handle)
	{
		DWORD error = GetLastError();
		Printer::PrintFormatted<Printer::Color::RED>("[DynamicFFI] Failed to load library: {} (Error: {})\n", path.string(), error);
	}
	return handle;
#else
	void* handle = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);
	if (!handle)
	{
		Printer::PrintFormatted<Printer::Color::RED>("[DynamicFFI] Failed to load library: {} ({})\n", path.string(), dlerror());
	}
	return handle;
#endif
}

void DynamicFFIRegistry::UnloadPlatformLibrary(void* handle)
{
	if (!handle)
	{
		return;
	}

#ifdef _WIN32
	FreeLibrary(static_cast<HMODULE>(handle));
#else
	dlclose(handle);
#endif
}

void* DynamicFFIRegistry::GetPlatformFunction(void* libraryHandle, const std::string& functionName)
{
	if (!libraryHandle)
	{
		return nullptr;
	}

#ifdef _WIN32
	return reinterpret_cast<void*>(GetProcAddress(static_cast<HMODULE>(libraryHandle), functionName.c_str()));
#else
	return dlsym(libraryHandle, functionName.c_str());
#endif
}
