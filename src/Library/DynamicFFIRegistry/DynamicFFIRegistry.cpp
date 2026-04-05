#include "DynamicFFIRegistry.h"
#include "Common/Checksum/Checksum.h"

#ifdef _WIN32
    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #undef LoadLibrary
    #undef UnloadLibrary
#else
    #include <dlfcn.h>
#endif

#include <format>

DynamicFFIRegistry& DynamicFFIRegistry::GetInstance()
{
	static DynamicFFIRegistry instance;
	return instance;
}

std::expected<void, std::string> DynamicFFIRegistry::LoadLibrary(const std::filesystem::path& libraryPath, const std::string& packageName, std::optional<std::string_view> expectedChecksum)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_libraries.contains(packageName))
	{
		return std::unexpected(std::format("FFI error: package '{}' already has a loaded native library.", packageName));
	}

	if (!std::filesystem::exists(libraryPath))
	{
		return std::unexpected(std::format("FFI error: library file not found: {}", libraryPath.string()));
	}

	const std::expected<void, std::string> checksum_result = VerifyLibraryChecksum(libraryPath, expectedChecksum);
	if (!checksum_result.has_value())
	{
		return std::unexpected(checksum_result.error());
	}

	const std::expected<void*, std::string> load_result = LoadPlatformLibrary(libraryPath);
	if (!load_result.has_value())
	{
		return std::unexpected(load_result.error());
	}

	LibraryHandle* libHandle = new LibraryHandle{ load_result.value(), packageName, libraryPath };
	m_libraries[packageName] = std::unique_ptr<LibraryHandle>(libHandle);
	return {};
}

std::expected<void, std::string> DynamicFFIRegistry::LoadLibraryWithFunctions(const std::filesystem::path& libraryPath, const std::string& packageName, const std::unordered_map<std::string, std::string>& functionMappings, std::optional<std::string_view> expectedChecksum)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_libraries.contains(packageName))
	{
		return std::unexpected(std::format("FFI error: package '{}' already has a loaded native library.", packageName));
	}

	if (!std::filesystem::exists(libraryPath))
	{
		return std::unexpected(std::format("FFI error: library file not found: {}", libraryPath.string()));
	}

	const std::expected<void, std::string> checksum_result = VerifyLibraryChecksum(libraryPath, expectedChecksum);
	if (!checksum_result.has_value())
	{
		return std::unexpected(checksum_result.error());
	}

	const std::expected<void*, std::string> load_result = LoadPlatformLibrary(libraryPath);
	if (!load_result.has_value())
	{
		return std::unexpected(load_result.error());
	}

	void* handle = load_result.value();
	LibraryHandle* libHandle = new LibraryHandle{ handle, packageName, libraryPath };
	m_libraries[packageName] = std::unique_ptr<LibraryHandle>(libHandle);

	std::unordered_map<std::string, FFIFunction> resolved_functions;
	for (const std::pair<const std::string, std::string>& func_pair : functionMappings)
	{
		const std::string& midori_name = func_pair.first;
		const std::string& native_name = func_pair.second;

		void* func_ptr = GetPlatformFunction(handle, native_name);
		if (func_ptr == nullptr)
		{
			UnloadPlatformLibrary(handle);
			m_libraries.erase(packageName);
			return std::unexpected(std::format(
				"FFI error: package '{}' declares function '{}' but symbol '{}' was not found in {}",
				packageName,
				midori_name,
				native_name,
				libraryPath.string()));
		}

		resolved_functions.emplace(midori_name, reinterpret_cast<FFIFunction>(func_ptr));
	}

	for (auto& [midori_name, function] : resolved_functions)
	{
		m_functions[midori_name] = function;
	}

	return {};
}

bool DynamicFFIRegistry::UnloadLibrary(const std::string& packageName)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	const std::unordered_map<std::string, std::unique_ptr<LibraryHandle>>::iterator it = m_libraries.find(packageName);
	if (it == m_libraries.end())
	{
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
	return true;
}

bool DynamicFFIRegistry::RegisterFunction(const std::string& functionName, FFIFunction function)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_functions.contains(functionName))
	{
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

std::expected<void*, std::string> DynamicFFIRegistry::LoadPlatformLibrary(const std::filesystem::path& path)
{
#ifdef _WIN32
	void* handle = LoadLibraryW(path.c_str());
	if (!handle)
	{
		DWORD error = GetLastError();
		return std::unexpected(std::format("FFI error: failed to load library '{}' (Windows error {}).", path.string(), error));
	}
	return handle;
#else
	void* handle = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);
	if (!handle)
	{
		return std::unexpected(std::format("FFI error: failed to load library '{}' ({}).", path.string(), dlerror()));
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

std::expected<void, std::string> DynamicFFIRegistry::VerifyLibraryChecksum(const std::filesystem::path& libraryPath, std::optional<std::string_view> expectedChecksum)
{
	if (!expectedChecksum.has_value() || expectedChecksum->empty())
	{
		return {};
	}

	const std::expected<bool, std::string> verification = MidoriChecksum::VerifyFileChecksum(libraryPath, *expectedChecksum);
	if (!verification.has_value())
	{
		return std::unexpected(std::format(
			"FFI error: failed to verify checksum for {}: {}",
			libraryPath.string(),
			verification.error()));
	}

	if (!verification.value())
	{
		return std::unexpected(std::format(
			"FFI error: checksum mismatch for {}. Refusing to load the library.",
			libraryPath.string()));
	}

	return {};
}
