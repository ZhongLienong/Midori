#pragma once

#include "Library/MidoriBuiltinFFIRegistry/MidoriFFIRegistry.h"
#include <filesystem>
#include <memory>
#include <mutex>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>

class DynamicFFIRegistry
{
public:
	static DynamicFFIRegistry& GetInstance();

	bool LoadLibrary(const std::filesystem::path& libraryPath, const std::string& packageName);
	bool LoadLibraryWithFunctions(const std::filesystem::path& libraryPath, const std::string& packageName, const std::unordered_map<std::string, std::string>& functionMappings);
	bool UnloadLibrary(const std::string& packageName);

	bool RegisterFunction(const std::string& functionName, FFIFunction function);
	std::optional<FFIFunction> FindFunction(std::string_view functionName) const;

	bool IsLibraryLoaded(const std::string& packageName) const;
	void Clear();

	~DynamicFFIRegistry();

private:
	DynamicFFIRegistry() = default;
	DynamicFFIRegistry(const DynamicFFIRegistry&) = delete;
	DynamicFFIRegistry& operator=(const DynamicFFIRegistry&) = delete;

	struct LibraryHandle
	{
		void* m_handle;
		std::string m_packageName;
		std::filesystem::path m_path;
	};

	mutable std::mutex m_mutex;
	std::unordered_map<std::string, FFIFunction> m_functions;
	std::unordered_map<std::string, std::unique_ptr<LibraryHandle>> m_libraries;

	void* LoadPlatformLibrary(const std::filesystem::path& path);
	void UnloadPlatformLibrary(void* handle);
	void* GetPlatformFunction(void* libraryHandle, const std::string& functionName);
};
