#include "ImportResolver.h"
#include <algorithm>
#include <cstdlib>

#ifdef _WIN32
#include <stdlib.h>
#endif

using namespace std::string_literals;

ImportResolver::ImportResolver(const std::string& current_file_path)
{
	std::filesystem::path file_path(current_file_path);
	if (file_path.has_parent_path())
	{
		m_current_file_dir = file_path.parent_path();
	}
	else
	{
		m_current_file_dir = std::filesystem::current_path();
	}

	InitializeSystemSearchPaths();
}

std::optional<ImportResolver::ResolvedImport> ImportResolver::Resolve(const std::string& import_specifier) const
{
	if (IsSystemImport(import_specifier))
	{
		return ResolveSystemImport(import_specifier);
	}
	else
	{
		return ResolvePathImport(import_specifier);
	}
}

void ImportResolver::InitializeSystemSearchPaths()
{
	// Read MIDORI_PATH environment variable
#ifdef _WIN32
	char* midori_path = nullptr;
	size_t len = 0u;
	if (_dupenv_s(&midori_path, &len, "MIDORI_PATH") == 0 && midori_path != nullptr)
	{
		std::string path_str(midori_path);
		std::free(midori_path);
#else
	const char* midori_path = std::getenv("MIDORI_PATH");
	if (midori_path != nullptr)
	{
		std::string path_str(midori_path);
#endif
		// Split by platform-specific path separator
#ifdef _WIN32
		const char separator = ';';
#else
		const char separator = ':';
#endif
		size_t start = 0u;
		size_t end = path_str.find(separator);

		while (end != std::string::npos)
		{
			std::string path_segment = path_str.substr(start, end - start);
			if (!path_segment.empty())
			{
				std::filesystem::path search_path(path_segment);
				if (std::filesystem::exists(search_path) && std::filesystem::is_directory(search_path))
				{
					m_system_search_paths.push_back(std::filesystem::weakly_canonical(search_path));
				}
			}
			start = end + 1u;
			end = path_str.find(separator, start);
		}

		// Handle last path segment
		std::string last_segment = path_str.substr(start);
		if (!last_segment.empty())
		{
			std::filesystem::path search_path(last_segment);
			if (std::filesystem::exists(search_path) && std::filesystem::is_directory(search_path))
			{
				m_system_search_paths.push_back(std::filesystem::weakly_canonical(search_path));
			}
		}
	}
}

bool ImportResolver::IsSystemImport(const std::string& import_specifier)
{
	return import_specifier.size() >= 3 &&
		import_specifier.front() == '<' &&
		import_specifier.back() == '>';
}

std::string ImportResolver::ExtractModuleName(const std::string& system_import)
{
	// Remove angle brackets: <IO> -> IO
	if (system_import.size() >= 2 && system_import.front() == '<' && system_import.back() == '>')
	{
		return system_import.substr(1, system_import.size() - 2);
	}
	return system_import;
}

std::optional<ImportResolver::ResolvedImport> ImportResolver::ResolvePathImport(const std::string& import_specifier) const
{
	std::filesystem::path resolved_path(import_specifier);

#ifdef __EMSCRIPTEN__
	// In WASM, absolute paths (starting with /) are in the virtual filesystem
	if (import_specifier[0u] != '/')
	{
		resolved_path = m_current_file_dir / resolved_path;
	}
	std::string absolute_path = resolved_path.string();
#else
	if (!resolved_path.is_absolute())
	{
		resolved_path = m_current_file_dir / resolved_path;
	}

	if (!std::filesystem::exists(resolved_path))
	{
		return std::nullopt;
	}

	std::string absolute_path = std::filesystem::weakly_canonical(resolved_path).string();
#endif

	ResolvedImport result;
	result.m_absolute_path = absolute_path;
	result.m_type = ImportType::PATH;
	result.m_original_specifier = import_specifier;
	return result;
}

std::optional<ImportResolver::ResolvedImport> ImportResolver::ResolveSystemImport(const std::string& import_specifier) const
{
	std::string module_name = ExtractModuleName(import_specifier);
	std::vector<std::filesystem::path> possible_paths = GetModuleFilePaths(module_name);

	// Search through all system search paths
	for (const auto& search_dir : m_system_search_paths)
	{
		for (const auto& module_path : possible_paths)
		{
			std::filesystem::path full_path = search_dir / module_path;
			if (std::filesystem::exists(full_path) && std::filesystem::is_regular_file(full_path))
			{
				ResolvedImport result;
				result.m_absolute_path = std::filesystem::weakly_canonical(full_path).string();
				result.m_type = ImportType::SYSTEM;
				result.m_original_specifier = import_specifier;
				return result;
			}
		}
	}

	return std::nullopt;
}

std::vector<std::filesystem::path> ImportResolver::GetModuleFilePaths(const std::string& module_name)
{
	std::vector<std::filesystem::path> paths;

	// Convert "Math.Vector" to "Math/Vector.mdr"
	std::string path_str = module_name;
	std::ranges::replace(path_str, '.', static_cast<char>(std::filesystem::path::preferred_separator));
	paths.push_back(path_str + ".mdr"s);

	// Also try as-is with .mdr extension
	if (module_name.find('.') == std::string::npos)
	{
		paths.push_back(module_name + ".mdr"s);
	}

	return paths;
}
