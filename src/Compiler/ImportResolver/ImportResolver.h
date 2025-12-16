#pragma once

#include <string>
#include <vector>
#include <filesystem>
#include <optional>

class ImportResolver
{
public:
	enum class ImportType
	{
		PATH,    // Relative or absolute file path
		SYSTEM   // System module (uses MIDORI_PATH)
	};

	struct ResolvedImport
	{
		std::string m_absolute_path;
		ImportType m_type;
		std::string m_original_specifier;  // Original import string for error messages
	};

private:
	std::vector<std::filesystem::path> m_system_search_paths;
	std::filesystem::path m_current_file_dir;

public:
	explicit ImportResolver(const std::string& current_file_path);

	std::optional<ResolvedImport> Resolve(const std::string& import_specifier) const;

	const std::vector<std::filesystem::path>& GetSystemSearchPaths() const { return m_system_search_paths; }

private:
	void InitializeSystemSearchPaths();

	static bool IsSystemImport(const std::string& import_specifier);

	static std::string ExtractModuleName(const std::string& system_import);

	std::optional<ResolvedImport> ResolvePathImport(const std::string& import_specifier) const;

	std::optional<ResolvedImport> ResolveSystemImport(const std::string& import_specifier) const;

	static std::vector<std::filesystem::path> GetModuleFilePaths(const std::string& module_name);
};
