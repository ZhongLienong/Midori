#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace MidoriProject
{
	struct TestConfiguration
	{
		std::filesystem::path m_directory = "test";
		int m_timeout_ms = 30000;
	};

	struct ManifestConfiguration
	{
		std::filesystem::path m_root;
		std::string m_name;
		std::filesystem::path m_entry;
		std::filesystem::path m_source_dir;
		std::filesystem::path m_packages_dir;
		std::filesystem::path m_prelude_dir;
		std::vector<std::filesystem::path> m_extra_paths;
		TestConfiguration m_test;
	};

	[[nodiscard]] std::optional<ManifestConfiguration> FindManifestConfiguration(const std::filesystem::path& input_path);

	void ApplyProjectManifestToEnvironment(const std::filesystem::path& input_path);

	bool InitializeProject(const std::filesystem::path& target_dir, std::string_view project_name, std::string& error_message);
}

namespace MidoriPackage
{
	bool InitializePackage(const std::filesystem::path& target_dir, std::string_view package_name, std::string& error_message);
}
