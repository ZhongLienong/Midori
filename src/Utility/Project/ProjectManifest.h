#pragma once

#include <filesystem>
#include <string>
#include <string_view>

namespace MidoriProject
{
	void ApplyProjectManifestToEnvironment(const std::filesystem::path& input_path);

	bool InitializeProject(const std::filesystem::path& target_dir, std::string_view project_name, std::string& error_message);
}

namespace MidoriPackage
{
	bool InitializePackage(const std::filesystem::path& target_dir, std::string_view package_name, std::string& error_message);
}
