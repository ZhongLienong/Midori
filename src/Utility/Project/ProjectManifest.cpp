#include "Utility/Project/ProjectManifest.h"

#include "Common/Printer/Printer.h"

#include <toml.hpp>

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <format>
#include <fstream>
#include <optional>
#include <system_error>
#include <vector>

namespace
{
	constexpr const char* ProjectManifestFileName = "project.midori";
	constexpr const char* PackageManifestFileName = "package.midori";

	struct ProjectSettings
	{
		std::filesystem::path m_root;
		std::string m_name;
		std::filesystem::path m_entry;
		std::filesystem::path m_source_dir;
		std::filesystem::path m_packages_dir;
		std::filesystem::path m_prelude_dir;
		std::vector<std::filesystem::path> m_extra_paths;
	};

	const toml::value* FindTable(const toml::value& data, const char* key)
	{
		if (!data.is_table())
		{
			return nullptr;
		}

		if (!data.contains(key))
		{
			return nullptr;
		}

		return &data.at(key);
	}

	std::vector<std::filesystem::path> ReadPathArray(const toml::value& value)
	{
		std::vector<std::filesystem::path> paths;
		if (!value.is_array())
		{
			return paths;
		}

		paths.reserve(value.as_array().size());
		for (const toml::value& item : value.as_array())
		{
			if (item.is_string())
			{
				paths.emplace_back(item.as_string());
			}
		}

		return paths;
	}

	std::optional<ProjectSettings> LoadProjectSettings(const std::filesystem::path& manifest_path, const std::filesystem::path& root, bool report_missing_project)
	{
		try
		{
			const toml::value data = toml::parse(manifest_path);
			const toml::value* project_table = FindTable(data, "project");
			if (project_table == nullptr)
			{
				if (report_missing_project)
				{
					Printer::Print<Printer::Color::RED>(
						std::format("[ProjectManifest] Missing [project] table in {}\n", manifest_path.string()));
				}
				return std::nullopt;
			}

			ProjectSettings settings;
			settings.m_root = root;
			settings.m_name = toml::find_or<std::string>(*project_table, "name", settings.m_name);
			settings.m_entry = toml::find_or<std::string>(*project_table, "entry", "");
			settings.m_source_dir = toml::find_or<std::string>(*project_table, "source_dir", "");
			settings.m_packages_dir = toml::find_or<std::string>(*project_table, "packages_dir", "");
			settings.m_prelude_dir = toml::find_or<std::string>(*project_table, "prelude_dir", "");

			if (project_table->contains("midori_path"))
			{
				settings.m_extra_paths = ReadPathArray(project_table->at("midori_path"));
			}

			return settings;
		}
		catch (const std::exception& e)
		{
			Printer::Print<Printer::Color::RED>(std::format("[ProjectManifest] Failed to parse {}: {}\n", manifest_path.string(), e.what()));
			return std::nullopt;
		}
	}

	std::optional<ProjectSettings> FindProjectSettings(const std::filesystem::path& start_dir)
	{
		std::filesystem::path current = start_dir;
		while (true)
		{
			const std::filesystem::path project_manifest = current / ProjectManifestFileName;
			if (std::filesystem::exists(project_manifest))
			{
				return LoadProjectSettings(project_manifest, current, true);
			}

			const std::filesystem::path package_manifest = current / PackageManifestFileName;
			if (std::filesystem::exists(package_manifest))
			{
				std::optional<ProjectSettings> settings = LoadProjectSettings(package_manifest, current, false);
				if (settings.has_value())
				{
					return settings;
				}
			}

			if (!current.has_parent_path())
			{
				break;
			}

			const std::filesystem::path parent = current.parent_path();
			if (parent == current)
			{
				break;
			}
			current = parent;
		}

		return std::nullopt;
	}

	std::optional<std::string> ReadEnvironmentVariable(const char* name)
	{
	#ifdef _WIN32
		char* value = nullptr;
		size_t len = 0u;
		if (_dupenv_s(&value, &len, name) != 0 || value == nullptr)
		{
			return std::nullopt;
		}

		std::string result(value);
		std::free(value);
		if (result.empty())
		{
			return std::nullopt;
		}

		return result;
	#else
		const char* value = std::getenv(name);
		if (value == nullptr || value[0] == '\0')
		{
			return std::nullopt;
		}

		return std::string(value);
	#endif
	}

	std::vector<std::filesystem::path> SplitSearchPaths(const std::string& path_str, char separator)
	{
		std::vector<std::filesystem::path> paths;
		size_t start = 0u;
		size_t end = path_str.find(separator);

		while (end != std::string::npos)
		{
			std::string segment = path_str.substr(start, end - start);
			if (!segment.empty())
			{
				paths.emplace_back(segment);
			}
			start = end + 1u;
			end = path_str.find(separator, start);
		}

		std::string last_segment = path_str.substr(start);
		if (!last_segment.empty())
		{
			paths.emplace_back(last_segment);
		}

		return paths;
	}

	std::filesystem::path NormalizePath(const std::filesystem::path& path)
	{
		std::error_code ec;
		std::filesystem::path absolute = path;
		if (!path.is_absolute())
		{
			absolute = std::filesystem::absolute(path, ec);
			if (ec)
			{
				absolute = path;
			}
		}

		std::filesystem::path normalized = std::filesystem::weakly_canonical(absolute, ec);
		if (ec)
		{
			normalized = absolute.lexically_normal();
		}
		return normalized;
	}

	std::string NormalizePathString(const std::filesystem::path& path)
	{
		std::string value = NormalizePath(path).string();
	#ifdef _WIN32
		std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c){ return static_cast<char>(std::tolower(c));});
	#endif
		return value;
	}

	bool ContainsPath(const std::vector<std::filesystem::path>& paths, const std::filesystem::path& candidate)
	{
		const std::string normalized_candidate = NormalizePathString(candidate);
		for (const std::filesystem::path& path : paths)
		{
			if (NormalizePathString(path) == normalized_candidate)
			{
				return true;
			}
		}
		return false;
	}

	void AppendUniquePath(std::vector<std::filesystem::path>& paths, const std::filesystem::path& candidate)
	{
		if (candidate.empty())
		{
			return;
		}

		std::error_code ec;
		if (!std::filesystem::exists(candidate, ec) || !std::filesystem::is_directory(candidate, ec))
		{
			return;
		}

		if (!ContainsPath(paths, candidate))
		{
			paths.push_back(candidate);
		}
	}

	std::vector<std::filesystem::path> BuildProjectSearchPaths(const ProjectSettings& settings)
	{
		std::vector<std::filesystem::path> paths;

		const std::filesystem::path default_source = settings.m_root / "src";
		const std::filesystem::path default_packages = settings.m_root / "packages";
		const std::filesystem::path default_prelude = settings.m_root / "MidoriPrelude";

		if (!settings.m_source_dir.empty())
		{
			AppendUniquePath(paths, settings.m_root / settings.m_source_dir);
		}
		else
		{
			AppendUniquePath(paths, default_source);
		}

		if (!settings.m_packages_dir.empty())
		{
			AppendUniquePath(paths, settings.m_root / settings.m_packages_dir);
		}
		else
		{
			AppendUniquePath(paths, default_packages);
		}

		for (const std::filesystem::path& extra : settings.m_extra_paths)
		{
			AppendUniquePath(paths, extra.is_absolute() ? extra : settings.m_root / extra);
		}

		if (!settings.m_prelude_dir.empty())
		{
			AppendUniquePath(paths, settings.m_root / settings.m_prelude_dir);
		}
		else
		{
			AppendUniquePath(paths, default_prelude);
		}

		return paths;
	}

	std::string JoinSearchPaths(const std::vector<std::filesystem::path>& paths, char separator)
	{
		std::string result;
		for (size_t i = 0uz; i < paths.size(); i += 1uz)
		{
			if (i != 0uz)
			{
				result.push_back(separator);
			}
			result.append(paths[i].string());
		}
		return result;
	}

	void SetEnvironmentVariable(const char* name, const std::string& value)
	{
	#ifdef _WIN32
		_putenv_s(name, value.c_str());
	#else
		setenv(name, value.c_str(), 1);
	#endif
	}

	std::string DeriveProjectName(const std::filesystem::path& root)
	{
		const std::string name = root.filename().string();
		if (!name.empty())
		{
			return name;
		}
		return "MidoriProject";
	}

	std::string EscapeTomlString(std::string_view value)
	{
		std::string result;
		result.reserve(value.size());
		for (char c : value)
		{
			switch (c)
			{
			case '\\':
				result.append("\\\\");
				break;
			case '"':
				result.append("\\\"");
				break;
			case '\n':
				result.append("\\n");
				break;
			case '\r':
				result.append("\\r");
				break;
			case '\t':
				result.append("\\t");
				break;
			default:
				result.push_back(c);
				break;
			}
		}
		return result;
	}

	bool WriteFile(const std::filesystem::path& path, std::string_view contents, std::string& error_message)
	{
		std::ofstream out(path, std::ios::binary);
		if (!out.is_open())
		{
			error_message = std::format("Failed to write file: {}", path.string());
			return false;
		}

		out.write(contents.data(), static_cast<std::streamsize>(contents.size()));
		if (!out)
		{
			error_message = std::format("Failed to write file: {}", path.string());
			return false;
		}

		return true;
	}
}

namespace MidoriProject
{
	void ApplyProjectManifestToEnvironment(const std::filesystem::path& input_path)
	{
		std::filesystem::path start_dir = input_path;
		std::error_code ec;
		if (std::filesystem::is_directory(start_dir, ec))
		{
			// Keep directory as-is.
		}
		else if (input_path.has_parent_path())
		{
			start_dir = input_path.parent_path();
		}
		else
		{
			start_dir = std::filesystem::current_path();
		}

		std::optional<ProjectSettings> settings = FindProjectSettings(start_dir);
		if (!settings.has_value())
		{
			return;
		}

		std::vector<std::filesystem::path> combined_paths = BuildProjectSearchPaths(settings.value());

		std::optional<std::string> env_value = ReadEnvironmentVariable("MIDORI_PATH");
	#ifdef _WIN32
		const char separator = ';';
	#else
		const char separator = ':';
	#endif
		if (env_value.has_value())
		{
			std::vector<std::filesystem::path> env_paths = SplitSearchPaths(env_value.value(), separator);
			for (const std::filesystem::path& path : env_paths)
			{
				AppendUniquePath(combined_paths, path);
			}
		}

		if (!combined_paths.empty())
		{
			SetEnvironmentVariable("MIDORI_PATH", JoinSearchPaths(combined_paths, separator));
		}
	}

	bool InitializeProject(const std::filesystem::path& target_dir, std::string_view project_name, std::string& error_message)
	{
		std::error_code ec;
		std::filesystem::path root = target_dir;
		if (root.empty())
		{
			root = std::filesystem::current_path(ec);
			if (ec)
			{
				error_message = "Failed to resolve current directory.";
				return false;
			}
		}

		if (!root.is_absolute())
		{
			root = std::filesystem::absolute(root, ec);
			if (ec)
			{
				error_message = "Failed to resolve project path.";
				return false;
			}
		}

		if (std::filesystem::exists(root, ec))
		{
			if (!std::filesystem::is_directory(root, ec))
			{
				error_message = std::format("Path is not a directory: {}", root.string());
				return false;
			}
		}
		else
		{
			if (!std::filesystem::create_directories(root, ec))
			{
				error_message = std::format("Failed to create directory: {}", root.string());
				return false;
			}
		}

		const std::filesystem::path manifest_path = root / ProjectManifestFileName;
		const std::filesystem::path src_dir = root / "src";
		const std::filesystem::path packages_dir = root / "packages";
		const std::filesystem::path main_path = src_dir / "Main.mdr";

		if (std::filesystem::exists(manifest_path, ec))
		{
			error_message = std::format("project.midori already exists at {}", manifest_path.string());
			return false;
		}

		if (std::filesystem::exists(main_path, ec))
		{
			error_message = std::format("Main module already exists at {}", main_path.string());
			return false;
		}

		if (!std::filesystem::create_directories(src_dir, ec))
		{
			if (ec)
			{
				error_message = std::format("Failed to create directory: {}", src_dir.string());
				return false;
			}
		}

		if (!std::filesystem::create_directories(packages_dir, ec))
		{
			if (ec)
			{
				error_message = std::format("Failed to create directory: {}", packages_dir.string());
				return false;
			}
		}

		const std::string resolved_name = project_name.empty() ? DeriveProjectName(root) : std::string(project_name);
		const std::string escaped_name = EscapeTomlString(resolved_name);
		const std::string manifest_contents = std::format
		(
			"[project]\n"
			"name = \"{}\"\n"
			"entry = \"src/Main.mdr\"\n"
			"source_dir = \"src\"\n"
			"packages_dir = \"packages\"\n"
			"prelude_dir = \"MidoriPrelude\"\n",
			escaped_name
		);

		if (!WriteFile(manifest_path, manifest_contents, error_message))
		{
			return false;
		}

		const std::string main_contents =
			"module Main\n"
			"\n"
			"defun main(): Int => 0;\n";

		if (!WriteFile(main_path, main_contents, error_message))
		{
			return false;
		}

		return true;
	}
}
