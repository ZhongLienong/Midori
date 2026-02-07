#include "PackageManifest.h"
#include "Common/Printer/Printer.h"
#include <toml.hpp>
#include <utility>

namespace
{
	constexpr const char* kManifestFileName = "package.midori";

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

	std::vector<std::string> ReadStringArray(const toml::value& value)
	{
		std::vector<std::string> items;
		if (!value.is_array())
		{
			return items;
		}

		items.reserve(value.as_array().size());

		for (const toml::value& item : value.as_array())
		{
			items.push_back(item.as_string());
		}

		return items;
	}

	std::unordered_map<std::string, std::string> ReadStringTable(const toml::value& value)
	{
		std::unordered_map<std::string, std::string> entries;
		for (const std::pair<const std::string, toml::value>& entry : value.as_table())
		{
			entries.emplace(entry.first, entry.second.as_string());
		}

		return entries;
	}

	PackageInfo ParsePackageInfo(const toml::value& data)
	{
		PackageInfo info;
		const toml::value* package_table = FindTable(data, "package");
		if (package_table == nullptr)
		{
			return info;
		}

		const toml::value& pkg = *package_table;
		info.m_name = toml::find_or<std::string>(pkg, "name", info.m_name);
		info.m_version = toml::find_or<std::string>(pkg, "version", info.m_version);
		info.m_description = toml::find_or<std::string>(pkg, "description", info.m_description);
		info.m_license = toml::find_or<std::string>(pkg, "license", info.m_license);
		info.m_repository = toml::find_or<std::string>(pkg, "repository", info.m_repository);
		info.m_midori_version = toml::find_or<std::string>(pkg, "midori_version", info.m_midori_version);

		if (pkg.contains("authors"))
		{
			info.m_authors = ReadStringArray(pkg.at("authors"));
		}

		return info;
	}

	PackageModules ParsePackageModules(const toml::value& data)
	{
		PackageModules modules;
		const toml::value* package_table = FindTable(data, "package");
		if (package_table == nullptr)
		{
			return modules;
		}

		const toml::value* modules_table = FindTable(*package_table, "modules");
		if (modules_table == nullptr)
		{
			return modules;
		}

		modules.m_main = toml::find_or<std::string>(*modules_table, "main", modules.m_main);
		if (modules_table->contains("exports"))
		{
			modules.m_exports = ReadStringArray(modules_table->at("exports"));
		}

		return modules;
	}

	PackageDependencies ParsePackageDependencies(const toml::value& data)
	{
		PackageDependencies dependencies;
		const toml::value* deps_table = FindTable(data, "dependencies");
		if (deps_table == nullptr)
		{
			return dependencies;
		}

		dependencies.m_dependencies = ReadStringTable(*deps_table);
		return dependencies;
	}

	PackageFFI ParsePackageFFI(const toml::value& data)
	{
		PackageFFI ffi;
		const toml::value* ffi_table = FindTable(data, "ffi");
		if (ffi_table == nullptr)
		{
			return ffi;
		}

		ffi.m_enabled = toml::find_or<bool>(*ffi_table, "enabled", ffi.m_enabled);
		ffi.m_libraryName = toml::find_or<std::string>(*ffi_table, "library_name", ffi.m_libraryName);

		if (ffi_table->contains("functions"))
		{
			ffi.m_functions = ReadStringTable(ffi_table->at("functions"));
		}

		return ffi;
	}

	PackageBuild ParsePackageBuild(const toml::value& data)
	{
		PackageBuild build;
		const toml::value* build_table = FindTable(data, "build");
		if (build_table == nullptr)
		{
			return build;
		}

		build.m_cmakeMinimumVersion = toml::find_or<std::string>(*build_table, "cmake_minimum_version", build.m_cmakeMinimumVersion);
		build.m_cppStandard = toml::find_or<std::string>(*build_table, "cpp_standard", build.m_cppStandard);
		return build;
	}

	std::optional<PrebuiltBinary> ParsePrebuiltBinary(const toml::value& prebuilt_table, const char* key)
	{
		if (!prebuilt_table.contains(key))
		{
			return std::nullopt;
		}

		const toml::value& table = prebuilt_table.at(key);
		PrebuiltBinary binary;
		binary.m_path = toml::find_or<std::string>(table, "path", "");
		binary.m_checksum = toml::find_or<std::string>(table, "checksum", "");
		return binary;
	}

	PackagePrebuilt ParsePackagePrebuilt(const toml::value& data)
	{
		PackagePrebuilt prebuilt;
		const toml::value* prebuilt_table = FindTable(data, "prebuilt");
		if (prebuilt_table == nullptr)
		{
			return prebuilt;
		}

		prebuilt.m_windowsX64 = ParsePrebuiltBinary(*prebuilt_table, "windows_x64");
		prebuilt.m_linuxX86_64 = ParsePrebuiltBinary(*prebuilt_table, "linux_x86_64");
		prebuilt.m_macosArm64 = ParsePrebuiltBinary(*prebuilt_table, "macos_arm64");
		prebuilt.m_macosX86_64 = ParsePrebuiltBinary(*prebuilt_table, "macos_x86_64");
		return prebuilt;
	}

	PackageManifest BuildManifest(const std::filesystem::path& packageDirectory, const toml::value& data)
	{
		return PackageManifest::Create(packageDirectory)
			.WithInfo(ParsePackageInfo(data))
			.WithModules(ParsePackageModules(data))
			.WithDependencies(ParsePackageDependencies(data))
			.WithFFI(ParsePackageFFI(data))
			.WithBuild(ParsePackageBuild(data))
			.WithPrebuilt(ParsePackagePrebuilt(data));
	}

	std::optional<std::filesystem::path> SelectPrebuiltLibraryPath(
		const PackagePrebuilt& prebuilt,
		const std::filesystem::path& packageDirectory)
	{
#ifdef _WIN32
		if (prebuilt.m_windowsX64.has_value())
		{
			return packageDirectory / prebuilt.m_windowsX64->m_path;
		}
#elif defined(__APPLE__)
		#if defined(__aarch64__) || defined(_M_ARM64)
			if (prebuilt.m_macosArm64.has_value())
			{
				return packageDirectory / prebuilt.m_macosArm64->m_path;
			}
		#else
			if (prebuilt.m_macosX86_64.has_value())
			{
				return packageDirectory / prebuilt.m_macosX86_64->m_path;
			}
		#endif
#else
		if (prebuilt.m_linuxX86_64.has_value())
		{
			return packageDirectory / prebuilt.m_linuxX86_64->m_path;
		}
#endif
		return std::nullopt;
	}
}

PackageManifest PackageManifest::Create(std::filesystem::path packageDirectory)
{
	PackageManifest manifest;
	manifest.m_packageDirectory = std::move(packageDirectory);
	return manifest;
}

PackageManifest PackageManifest::WithInfo(PackageInfo info) &&
{
	m_info = std::move(info);
	return std::move(*this);
}

PackageManifest PackageManifest::WithModules(PackageModules modules) &&
{
	m_modules = std::move(modules);
	return std::move(*this);
}

PackageManifest PackageManifest::WithDependencies(PackageDependencies dependencies) &&
{
	m_dependencies = std::move(dependencies);
	return std::move(*this);
}

PackageManifest PackageManifest::WithFFI(PackageFFI ffi) &&
{
	m_ffi = std::move(ffi);
	return std::move(*this);
}

PackageManifest PackageManifest::WithBuild(PackageBuild build) &&
{
	m_build = std::move(build);
	return std::move(*this);
}

PackageManifest PackageManifest::WithPrebuilt(PackagePrebuilt prebuilt) &&
{
	m_prebuilt = std::move(prebuilt);
	return std::move(*this);
}

std::optional<PackageManifest> PackageManifest::Load(const std::filesystem::path& packageDirectory)
{
	const std::filesystem::path manifest_path = packageDirectory / kManifestFileName;

	if (!std::filesystem::exists(manifest_path))
	{
		Printer::PrintFormatted<Printer::Color::RED>("[PackageManifest] package.midori not found in: {}\n", packageDirectory.string());
		return std::nullopt;
	}

	try
	{
		const toml::value data = toml::parse(manifest_path);
		PackageManifest manifest = BuildManifest(packageDirectory, data);

		Printer::PrintFormatted<Printer::Color::GREEN>("[PackageManifest] Successfully loaded package: {} v{}\n",
			manifest.GetInfo().m_name, manifest.GetInfo().m_version);

		return manifest;
	}
	catch (const std::exception& e)
	{
		Printer::PrintFormatted<Printer::Color::RED>("[PackageManifest] Failed to parse package.midori: {}\n", e.what());
		return std::nullopt;
	}
}

std::filesystem::path PackageManifest::GetMainModulePath() const
{
	if (m_modules.m_main.empty())
	{
		return {};
	}

	return m_packageDirectory / m_modules.m_main;
}

std::filesystem::path PackageManifest::GetFFILibraryPath() const
{
	if (!m_ffi.m_enabled || m_ffi.m_libraryName.empty())
	{
		return {};
	}

	const std::optional<std::filesystem::path> prebuilt_path = SelectPrebuiltLibraryPath(m_prebuilt, m_packageDirectory);
	if (prebuilt_path.has_value())
	{
		return prebuilt_path.value();
	}

#ifdef _WIN32
	return m_packageDirectory / "lib" / "windows" / "x64" / (m_ffi.m_libraryName + ".dll");
#elif defined(__APPLE__)
	return m_packageDirectory / "lib" / "macos" / ("lib" + m_ffi.m_libraryName + ".dylib");
#else
	return m_packageDirectory / "lib" / "linux" / "x86_64" / ("lib" + m_ffi.m_libraryName + ".so");
#endif
}

const PackageInfo& PackageManifest::GetInfo() const
{
	return m_info;
}

const PackageModules& PackageManifest::GetModules() const
{
	return m_modules;
}

const PackageDependencies& PackageManifest::GetDependencies() const
{
	return m_dependencies;
}

const PackageFFI& PackageManifest::GetFFI() const
{
	return m_ffi;
}

const PackageBuild& PackageManifest::GetBuild() const
{
	return m_build;
}

const PackagePrebuilt& PackageManifest::GetPrebuilt() const
{
	return m_prebuilt;
}

const std::filesystem::path& PackageManifest::GetPackageDirectory() const
{
	return m_packageDirectory;
}
