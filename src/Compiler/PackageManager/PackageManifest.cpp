#include "PackageManifest.h"
#include "Common/Printer/Printer.h"
#include <toml.hpp>
#include <fstream>

std::optional<PackageManifest> PackageManifest::Load(const std::filesystem::path& packageDirectory)
{
	const std::filesystem::path manifestPath = packageDirectory / "package.midori";

	if (!std::filesystem::exists(manifestPath))
	{
		Printer::PrintFormatted<Printer::Color::RED>("[PackageManifest] package.midori not found in: {}\n", packageDirectory.string());
		return std::nullopt;
	}

	try
	{
		const toml::value data = toml::parse(manifestPath);

		PackageManifest manifest;
		manifest.m_packageDirectory = packageDirectory;

		if (data.contains("package"))
		{
			const toml::value& pkg = data.at("package");
			manifest.m_info.m_name = toml::find_or<std::string>(pkg, "name", "");
			manifest.m_info.m_version = toml::find_or<std::string>(pkg, "version", "0.0.0");
			manifest.m_info.m_description = toml::find_or<std::string>(pkg, "description", "");
			manifest.m_info.m_license = toml::find_or<std::string>(pkg, "license", "");
			manifest.m_info.m_repository = toml::find_or<std::string>(pkg, "repository", "");
			manifest.m_info.m_midoriVersion = toml::find_or<std::string>(pkg, "midori_version", ">=1.0.0");

			if (pkg.contains("authors"))
			{
				const toml::value& authors = pkg.at("authors");
				if (authors.is_array())
				{
					for (const toml::value& author : authors.as_array())
					{
						manifest.m_info.m_authors.push_back(author.as_string());
					}
				}
			}

			if (pkg.contains("modules"))
			{
				const toml::value& modules = pkg.at("modules");
				manifest.m_modules.m_main = toml::find_or<std::string>(modules, "main", "");

				if (modules.contains("exports"))
				{
					const toml::value& exports = modules.at("exports");
					if (exports.is_array())
					{
						for (const toml::value& exp : exports.as_array())
						{
							manifest.m_modules.m_exports.push_back(exp.as_string());
						}
					}
				}
			}
		}

		if (data.contains("dependencies"))
		{
			const toml::value& deps = data.at("dependencies");
			for (const std::pair<const std::string, toml::value>& pair : deps.as_table())
			{
				manifest.m_dependencies.m_dependencies[pair.first] = pair.second.as_string();
			}
		}

		if (data.contains("ffi"))
		{
			const toml::value& ffi = data.at("ffi");
			manifest.m_ffi.m_enabled = toml::find_or<bool>(ffi, "enabled", false);
			manifest.m_ffi.m_libraryName = toml::find_or<std::string>(ffi, "library_name", "");

			if (ffi.contains("functions"))
			{
				const toml::value& functions = ffi.at("functions");
				for (const std::pair<const std::string, toml::value>& pair : functions.as_table())
				{
					manifest.m_ffi.m_functions[pair.first] = pair.second.as_string();
				}
			}
		}

		if (data.contains("build"))
		{
			const toml::value& build = data.at("build");
			manifest.m_build.m_cmakeMinimumVersion = toml::find_or<std::string>(build, "cmake_minimum_version", "3.24");
			manifest.m_build.m_cppStandard = toml::find_or<std::string>(build, "cpp_standard", "23");
		}

		if (data.contains("prebuilt"))
		{
			const toml::value& prebuilt = data.at("prebuilt");

			if (prebuilt.contains("windows_x64"))
			{
				const toml::value& win = prebuilt.at("windows_x64");
				PrebuiltBinary binary;
				binary.m_path = toml::find_or<std::string>(win, "path", "");
				binary.m_checksum = toml::find_or<std::string>(win, "checksum", "");
				manifest.m_prebuilt.m_windowsX64 = binary;
			}

			if (prebuilt.contains("linux_x86_64"))
			{
				const toml::value& linux_val = prebuilt.at("linux_x86_64");
				PrebuiltBinary binary;
				binary.m_path = toml::find_or<std::string>(linux_val, "path", "");
				binary.m_checksum = toml::find_or<std::string>(linux_val, "checksum", "");
				manifest.m_prebuilt.m_linuxX86_64 = binary;
			}

			if (prebuilt.contains("macos_arm64"))
			{
				const toml::value& macos = prebuilt.at("macos_arm64");
				PrebuiltBinary binary;
				binary.m_path = toml::find_or<std::string>(macos, "path", "");
				binary.m_checksum = toml::find_or<std::string>(macos, "checksum", "");
				manifest.m_prebuilt.m_macosArm64 = binary;
			}

			if (prebuilt.contains("macos_x86_64"))
			{
				const toml::value& macos = prebuilt.at("macos_x86_64");
				PrebuiltBinary binary;
				binary.m_path = toml::find_or<std::string>(macos, "path", "");
				binary.m_checksum = toml::find_or<std::string>(macos, "checksum", "");
				manifest.m_prebuilt.m_macosX86_64 = binary;
			}
		}

		Printer::PrintFormatted<Printer::Color::GREEN>("[PackageManifest] Successfully loaded package: {} v{}\n",
			manifest.m_info.m_name, manifest.m_info.m_version);

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

#ifdef _WIN32
	if (m_prebuilt.m_windowsX64.has_value())
	{
		return m_packageDirectory / m_prebuilt.m_windowsX64->m_path;
	}
	return m_packageDirectory / "lib" / "windows" / "x64" / (m_ffi.m_libraryName + ".dll");
#elif defined(__APPLE__)
	#if defined(__aarch64__) || defined(_M_ARM64)
		if (m_prebuilt.m_macosArm64.has_value())
		{
			return m_packageDirectory / m_prebuilt.m_macosArm64->m_path;
		}
	#else
		if (m_prebuilt.m_macosX86_64.has_value())
		{
			return m_packageDirectory / m_prebuilt.m_macosX86_64->m_path;
		}
	#endif
	return m_packageDirectory / "lib" / "macos" / ("lib" + m_ffi.m_libraryName + ".dylib");
#else
	if (m_prebuilt.m_linuxX86_64.has_value())
	{
		return m_packageDirectory / m_prebuilt.m_linuxX86_64->m_path;
	}
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
