#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

struct PackageInfo
{
	std::string m_name;
	std::string m_version;
	std::vector<std::string> m_authors;
	std::string m_description;
	std::string m_license;
	std::string m_repository;
	std::string m_midoriVersion;
};

struct PackageModules
{
	std::string m_main;
	std::vector<std::string> m_exports;
};

struct PackageDependencies
{
	std::unordered_map<std::string, std::string> m_dependencies;
};

struct PackageFFI
{
	bool m_enabled;
	std::string m_libraryName;
	std::unordered_map<std::string, std::string> m_functions;
};

struct PackageBuild
{
	std::string m_cmakeMinimumVersion;
	std::string m_cppStandard;
};

struct PrebuiltBinary
{
	std::filesystem::path m_path;
	std::string m_checksum;
};

struct PackagePrebuilt
{
	std::optional<PrebuiltBinary> m_windowsX64;
	std::optional<PrebuiltBinary> m_linuxX86_64;
	std::optional<PrebuiltBinary> m_macosArm64;
	std::optional<PrebuiltBinary> m_macosX86_64;
};

class PackageManifest
{
public:
	static std::optional<PackageManifest> Load(const std::filesystem::path& packageDirectory);

	const PackageInfo& GetInfo() const;
	const PackageModules& GetModules() const;
	const PackageDependencies& GetDependencies() const;
	const PackageFFI& GetFFI() const;
	const PackageBuild& GetBuild() const;
	const PackagePrebuilt& GetPrebuilt() const;

	const std::filesystem::path& GetPackageDirectory() const;
	std::filesystem::path GetMainModulePath() const;
	std::filesystem::path GetFFILibraryPath() const;

private:
	PackageManifest() = default;

	PackageInfo m_info;
	PackageModules m_modules;
	PackageDependencies m_dependencies;
	PackageFFI m_ffi;
	PackageBuild m_build;
	PackagePrebuilt m_prebuilt;
	std::filesystem::path m_packageDirectory;
};
