#include <catch2/catch_test_macros.hpp>

#include "Compiler/ImportResolver/ImportResolver.h"
#include "support/ScopedEnvVar.h"
#include "support/TempProject.h"

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace
{
#ifdef _WIN32
	constexpr char s_search_path_separator = ';';
#else
	constexpr char s_search_path_separator = ':';
#endif
}

TEST_CASE("ImportResolver resolves relative path imports against the current file", "[module][import]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile("src/Main.mdr", "module Main\n"),
		MidoriTest::TempProjectFile("src/lib/Helper.mdr", "module Helper\n")
	});

	const std::filesystem::path main_file_path = std::filesystem::weakly_canonical(project.Path("src/Main.mdr"));
	const std::filesystem::path helper_file_path = std::filesystem::weakly_canonical(project.Path("src/lib/Helper.mdr"));

	const ImportResolver resolver(main_file_path.string());
	const std::optional<ImportResolver::ResolvedImport> resolved_import = resolver.Resolve("./lib/Helper.mdr");

	REQUIRE(resolved_import.has_value());
	CHECK(resolved_import->m_type == ImportResolver::ImportType::PATH);
	CHECK(resolved_import->m_original_specifier == "./lib/Helper.mdr");
	CHECK(resolved_import->m_absolute_path == helper_file_path.string());
}

TEST_CASE("ImportResolver resolves dotted system imports from explicit search paths", "[module][import]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile("Main.mdr", "module Main\n"),
		MidoriTest::TempProjectFile("stdlib/Std/IO.mdr", "module Std.IO\n")
	});

	const std::filesystem::path main_file_path = std::filesystem::weakly_canonical(project.Path("Main.mdr"));
	const std::filesystem::path io_module_path = std::filesystem::weakly_canonical(project.Path("stdlib/Std/IO.mdr"));

	const ImportResolver resolver(main_file_path.string());
	const ImportResolver configured_resolver = resolver.WithSystemSearchPaths({ project.Path("stdlib") });
	const std::optional<ImportResolver::ResolvedImport> resolved_import = configured_resolver.Resolve("<Std.IO>");

	REQUIRE(resolved_import.has_value());
	CHECK(resolved_import->m_type == ImportResolver::ImportType::SYSTEM);
	CHECK(resolved_import->m_original_specifier == "<Std.IO>");
	CHECK(resolved_import->m_absolute_path == io_module_path.string());
}

TEST_CASE("ImportResolver canonicalizes MIDORI_PATH entries and ignores missing directories", "[module][import]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile("Main.mdr", "module Main\n"),
		MidoriTest::TempProjectFile("stdlib/Std/Math.mdr", "module Std.Math\n")
	});

	const std::filesystem::path main_file_path = std::filesystem::weakly_canonical(project.Path("Main.mdr"));
	const std::filesystem::path stdlib_path = std::filesystem::weakly_canonical(project.Path("stdlib"));
	const std::filesystem::path math_module_path = std::filesystem::weakly_canonical(project.Path("stdlib/Std/Math.mdr"));
	const std::string midori_path_value = project.Path("missing").string() + std::string(1, s_search_path_separator) + project.Path("stdlib").string();
	const MidoriTest::ScopedEnvVar midori_path("MIDORI_PATH", midori_path_value);

	const ImportResolver resolver(main_file_path.string());

	REQUIRE(resolver.GetSystemSearchPaths().size() == 1);
	CHECK(resolver.GetSystemSearchPaths().front() == stdlib_path);

	const std::optional<ImportResolver::ResolvedImport> resolved_import = resolver.Resolve("<Std.Math>");
	REQUIRE(resolved_import.has_value());
	CHECK(resolved_import->m_absolute_path == math_module_path.string());
}

TEST_CASE("ImportResolver returns nullopt for missing imports", "[module][import]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile("Main.mdr", "module Main\n")
	});

	const std::filesystem::path main_file_path = std::filesystem::weakly_canonical(project.Path("Main.mdr"));

	const ImportResolver resolver(main_file_path.string());
	const ImportResolver configured_resolver = resolver.WithSystemSearchPaths({ project.Root() / "stdlib" });

	CHECK_FALSE(configured_resolver.Resolve("./Missing.mdr").has_value());
	CHECK_FALSE(configured_resolver.Resolve("<Std.Missing>").has_value());
}
