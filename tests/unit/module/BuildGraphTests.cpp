#include <catch2/catch_test_macros.hpp>

#include "Compiler/BuildGraph/BuildGraph.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/ModuleManager/ModuleManager.h"
#include "Compiler/Token/Token.h"
#include "support/CompileHelpers.h"
#include "support/SourceFixture.h"
#include "support/TempProject.h"

#include <algorithm>
#include <expected>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

namespace
{
	std::expected<BuildGraph, CompilerError> GenerateBuildGraphFromFile(const std::filesystem::path& file_path)
	{
		std::ifstream input_file(file_path);
		if (!input_file.is_open())
		{
			return std::unexpected(CompilerError::Simple(CompilerStage::Module, "Could not open a test module."));
		}

		std::ostringstream buffer;
		buffer << input_file.rdbuf();

		MidoriTest::SourceFixture source_fixture(buffer.str(), file_path.string());
		MidoriResult::LexerResult lex_result = Lexer(std::string(source_fixture.SourceCode()), source_fixture.FileName()).Lex();
		if (!lex_result.has_value())
		{
			return std::unexpected(std::move(lex_result.error()));
		}

		return ModuleManager(std::move(lex_result.value()), source_fixture.FileName(), source_fixture.SourceLines()).GenerateBuildGraph();
	}
}

TEST_CASE("BuildGraph computes deterministic compilation tiers for shared dependencies", "[module][graph]")
{
	BuildGraph graph;

	BuildGraph::BuildNode core_node;
	core_node.m_file_name = "Core";
	graph.m_nodes.emplace(core_node.m_file_name, core_node);

	BuildGraph::BuildNode lib_node;
	lib_node.m_file_name = "Lib";
	lib_node.m_dependencies = { "Core" };
	graph.m_nodes.emplace(lib_node.m_file_name, lib_node);

	BuildGraph::BuildNode util_node;
	util_node.m_file_name = "Util";
	util_node.m_dependencies = { "Core" };
	graph.m_nodes.emplace(util_node.m_file_name, util_node);

	BuildGraph::BuildNode main_node;
	main_node.m_file_name = "Main";
	main_node.m_dependencies = { "Lib", "Util" };
	graph.m_nodes.emplace(main_node.m_file_name, main_node);

	const std::vector<std::vector<std::string>> expected_tiers
	{
		{ "Core" },
		{ "Lib", "Util" },
		{ "Main" }
	};

	REQUIRE(graph.GetCompilationTiers() == expected_tiers);
}

TEST_CASE("ModuleManager preserves dependency metadata and strips module statements from nodes", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"Main.mdr",
			"module Main\n"
			"import { \"./Lib.mdr\", \"./Util.mdr\" }\n"
			"use Lib.{PrintLine, Parse}\n"
			"defun main(): Int => 0;\n"
		),
		MidoriTest::TempProjectFile
		(
			"Lib.mdr",
			"module Lib\n"
			"public export { PrintLine, Parse }\n"
			"defun PrintLine(): Int => 0;\n"
			"defun Parse(): Int => 0;\n"
		),
		MidoriTest::TempProjectFile
		(
			"Util.mdr",
			"module Util\n"
			"def value = 1;\n"
		)
	});

	const std::filesystem::path main_file_path = std::filesystem::weakly_canonical(project.Path("Main.mdr"));
	const std::filesystem::path lib_file_path = std::filesystem::weakly_canonical(project.Path("Lib.mdr"));
	const std::filesystem::path util_file_path = std::filesystem::weakly_canonical(project.Path("Util.mdr"));

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(main_file_path);
	if (!graph_result.has_value())
	{
		FAIL(std::string(graph_result.error().Rendered()));
	}

	const BuildGraph& graph = graph_result.value();
	REQUIRE(graph.m_nodes.size() == 3);
	REQUIRE(graph.m_nodes.contains(main_file_path.string()));
	REQUIRE(graph.m_nodes.contains(lib_file_path.string()));
	REQUIRE(graph.m_nodes.contains(util_file_path.string()));

	const BuildGraph::BuildNode& main_node = graph.m_nodes.at(main_file_path.string());
	REQUIRE(main_node.m_dependencies == std::vector<std::string>{ lib_file_path.string(), util_file_path.string() });

	REQUIRE(graph.m_use_imports.contains(main_file_path.string()));
	REQUIRE(graph.m_use_imports.at(main_file_path.string()).size() == 2);
	CHECK(graph.m_use_imports.at(main_file_path.string())[0].m_module_name == "Lib");
	CHECK(graph.m_use_imports.at(main_file_path.string())[0].m_symbol_name == "PrintLine");
	CHECK(graph.m_use_imports.at(main_file_path.string())[1].m_module_name == "Lib");
	CHECK(graph.m_use_imports.at(main_file_path.string())[1].m_symbol_name == "Parse");

	REQUIRE(graph.m_module_declarations.at(main_file_path.string()).ModuleName() == "Main");
	REQUIRE(graph.m_module_declarations.at(lib_file_path.string()).ModuleName() == "Lib");
	REQUIRE(graph.m_module_declarations.at(lib_file_path.string()).HasExport("PrintLine"));
	REQUIRE(graph.m_module_declarations.at(lib_file_path.string()).HasExport("Parse"));

	const std::vector<Token::Name> remaining_tokens = MidoriTest::CollectTokenNames(main_node.m_tokens);
	CHECK(std::ranges::find(remaining_tokens, Token::Name::MODULE) == remaining_tokens.end());
	CHECK(std::ranges::find(remaining_tokens, Token::Name::IMPORT) == remaining_tokens.end());
	CHECK(std::ranges::find(remaining_tokens, Token::Name::USE) == remaining_tokens.end());
	CHECK(std::ranges::find(remaining_tokens, Token::Name::DEFUN) != remaining_tokens.end());

	const std::vector<std::vector<std::string>> expected_tiers
	{
		{ lib_file_path.string(), util_file_path.string() },
		{ main_file_path.string() }
	};
	REQUIRE(graph.GetCompilationTiers() == expected_tiers);
}

TEST_CASE("ModuleManager preserves dotted module names in use imports", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"Main.mdr",
			"module Main\n"
			"import { \"./MathVector.mdr\" }\n"
			"use Math.Vector.{add, sub}\n"
			"defun main(): Int => 0;\n"
		),
		MidoriTest::TempProjectFile
		(
			"MathVector.mdr",
			"module Math.Vector\n"
			"public export { add, sub }\n"
			"def add = 1;\n"
			"def sub = 0;\n"
		)
	});

	const std::filesystem::path main_file_path = std::filesystem::weakly_canonical(project.Path("Main.mdr"));

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(main_file_path);
	if (!graph_result.has_value())
	{
		FAIL(std::string(graph_result.error().Rendered()));
	}

	const BuildGraph& graph = graph_result.value();
	REQUIRE(graph.m_use_imports.contains(main_file_path.string()));
	REQUIRE(graph.m_use_imports.at(main_file_path.string()).size() == 2u);
	CHECK(graph.m_use_imports.at(main_file_path.string())[0].m_module_name == "Math.Vector");
	CHECK(graph.m_use_imports.at(main_file_path.string())[0].m_symbol_name == "add");
	CHECK(graph.m_use_imports.at(main_file_path.string())[1].m_module_name == "Math.Vector");
	CHECK(graph.m_use_imports.at(main_file_path.string())[1].m_symbol_name == "sub");
}

TEST_CASE("ModuleManager requires an explicit module declaration in every file", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"MissingModule.mdr",
			"def value = 1;\n"
		)
	});

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(project.Path("MissingModule.mdr"));
	REQUIRE_FALSE(graph_result.has_value());

	const CompilerError& error = graph_result.error();
	CHECK(error.m_stage == CompilerStage::Module);
	CHECK(error.m_message.find("Module declaration required") != std::string::npos);
	CHECK(error.m_message.find("exactly one") != std::string::npos);
}

TEST_CASE("ModuleManager rejects multiple module declarations in one file", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"MultipleModules.mdr",
			"module First\n"
			"module Second\n"
			"def value = 1;\n"
		)
	});

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(project.Path("MultipleModules.mdr"));
	REQUIRE_FALSE(graph_result.has_value());

	const CompilerError& error = graph_result.error();
	CHECK(error.m_stage == CompilerStage::Module);
	CHECK(error.m_message.find("Multiple module declarations") != std::string::npos);
	CHECK(error.m_message.find("exactly one") != std::string::npos);
}

TEST_CASE("ModuleManager requires the module declaration to be the first top-level statement", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"LateModule.mdr",
			"def value = 1;\n"
			"module Late\n"
		)
	});

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(project.Path("LateModule.mdr"));
	REQUIRE_FALSE(graph_result.has_value());

	const CompilerError& error = graph_result.error();
	CHECK(error.m_stage == CompilerStage::Module);
	CHECK(error.m_message.find("first top-level statement") != std::string::npos);
}

TEST_CASE("ModuleManager returns a diagnostic for reserved-keyword module names", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile(
			"Invalid.mdr",
			"module def\n"
			"def value = 1;\n"
		)
	});

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(project.Path("Invalid.mdr"));
	REQUIRE_FALSE(graph_result.has_value());

	const CompilerError& error = graph_result.error();
	CHECK(error.m_stage == CompilerStage::Module);
	CHECK(error.m_message.find("reserved keyword") != std::string::npos);
	CHECK(error.m_message.find("cannot be used as a module name") != std::string::npos);
}

TEST_CASE("ModuleManager returns a diagnostic for malformed dotted use syntax", "[module][graph]")
{
	const MidoriTest::TempProject project
	({
		MidoriTest::TempProjectFile
		(
			"InvalidUse.mdr",
			"module Main\n"
			"use Math..{add}\n"
			"def value = 1;\n"
		)
	});

	std::expected<BuildGraph, CompilerError> graph_result = GenerateBuildGraphFromFile(project.Path("InvalidUse.mdr"));
	REQUIRE_FALSE(graph_result.has_value());

	const CompilerError& error = graph_result.error();
	CHECK(error.m_stage == CompilerStage::Module);
	CHECK(std::string(error.Rendered()).find("use statement") != std::string::npos);
}
