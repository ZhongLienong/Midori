#pragma once

#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"
#include "Compiler/TypeChecker/TypeChecker.h"
#include "Compiler/BytecodeModule/BytecodeModule.h"
#include "Compiler/Module/Module.h"
#include <string>
#include <filesystem>
#include <unordered_set>
#include <unordered_map>
#include <string_view>
#include <optional>

struct CompiledModule
{
	std::string m_module_name;
	std::filesystem::path m_file_path;

	struct SymbolTable
	{
		std::unordered_set<std::string> m_exports;
		std::unordered_map<std::string, VisibilityLevel> m_export_visibility;

		bool HasExport(std::string_view name) const;

		VisibilityLevel GetExportVisibility(std::string_view name) const;
	};

	SymbolTable m_symbols;
	TypeChecker::TypeEnvironment m_type_signatures;
	std::optional<BytecodeModule> m_bytecode;        // Per-module bytecode for incremental compilation

	CompiledModule(std::string module_name, std::filesystem::path file_path, SymbolTable symbols, TypeChecker::TypeEnvironment type_signatures = {});

	CompiledModule(const CompiledModule&) = delete;
	CompiledModule& operator=(const CompiledModule&) = delete;
	CompiledModule(CompiledModule&&) noexcept = default;
	CompiledModule& operator=(CompiledModule&&) noexcept = default;
};
