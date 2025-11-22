#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <filesystem>
#include <memory>

struct Token;

#include "Compiler/AbstractSyntaxTree/Type.h"

enum class VisibilityLevel
{
	Public,      // Exported publicly (accessible to all importers)
	Private,     // Exported privately (accessible only to same namespace modules)
	Internal     // Not exported (module-internal only)
};

struct ModuleExport
{
	std::string m_symbol_name;
	VisibilityLevel m_visibility;

	ModuleExport(std::string_view symbol_name, VisibilityLevel visibility);
};

// Represents a symbol imported via 'use' statement
// e.g., "use IO.{Print, PrintLine}" creates UseImport entries for Print and PrintLine
struct UseImport
{
	std::string m_module_name;      // The module name (e.g., "IO")
	std::string m_symbol_name;      // The symbol name (e.g., "Print")

	UseImport(std::string_view module_name, std::string_view symbol_name);
};

struct ModuleDeclaration
{
	std::string m_module_name;         
	std::filesystem::path m_file_path; 
	std::vector<ModuleExport> m_exports;  
	std::vector<std::string> m_imports;  
	bool m_has_module_declaration;          // True if file has explicit "module X" declaration

	ModuleDeclaration();

	ModuleDeclaration(std::string_view module_name, const std::filesystem::path& file_path);

	bool HasExport(std::string_view symbol_name) const;

	VisibilityLevel GetExportVisibility(std::string_view symbol_name) const;
};
