#pragma once

#include <filesystem>

#include "Common/Executable/Executable.h"
#include "Common/Value/Value.h"
#include "Compiler/CodeGenerator/GenericFunctionInfo.h"

struct BytecodeModule
{
	enum class SymbolType
	{
		FUNCTION,
		FOREIGN_FUNCTION,
		GLOBAL_VARIABLE,
		STRUCT_TYPE,
		UNION_TYPE
	};

	struct ExportedSymbol
	{
		std::string m_name;
		size_t m_procedure_index;  // Index into m_procedures for functions
		size_t m_global_index;     // Index into m_global_variables where the symbol is stored
		SymbolType m_type;

		ExportedSymbol(std::string name, size_t proc_index, size_t global_index, SymbolType type);

		ExportedSymbol(std::string name, size_t index, SymbolType type);
	};
	struct ImportedSymbol
	{
		std::string m_name;
		std::string m_from_module;
		std::vector<size_t> m_usage_locations;

		ImportedSymbol(std::string name, std::string from_module);
	};

	std::string m_module_name;
	std::filesystem::path m_source_path;
	std::vector<BytecodeStream> m_procedures;
	std::vector<ExportedSymbol> m_exports;
	std::vector<ImportedSymbol> m_imports;
	std::vector<std::string> m_string_pool;
	std::vector<MidoriText> m_procedure_names;
	std::vector<MidoriText> m_global_variables;
	std::unordered_map<std::string, GenericFunctionInfo> m_generic_functions;

	BytecodeModule() = default;
	BytecodeModule(std::string module_name, std::filesystem::path source_path);

	BytecodeModule(const BytecodeModule&) = delete;
	BytecodeModule& operator=(const BytecodeModule&) = delete;
	BytecodeModule(BytecodeModule&&) noexcept = default;
	BytecodeModule& operator=(BytecodeModule&&) noexcept = default;
};
