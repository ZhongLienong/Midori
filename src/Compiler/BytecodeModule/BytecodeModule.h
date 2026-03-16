#pragma once

#include <cstdint>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

#include "Common/Executable/Executable.h"
#include "Common/Value/Value.h"
#include "Compiler/CodeGenerator/GenericFunctionInfo.h"

struct BytecodeModule
{
	enum class SymbolType : std::uint8_t
	{
		FUNCTION,
		FOREIGN_FUNCTION,
		GLOBAL_VARIABLE,
		STRUCT_TYPE,
		UNION_TYPE
	};

	struct ExportedSymbol
	{
		size_t m_procedure_index;  // Index into m_procedures for functions
		size_t m_global_index;     // Index into m_global_variables where the symbol is stored
		SymbolType m_type;
		std::string m_name;

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

	using ProcedureList = std::vector<BytecodeStream>;
	using ExportList = std::vector<ExportedSymbol>;
	using ImportList = std::vector<ImportedSymbol>;
	using StringPool = std::vector<std::string>;
	using ProcedureNameList = std::vector<MidoriText>;
	using GlobalVariableList = std::vector<MidoriText>;
	using GenericFunctionMap = std::unordered_map<std::string, GenericFunctionInfo>;

	std::string m_module_name;
	std::filesystem::path m_source_path;
	ProcedureList m_procedures;
	ExportList m_exports;
	ImportList m_imports;
	StringPool m_string_pool;
	ProcedureNameList m_procedure_names;
	GlobalVariableList m_global_variables;
	GenericFunctionMap m_generic_functions;

	BytecodeModule() = default;
	BytecodeModule(std::string module_name, std::filesystem::path source_path);

	[[nodiscard]] BytecodeModule WithProcedure(BytecodeStream procedure) &&;
	[[nodiscard]] BytecodeModule WithExport(ExportedSymbol export_symbol) &&;
	[[nodiscard]] BytecodeModule WithImport(ImportedSymbol import_symbol) &&;
	[[nodiscard]] BytecodeModule WithString(std::string str) &&;
	[[nodiscard]] BytecodeModule WithProcedureName(MidoriText name) &&;
	[[nodiscard]] BytecodeModule WithGlobalVariable(MidoriText variable) &&;
	[[nodiscard]] BytecodeModule WithGenericFunction(std::string name, GenericFunctionInfo info) &&;

	BytecodeModule(const BytecodeModule&) = delete;
	BytecodeModule& operator=(const BytecodeModule&) = delete;
	BytecodeModule(BytecodeModule&&) noexcept = default;
	BytecodeModule& operator=(BytecodeModule&&) noexcept = default;
};
