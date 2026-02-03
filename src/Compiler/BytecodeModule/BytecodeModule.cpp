#include "BytecodeModule.h"

#include <utility>

BytecodeModule::ExportedSymbol::ExportedSymbol(std::string name, size_t proc_index, size_t global_index, SymbolType type)
	: m_procedure_index(proc_index),
	m_global_index(global_index),
	m_type(type),
	m_name(std::move(name))
{
}

BytecodeModule::ExportedSymbol::ExportedSymbol(std::string name, size_t index, SymbolType type)
	: ExportedSymbol(std::move(name), index, index, type)
{
}

BytecodeModule::ImportedSymbol::ImportedSymbol(std::string name, std::string from_module)
	: m_name(std::move(name)),
	m_from_module(std::move(from_module))
{
}

BytecodeModule::BytecodeModule(std::string module_name, std::filesystem::path source_path)
	: m_module_name(std::move(module_name)),
	m_source_path(std::move(source_path))
{
}

BytecodeModule BytecodeModule::WithProcedure(BytecodeStream procedure) &&
{
	m_procedures.push_back(std::move(procedure));
	return std::move(*this);
}

BytecodeModule BytecodeModule::WithExport(ExportedSymbol export_symbol) &&
{
	m_exports.push_back(std::move(export_symbol));
	return std::move(*this);
}

BytecodeModule BytecodeModule::WithImport(ImportedSymbol import_symbol) &&
{
	m_imports.push_back(std::move(import_symbol));
	return std::move(*this);
}

BytecodeModule BytecodeModule::WithString(std::string str) &&
{
	m_string_pool.push_back(std::move(str));
	return std::move(*this);
}

BytecodeModule BytecodeModule::WithProcedureName(MidoriText name) &&
{
	m_procedure_names.push_back(std::move(name));
	return std::move(*this);
}

BytecodeModule BytecodeModule::WithGlobalVariable(MidoriText variable) &&
{
	m_global_variables.push_back(std::move(variable));
	return std::move(*this);
}

BytecodeModule BytecodeModule::WithGenericFunction(std::string name, GenericFunctionInfo info) &&
{
	m_generic_functions.emplace(std::move(name), std::move(info));
	return std::move(*this);
}
