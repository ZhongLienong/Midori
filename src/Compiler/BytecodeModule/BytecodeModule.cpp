#include "BytecodeModule.h"

#include <algorithm>
#include <ranges>

BytecodeModule::ExportedSymbol::ExportedSymbol(std::string name, size_t proc_index, size_t global_index, SymbolType type)
	: m_name(std::move(name)), 
	m_procedure_index(proc_index), 
	m_global_index(global_index), 
	m_type(type)
{
}

BytecodeModule::ExportedSymbol::ExportedSymbol(std::string name, size_t index, SymbolType type)
	: m_name(std::move(name)), 
	m_procedure_index(index), 
	m_global_index(index), 
	m_type(type)
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