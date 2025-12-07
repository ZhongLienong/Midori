#include "CompiledModule.h"

CompiledModule::CompiledModule(std::string module_name, std::filesystem::path file_path, SymbolTable symbols, TypeEnvironment type_signatures, TypeclassMetadataMap typeclass_metadata)
	: m_module_name(std::move(module_name)),
	m_file_path(std::move(file_path)),
	m_symbols(std::move(symbols)),
	m_type_signatures(std::move(type_signatures)),
	m_typeclass_metadata(std::move(typeclass_metadata))
{
}

bool CompiledModule::SymbolTable::HasExport(std::string_view name) const
{
	return m_exports.contains(std::string(name));
}

VisibilityLevel CompiledModule::SymbolTable::GetExportVisibility(std::string_view name) const
{
	std::unordered_map<std::string, VisibilityLevel>::const_iterator it = m_export_visibility.find(std::string(name));
	if (it != m_export_visibility.end())
	{
		return it->second;
	}
	return VisibilityLevel::Internal;  // Default to internal if not found
}