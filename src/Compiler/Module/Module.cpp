#include "Module.h"
#include "Compiler/Token/Token.h"

#include <algorithm>

ModuleExport::ModuleExport(std::string_view symbol_name, VisibilityLevel visibility)
	: m_symbol_name(symbol_name),
	m_visibility(visibility)
{
}

ModuleDeclaration::ModuleDeclaration()
	: m_module_name(""),
	m_file_path(""),
	m_has_module_declaration(false)
{
}

ModuleDeclaration::ModuleDeclaration(std::string_view module_name, const std::filesystem::path& file_path)
	: m_module_name(module_name),
	m_file_path(file_path),
	m_has_module_declaration(true)
{
}

bool ModuleDeclaration::HasExport(std::string_view symbol_name) const
{
	return std::ranges::any_of
	(
		m_exports, 
		[&symbol_name](const ModuleExport& exp)
		{
			return exp.m_symbol_name == symbol_name;
		}
	);
}

VisibilityLevel ModuleDeclaration::GetExportVisibility(std::string_view symbol_name) const
{
	std::vector<ModuleExport>::const_iterator it = std::ranges::find_if
	(
		m_exports, 
		[&symbol_name](const ModuleExport& exp)
		{
			return exp.m_symbol_name == symbol_name;
		}
	);

	if (it != m_exports.cend())
	{
		return it->m_visibility;
	}

	return VisibilityLevel::Internal;
}
