#include "BytecodeLinker.h"
#include "Common/Constant/Constant.h"

#include <algorithm>
#include <format>
#include <functional>
#include <numeric>
#include <ranges>

BytecodeLinker::BytecodeLinker(std::vector<BytecodeModule>&& modules, std::string_view entry_module_name)
	: m_modules(std::move(modules)),
	m_entry_module_name(entry_module_name)
{
}

MidoriResult::BytecodeLinkerResult BytecodeLinker::Link()
{
	if (m_modules.empty())
	{
		return std::unexpected("Cannot link: no modules were successfully compiled.\n\nPossible causes:\n  - All source files failed to compile (check for syntax/type errors above)\n  - Circular module dependencies detected\n  - Module resolution failed (check import paths)\n  - Empty build graph (no valid modules to compile)");
	}

	AssignModuleBaseOffsets();

	MidoriResult::VoidResult symbol_result = BuildGlobalSymbolTable();
	if (!symbol_result.has_value())
	{
		return std::unexpected(symbol_result.error());
	}

	MergeConstantPools();
	MergeFunctionNames();
	MergeGlobalVariables();

	MidoriResult::VoidResult import_result = ResolveImportsAndPatch();
	if (!import_result.has_value())
	{
		return std::unexpected(import_result.error());
	}

	ConcatenateBytecode();

	m_global_procedures.insert(m_global_procedures.begin(), BytecodeStream());

	// Create bootstrap name with entry module context for debugging
	std::string bootstrap_name = std::format("{}@{}", MODULE_BOOTSTRAP_PREFIX, m_entry_module_name);
	m_global_procedure_names.insert(m_global_procedure_names.begin(), MidoriText(bootstrap_name.c_str()));

	PatchBootstrapOffsets();

	m_global_procedures[0u] = BuildBootstrapProcedure();

	MidoriExecutable executable;
	executable.AttachProcedures(std::move(m_global_procedures));
	executable.AttachProcedureNames(std::move(m_global_procedure_names));
	executable.AddStringPool(std::move(m_global_string_pool));
	executable.SetFileName(std::string(m_entry_module_name));

	std::ranges::for_each
	(
		m_global_variables,
		[&executable](MidoriText& global_var)
		{
			executable.AddGlobalVariable(std::move(global_var));
		}
	);

	return executable;
}

void BytecodeLinker::AssignModuleBaseOffsets()
{
	using AccumulatorFn = std::function<size_t(size_t, const BytecodeModule&)>;

	AccumulatorFn assign_and_accumulate = [this](size_t current_index, const BytecodeModule& module) -> size_t
		{
			m_module_base_procedure_indices[module.m_module_name] = current_index;
			return current_index + module.m_procedures.size();
		};

	static_cast<void>(std::accumulate(m_modules.begin(), m_modules.end(), 0uz, assign_and_accumulate));
}

MidoriResult::VoidResult BytecodeLinker::BuildGlobalSymbolTable()
{
	using ExportProcessorFn = std::function<MidoriResult::VoidResult(const BytecodeModule&)>;

	ExportProcessorFn process_module_exports = [this](const BytecodeModule& module) -> MidoriResult::VoidResult
		{
			size_t module_base = m_module_base_procedure_indices[module.m_module_name];

			using ExportValidatorFn = std::function<MidoriResult::VoidResult(const BytecodeModule::ExportedSymbol&)>;

			ExportValidatorFn validate_and_register = [this, &module, module_base](const BytecodeModule::ExportedSymbol& exp) -> MidoriResult::VoidResult
				{
					std::string symbol_key = MakeSymbolKey(module.m_module_name, exp.m_name);
					size_t global_procedure_index = module_base + exp.m_procedure_index;

					if (m_global_symbol_to_procedure.contains(symbol_key))
					{
						return std::unexpected(std::format("Duplicate symbol export: {} from module {}", exp.m_name, module.m_module_name));
					}

					m_global_symbol_to_procedure[symbol_key] = global_procedure_index;
					return {};
				};

			std::vector<MidoriResult::VoidResult> results;
			std::ranges::transform(module.m_exports, std::back_inserter(results), validate_and_register);

			std::vector<MidoriResult::VoidResult>::const_iterator error_it = std::ranges::find_if_not
			(
				results,
				[](const MidoriResult::VoidResult& result) { return result.has_value(); }
			);

			return (error_it != results.end()) ? *error_it : MidoriResult::VoidResult{};
		};

	std::vector<MidoriResult::VoidResult> module_results;
	std::ranges::transform(m_modules, std::back_inserter(module_results), process_module_exports);

	std::vector<MidoriResult::VoidResult>::const_iterator error_it = std::ranges::find_if_not
	(
		module_results,
		[](const MidoriResult::VoidResult& result) { return result.has_value(); }
	);

	return (error_it != module_results.end()) ? *error_it : MidoriResult::VoidResult{};
}

void BytecodeLinker::MergeConstantPools()
{
	using StringMapperFn = std::function<size_t(const std::string&)>;

	std::ranges::for_each
	(
		m_modules,
		[this](const BytecodeModule& module)
		{
			StringMapperFn map_string = [this](const std::string& str) -> size_t
				{
					return MergeString(str);
				};

			std::vector<size_t> string_index_mapping;
			std::ranges::transform(module.m_string_pool, std::back_inserter(string_index_mapping), map_string);

			m_module_string_index_mappings[module.m_module_name] = std::move(string_index_mapping);
		}
	);
}

void BytecodeLinker::MergeFunctionNames()
{
	std::ranges::for_each
	(
		m_modules,
		[this](const BytecodeModule& module)
		{
			std::ranges::copy(module.m_procedure_names, std::back_inserter(m_global_procedure_names));
		}
	);
}

void BytecodeLinker::MergeGlobalVariables()
{
	using GlobalAccumulatorFn = std::function<size_t(size_t, const BytecodeModule&)>;

	GlobalAccumulatorFn accumulate_globals = [this](size_t current_index, const BytecodeModule& module) -> size_t
		{
			m_module_base_global_indices[module.m_module_name] = current_index;

			std::ranges::copy(module.m_global_variables, std::back_inserter(m_global_variables));

			return current_index + module.m_global_variables.size();
		};

	static_cast<void>(std::accumulate(m_modules.begin(), m_modules.end(), 0uz, accumulate_globals));
}

MidoriResult::VoidResult BytecodeLinker::ResolveImportsAndPatch()
{
	using ImportValidatorFn = std::function<MidoriResult::VoidResult(const BytecodeModule::ImportedSymbol&)>;

	std::vector<MidoriResult::VoidResult> validation_results;

	std::ranges::for_each
	(
		m_modules,
		[this, &validation_results](const BytecodeModule& module)
		{
			ImportValidatorFn validate_import = [this, &module](const BytecodeModule::ImportedSymbol& import) -> MidoriResult::VoidResult
				{
					std::string symbol_key = MakeSymbolKey(import.m_from_module, import.m_name);
					bool found_as_procedure = m_global_symbol_to_procedure.contains(symbol_key);

					if (found_as_procedure)
					{
						return {};
					}

					std::vector<BytecodeModule>::const_iterator module_it = std::ranges::find_if
					(
						m_modules,
						[&import](const BytecodeModule& m) { return m.m_module_name == import.m_from_module; }
					);

					if (module_it == m_modules.end())
					{
						return std::unexpected(std::format("Unresolved import: {} from module {} (imported by {})",import.m_name, import.m_from_module, module.m_module_name));
					}

					std::vector<MidoriText>::const_iterator global_it = std::ranges::find_if
					(
						module_it->m_global_variables,
						[&import](const MidoriText& global_var) { return global_var.GetCString() == import.m_name; }
					);

					bool found_as_global = (global_it != module_it->m_global_variables.end());

					if (!found_as_global)
					{
						return std::unexpected(std::format("Unresolved import: {} from module {} (imported by {})",import.m_name, import.m_from_module, module.m_module_name));
					}

					return {};
				};

			std::ranges::transform(module.m_imports,std::back_inserter(validation_results),validate_import);
		}
	);

	std::vector<MidoriResult::VoidResult>::const_iterator error_it = std::ranges::find_if_not
	(
		validation_results,
		[](const MidoriResult::VoidResult& result) { return result.has_value(); }
	);

	return (error_it != validation_results.end()) ? *error_it : MidoriResult::VoidResult{};
}

void BytecodeLinker::ConcatenateBytecode()
{
	std::ranges::for_each
	(
		m_modules,
		[this](BytecodeModule& module)
		{
			size_t module_proc_base_offset = m_module_base_procedure_indices[module.m_module_name];
			size_t module_global_base_offset = m_module_base_global_indices[module.m_module_name];

			using ImportResolverFn = std::function<size_t(const BytecodeModule::ImportedSymbol&)>;

			ImportResolverFn resolve_import = [this](const BytecodeModule::ImportedSymbol& import) -> size_t
				{
					std::string symbol_key = MakeSymbolKey(import.m_from_module, import.m_name);

					std::vector<BytecodeModule>::const_iterator module_it = std::ranges::find_if
					(
						m_modules,
						[&import](const BytecodeModule& m) { return m.m_module_name == import.m_from_module; }
					);

					if (module_it == m_modules.end())
					{
						return 0;
					}

					std::optional<size_t> export_result = FindSymbolInExports(*module_it, import.m_name);
					if (export_result.has_value())
					{
						return export_result.value();
					}

					std::optional<size_t> global_result = FindSymbolInGlobals(*module_it, import.m_name, m_module_base_global_indices[module_it->m_module_name]);

					return global_result.value_or(0);
				};

			std::vector<size_t> import_resolved_indices;
			std::ranges::transform(module.m_imports,std::back_inserter(import_resolved_indices),resolve_import);

			const std::vector<size_t>& string_mapping = m_module_string_index_mappings[module.m_module_name];

			using BytecodePatcherFn = std::function<void(BytecodeStream&)>;

			BytecodePatcherFn patch_procedure = [this, &module_proc_base_offset, &string_mapping, &import_resolved_indices, &module_global_base_offset](BytecodeStream& procedure)
				{
					int bytecode_size = procedure.GetByteCodeSize();

					for (int offset = 0; offset < bytecode_size; )
					{
						OpCode opcode = procedure.ReadByteCode(offset);
						int advance = CalculateInstructionSize(opcode, procedure, offset);

						if (opcode == OpCode::ALLOCATE_CLOSURE || opcode == OpCode::ALLOCATE_STATIC_CLOSURE)
						{
							int old_proc_index = static_cast<int>(procedure.ReadByteCode(offset + 1));
							int new_proc_index = old_proc_index + static_cast<int>(module_proc_base_offset);
							procedure.SetByteCode(offset + 1, static_cast<OpCode>(new_proc_index));
						}
						else if (opcode == OpCode::LOAD_STRING)
						{
							int old_string_index = static_cast<int>(procedure.ReadByteCode(offset + 1));
							if (old_string_index >= 0 && static_cast<size_t>(old_string_index) < string_mapping.size())
							{
								size_t new_string_index = string_mapping[old_string_index];
								procedure.SetByteCode(offset + 1, static_cast<OpCode>(new_string_index));
							}
						}
						else if (opcode == OpCode::DEFINE_GLOBAL || opcode == OpCode::GET_GLOBAL || opcode == OpCode::SET_GLOBAL)
						{
							int old_global_index = static_cast<int>(procedure.ReadByteCode(offset + 1));



							if (IsImportIndex(old_global_index))
							{
								size_t import_array_index = ConvertImportIndex(old_global_index);
								if (import_array_index < import_resolved_indices.size())
								{
									size_t resolved_index = import_resolved_indices[import_array_index];
										procedure.SetByteCode(offset + 1, static_cast<OpCode>(resolved_index));
								}
							}
							else
							{
								int new_global_index = old_global_index + static_cast<int>(module_global_base_offset);
								procedure.SetByteCode(offset + 1, static_cast<OpCode>(new_global_index));
							}
						}

						offset += advance;
					}

					m_global_procedures.push_back(std::move(procedure));
				};

			std::ranges::for_each(module.m_procedures, patch_procedure);
		}
	);
}

void BytecodeLinker::PatchBootstrapOffsets()
{
	std::ranges::for_each
	(
		std::views::iota(1u, m_global_procedures.size()),
		[this](size_t proc_idx)
		{
			BytecodeStream& procedure = m_global_procedures[proc_idx];
			int bytecode_size = procedure.GetByteCodeSize();

			for (int offset = 0; offset < bytecode_size; )
			{
				OpCode opcode = procedure.ReadByteCode(offset);
				int advance = CalculateInstructionSize(opcode, procedure, offset);

				if (opcode == OpCode::ALLOCATE_CLOSURE || opcode == OpCode::ALLOCATE_STATIC_CLOSURE)
				{
					int old_proc_index = static_cast<int>(procedure.ReadByteCode(offset + 1));
					int new_proc_index = old_proc_index + 1;
					procedure.SetByteCode(offset + 1, static_cast<OpCode>(new_proc_index));
				}

				offset += advance;
			}
		}
	);
}

BytecodeStream BytecodeLinker::BuildBootstrapProcedure()
{
	using BootstrapBuilderFn = std::function<void(BytecodeStream&, const BytecodeModule&)>;

	BootstrapBuilderFn add_module_initializer = [this](BytecodeStream& bootstrap, const BytecodeModule& module)
		{
			std::unordered_map<std::string, size_t>::const_iterator base_it = m_module_base_procedure_indices.find(module.m_module_name);

			size_t module_base_index = base_it->second;
			size_t module_global_index = module_base_index + 1u;

			bootstrap.AddByteCode(OpCode::ALLOCATE_CLOSURE, 0);
			bootstrap.AddByteCode(static_cast<OpCode>(module_global_index), 0);
			bootstrap.AddByteCode(OpCode::CONSTRUCT_CLOSURE, 0);
			bootstrap.AddByteCode(static_cast<OpCode>(0), 0);
			bootstrap.AddByteCode(OpCode::CALL_DEFINED, 0);
			bootstrap.AddByteCode(static_cast<OpCode>(0), 0);
			bootstrap.AddByteCode(OpCode::POP, 0);
		};

	BytecodeStream bootstrap;

	std::ranges::for_each
	(
		m_modules,
		[&bootstrap, &add_module_initializer](const BytecodeModule& module)
		{
			add_module_initializer(bootstrap, module);
		}
	);

	bootstrap.AddByteCode(OpCode::HALT, 0);

	return bootstrap;
}

const BytecodeModule* BytecodeLinker::FindModule(const std::string& module_name) const
{
	std::vector<BytecodeModule>::const_iterator it = std::ranges::find_if
	(
		m_modules,
		[&module_name](const BytecodeModule& module) { return module.m_module_name == module_name; }
	);

	return (it != m_modules.end()) ? &(*it) : nullptr;
}

std::string BytecodeLinker::MakeSymbolKey(const std::string& module_name, const std::string& symbol_name) const
{
	return module_name + std::string(NameSeparator) + symbol_name;
}

bool BytecodeLinker::IsImportIndex(int global_index) const
{
	return global_index >= 248;
}

size_t BytecodeLinker::ConvertImportIndex(int global_index) const
{
	int import_index = global_index - 256;
	return static_cast<size_t>(-import_index - 1);
}

size_t BytecodeLinker::MergeString(const std::string& str)
{
	std::vector<std::string>::const_iterator it = std::ranges::find(m_global_string_pool, str);

	if (it != m_global_string_pool.end())
	{
		return std::distance(m_global_string_pool.cbegin(), it);
	}

	size_t new_index = m_global_string_pool.size();
	m_global_string_pool.push_back(str);
	return new_index;
}

std::optional<size_t> BytecodeLinker::FindSymbolInExports(const BytecodeModule& module, const std::string& symbol_name) const
{
	std::vector<BytecodeModule::ExportedSymbol>::const_iterator it = std::ranges::find_if
	(
		module.m_exports,
		[&symbol_name](const BytecodeModule::ExportedSymbol& exp) { return exp.m_name == symbol_name; }
	);

	if (it == module.m_exports.end())
	{
		return std::nullopt;
	}

	std::unordered_map<std::string, size_t>::const_iterator base_it = m_module_base_global_indices.find(module.m_module_name);
	size_t base_offset = base_it->second;

	return base_offset + it->m_global_index;
}

std::optional<size_t> BytecodeLinker::FindSymbolInGlobals(const BytecodeModule& module, const std::string& symbol_name, size_t base_global_offset) const
{
	std::vector<MidoriText>::const_iterator it = std::ranges::find_if
	(
		module.m_global_variables,
		[&symbol_name](const MidoriText& global_var) { return global_var.GetCString() == symbol_name; }
	);

	if (it == module.m_global_variables.end())
	{
		return std::nullopt;
	}

	size_t local_index = std::distance(module.m_global_variables.cbegin(), it);
	return base_global_offset + local_index;
}

int BytecodeLinker::CalculateInstructionSize(OpCode opcode, const BytecodeStream& procedure, int offset) const
{
	if (opcode == OpCode::INTEGER_CONSTANT || opcode == OpCode::FLOAT_CONSTANT || opcode == OpCode::WORD_CONSTANT)
	{
		return 9;
	}
	else if (opcode == OpCode::BYTE_CONSTANT)
	{
		return 2;
	}
	else if (opcode == OpCode::CREATE_ARRAY)
	{
		return 4;
	}
	else if
	(
		opcode == OpCode::JUMP ||
		opcode == OpCode::JUMP_IF_FALSE ||
		opcode == OpCode::JUMP_IF_TRUE ||
		opcode == OpCode::JUMP_BACK ||
		opcode == OpCode::BREAK ||
		opcode == OpCode::IF_INTEGER_EQUAL ||
		opcode == OpCode::IF_INTEGER_NOT_EQUAL ||
		opcode == OpCode::IF_INTEGER_GREATER ||
		opcode == OpCode::IF_INTEGER_GREATER_EQUAL ||
		opcode == OpCode::IF_INTEGER_LESS ||
		opcode == OpCode::IF_INTEGER_LESS_EQUAL ||
		opcode == OpCode::IF_FLOAT_EQUAL ||
		opcode == OpCode::IF_FLOAT_NOT_EQUAL ||
		opcode == OpCode::IF_FLOAT_GREATER ||
		opcode == OpCode::IF_FLOAT_GREATER_EQUAL ||
		opcode == OpCode::IF_FLOAT_LESS ||
		opcode == OpCode::IF_FLOAT_LESS_EQUAL
	)
	{
		return 3;
	}
	else if (opcode == OpCode::MATCH_JUMP_TABLE)
	{
		int case_count = static_cast<int>(procedure.ReadByteCode(offset + 1));
		return 2 + (case_count * 2);
	}
	else if (opcode == OpCode::CALL_FOREIGN)
	{
		return 3;
	}
	else if (opcode == OpCode::CALL_FOREIGN_INDEXED)
	{
		return 4;  // opcode + ffi_index + arity + return_type
	}
	else if
	(
		opcode == OpCode::ALLOCATE_CLOSURE ||
		opcode == OpCode::ALLOCATE_STATIC_CLOSURE ||
		opcode == OpCode::LOAD_STRING ||
		opcode == OpCode::DEFINE_GLOBAL ||
		opcode == OpCode::GET_GLOBAL ||
		opcode == OpCode::SET_GLOBAL ||
		opcode == OpCode::GET_LOCAL ||
		opcode == OpCode::SET_LOCAL ||
		opcode == OpCode::GET_CELL ||
		opcode == OpCode::SET_CELL ||
		opcode == OpCode::CALL_DEFINED ||
		opcode == OpCode::CONSTRUCT_CLOSURE ||
		opcode == OpCode::GET_MEMBER ||
		opcode == OpCode::SET_MEMBER ||
		opcode == OpCode::POP_VALUES ||
		opcode == OpCode::POP_LOCAL_SCOPE ||
		opcode == OpCode::POP_BLOCK_SCOPE ||
		opcode == OpCode::POP_MATCH_SCOPE ||
		opcode == OpCode::TAIL_CALL ||
		opcode == OpCode::GET_ARRAY ||
		opcode == OpCode::SET_ARRAY ||
		opcode == OpCode::CONSTRUCT_STRUCT ||
		opcode == OpCode::CONSTRUCT_UNION ||
		opcode == OpCode::SET_TAG
	)
	{
		return 2;
	}
	else
	{
		return 1;
	}
}
