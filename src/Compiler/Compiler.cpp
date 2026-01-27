#include "Common/BuildConfig/BuildConfig.h"
#include "Common/Constant/Constant.h"
#include "Common/Printer/Printer.h"
#include "Compiler.h"
#include "Compiler/BuildGraph/BuildGraph.h"
#include "Compiler/BytecodeLinker/BytecodeLinker.h"
#include "Compiler/CodeGenerator/CodeGenerator.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/Module/CompiledModule.h"
#include "Compiler/ModuleManager/ModuleManager.h"
#include "Compiler/OptimizerManager/OptimizerManager.h"
#include "Compiler/Parser/Parser.h"
#include "Compiler/TypeChecker/TypeChecker.h"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <deque>
#include <filesystem>
#include <fstream>
#include <future>
#include <mutex>
#include <numeric>
#include <sstream>
#include <thread>

using namespace std::string_literals;

#if MIDORI_ENABLE_AST_DUMP
#include "Utility/AbstractSyntaxTreePrinter/AbstractSyntaxTreePrinter.h"
#endif

#if MIDORI_ENABLE_DISASSEMBLY
#include "Utility/Disassembler/Disassembler.h"
#endif

namespace
{
	struct ImportContext
	{
		std::unordered_map<std::string, CompiledModule::SymbolTable> m_imported_symbols;
		std::unordered_map<std::string, TypeChecker::TypeEnvironment> m_imported_type_signatures;
		CompiledModule::TypeclassMetadataMap m_imported_typeclass_metadata;
		TypeChecker::TypeEnvironment m_imported_types;
		std::unordered_map<std::string, TypeChecker::ClassInfo> m_imported_typeclass_infos;
		CompiledModule::TypeclassMethodMap m_imported_typeclass_methods;
		std::unordered_map<std::string, std::vector<std::string>> m_imported_typeclass_instances;
		std::unordered_map<std::string, GenericFunctionInfo> m_imported_generic_functions;
	};

	struct ParsedModule
	{
		MidoriProgramTree m_ast;
		CompiledModule::TypeclassMetadataMap m_typeclass_metadata;
		TypeChecker::TypeEnvironment m_type_signatures;
	};

	struct ModuleExportInfo
	{
		CompiledModule::SymbolTable m_symbols;
		std::unordered_set<std::string> m_export_set;
	};

	struct CompileEnv
	{
		const std::string& m_main_file_name;
		const std::vector<std::string>& m_main_source_lines;
		BuildGraph& m_build_graph;
		std::unordered_map<std::string, CompiledModule>& m_compiled_modules;
		std::mutex& m_modules_mutex;
		std::mutex& m_print_mutex;
		std::atomic<size_t>& m_completed_modules;
		const std::vector<std::vector<std::string>>& m_tiers;
		size_t m_total_modules;
	};

	struct CompileState
	{
		CompileEnv* m_env = nullptr;
		std::string m_file_path;
		size_t m_tier_idx = 0u;
		BuildGraph::BuildNode* m_node = nullptr;
		const ModuleDeclaration* m_module_decl = nullptr;
		ImportContext m_import_context;
		std::vector<std::string> m_source_lines;
		ParsedModule m_parsed_module;
		MidoriProgramTree m_ast;
		ModuleExportInfo m_export_info;
		std::string m_module_name;
		BytecodeModule m_bytecode;
	};

	template <typename T, void (*Apply)(CompileState&, T&&)>
	struct StateApplier
	{
		CompileState m_state;

		MidoriResult::Result<CompileState> operator()(T&& value)
		{
			Apply(m_state, std::move(value));
			return std::move(m_state);
		}
	};

	static void ApplyImportContext(CompileState& state, ImportContext&& import_context)
	{
		state.m_import_context = std::move(import_context);
	}

	static void ApplySourceLines(CompileState& state, std::vector<std::string>&& source_lines)
	{
		state.m_source_lines = std::move(source_lines);
	}

	static void ApplyParsedModule(CompileState& state, ParsedModule&& parsed_module)
	{
		state.m_parsed_module = std::move(parsed_module);
		state.m_ast = std::move(state.m_parsed_module.m_ast);
	}

	static void ApplyAst(CompileState& state, MidoriProgramTree&& ast)
	{
		state.m_ast = std::move(ast);
	}

	static void ApplyBytecode(CompileState& state, BytecodeModule&& bytecode)
	{
		state.m_bytecode = std::move(bytecode);
	}

	static void ReportCompiled(CompileEnv& env, const std::string& file_path, size_t tier_idx)
	{
		size_t current_module;
		{
			std::lock_guard<std::mutex> lock(env.m_print_mutex);
			current_module = ++env.m_completed_modules;
			std::string short_path = std::filesystem::path(file_path).filename().string();

			// Show tier info for multi-tier builds, just progress for single tier
			if (env.m_tiers.size() > 1u)
			{
				Printer::PrintLabeled<Printer::Color::BLUE, Printer::Color::WHITE>
				(
					std::format("{}/{}", current_module, env.m_total_modules),
					std::format("Tier {} -> {}\n", tier_idx + 1, short_path)
				);
			}
			else
			{
				Printer::PrintLabeled<Printer::Color::BLUE, Printer::Color::WHITE>
				(
					std::format("{}/{}", current_module, env.m_total_modules),
					std::format("{}\n", short_path)
				);
			}
		}
	}

	static bool IsMainFile(const CompileEnv& env, const std::string& file_path)
	{
#ifndef __EMSCRIPTEN__
		return std::filesystem::equivalent(file_path, env.m_main_file_name);
#else
		return file_path == env.m_main_file_name;
#endif
	}

	static MidoriResult::Result<std::vector<std::string>> LoadModuleSourceLines(const CompileEnv& env, const std::string& file_path)
	{
		if (IsMainFile(env, file_path))
		{
			return env.m_main_source_lines;
		}

		std::ifstream file(file_path);
		if (!file.is_open())
		{
			return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Could not open module file: "s + file_path, 0, file_path));
		}

		std::vector<std::string> module_source_lines;
		std::string line;
		while (std::getline(file, line))
		{
			module_source_lines.push_back(line);
		}

		return module_source_lines;
	}

	static MidoriResult::Result<ImportContext> BuildImportContext(CompileEnv& env, const BuildGraph::BuildNode& node, const std::string& file_path)
	{
		ImportContext context;
		std::unordered_map<std::string, std::string> imported_typeclass_sources;

		for (const std::string& dep_path : node.m_dependencies)
		{
			std::lock_guard<std::mutex> lock(env.m_modules_mutex);
			const CompiledModule& dep = env.m_compiled_modules.at(dep_path);
			context.m_imported_symbols[dep.m_module_name] = dep.m_symbols;
			context.m_imported_type_signatures[dep.m_module_name] = dep.m_type_signatures;

			for (const auto& [tc_name, metadata] : dep.m_typeclass_metadata)
			{
				if (context.m_imported_typeclass_metadata.contains(tc_name))
				{
					return std::unexpected(MidoriError::GenerateModuleErrorWithContext(std::format("Typeclass '{}' is defined in multiple imported modules ('{}' and '{}')", tc_name, imported_typeclass_sources.at(tc_name), dep.m_module_name), 0, file_path));
				}

				context.m_imported_typeclass_metadata[tc_name] = metadata;
				imported_typeclass_sources[tc_name] = dep.m_module_name;
			}

			for (const auto& [name, type] : dep.m_type_signatures)
			{
				context.m_imported_types[name] = type;
				context.m_imported_types[dep.m_module_name + NameSeparator.data() + name] = type;
			}

			if (dep.m_bytecode.has_value())
			{
				for (const auto& [name, info] : dep.m_bytecode.value().m_generic_functions)
				{
					context.m_imported_generic_functions[name] = info;
					context.m_imported_generic_functions[dep.m_module_name + "::" + name] = info;
				}
			}
		}

		for (const auto& [typeclass_name, metadata] : context.m_imported_typeclass_metadata)
		{
			TypeChecker::ClassInfo info(typeclass_name, std::vector<std::string>(metadata.m_type_param_names), std::vector<MidoriType::ClassConstraint>{}, TypeChecker::TypeEnvironment(metadata.m_method_types), std::unordered_set<std::string>{});
			context.m_imported_typeclass_infos[typeclass_name] = std::move(info);
			context.m_imported_typeclass_methods[typeclass_name] = metadata.m_method_names;
			context.m_imported_typeclass_instances[typeclass_name] = metadata.m_instance_methods;
		}

		return context;
	}

	static MidoriResult::Result<ParsedModule> ParseModule(TokenStream&& tokens, const std::string& file_path, const std::vector<std::string>& module_source_lines, const ImportContext& import_context, const std::vector<UseImport>& use_imports, const ModuleDeclaration* module_decl)
	{
		Parser parser(std::move(tokens), file_path, module_source_lines, import_context.m_imported_symbols, import_context.m_imported_type_signatures, use_imports, module_decl, import_context.m_imported_typeclass_metadata);
		std::expected<MidoriProgramTree, std::string> ast = parser.Parse();
		if (!ast.has_value())
		{
			return std::unexpected(ast.error());
		}

		std::unordered_set<std::string> export_set_for_types;
		if (module_decl)
		{
			for (const ModuleExport& exp : module_decl->m_exports)
			{
				export_set_for_types.insert(exp.m_symbol_name);
			}
		}

		TypeChecker::TypeEnvironment type_signatures = TypeChecker::ExtractTypeSignatures(ast.value(), module_decl ? &export_set_for_types : nullptr);

		return ParsedModule
		{
			std::move(*ast),
			parser.GetTypeclassMetadata(),
			std::move(type_signatures)
		};
	}

	static MidoriResult::TypeCheckerResult TypeCheckModule(MidoriProgramTree&& ast, const std::string& file_path, const std::vector<std::string>& module_source_lines, const ImportContext& import_context)
	{
		return TypeChecker(std::move(ast), file_path, module_source_lines, import_context.m_imported_types, import_context.m_imported_typeclass_infos).TypeCheck();
	}

	static MidoriResult::OptimizerResult OptimizeModule(MidoriProgramTree&& ast)
	{
		return OptimizerManager(std::move(ast)).Optimize();
	}

	static ModuleExportInfo BuildModuleExports(const ModuleDeclaration* module_decl, const CompiledModule::TypeclassMetadataMap& typeclass_metadata)
	{
		ModuleExportInfo export_info;
		if (module_decl)
		{
			for (const ModuleExport& exp : module_decl->m_exports)
			{
				export_info.m_symbols.m_exports.insert(exp.m_symbol_name);
				export_info.m_symbols.m_export_visibility[exp.m_symbol_name] = exp.m_visibility;
				export_info.m_export_set.insert(exp.m_symbol_name);

				if (typeclass_metadata.contains(exp.m_symbol_name))
				{
					const CompiledModule::TypeclassMetadata& tc_metadata = typeclass_metadata.at(exp.m_symbol_name);
					for (const std::string& instance_method : tc_metadata.m_instance_methods)
					{
						export_info.m_symbols.m_exports.insert(instance_method);
						export_info.m_symbols.m_export_visibility[instance_method] = exp.m_visibility;
						export_info.m_export_set.insert(instance_method);
					}
				}
			}
		}

		return export_info;
	}

	static MidoriResult::CodeGeneratorResult GenerateModuleBytecode(MidoriProgramTree&& optimized_ast, const std::string& file_path, const std::vector<std::string>& module_source_lines, const std::string& module_name, const std::unordered_set<std::string>& export_set, const ImportContext& import_context)
	{
		return CodeGenerator(std::move(optimized_ast), file_path, module_source_lines, module_name, export_set, import_context.m_imported_typeclass_methods, import_context.m_imported_typeclass_instances, import_context.m_imported_generic_functions).GenerateModuleBytecode();
	}

	static MidoriResult::VoidResult ValidateExports(const std::unordered_set<std::string>& export_set, const BytecodeModule& module_bytecode, const CompiledModule::TypeclassMetadataMap& typeclass_metadata, const TypeChecker::TypeEnvironment& type_signatures, const std::string& module_name, const std::string& file_path)
	{
		std::unordered_set<std::string> defined_exports;
		for (const BytecodeModule::ExportedSymbol& exported_symbol : module_bytecode.m_exports)
		{
			defined_exports.insert(exported_symbol.m_name);
		}

		for (const auto& [typeclass_name, tc_metadata] : typeclass_metadata)
		{
			defined_exports.insert(typeclass_name);
			for (const std::string& instance_method : tc_metadata.m_instance_methods)
			{
				defined_exports.insert(instance_method);
			}
		}

		// Include type signatures (structs, unions, type aliases) as valid exports
		for (const auto& [type_name, type_ptr] : type_signatures)
		{
			defined_exports.insert(type_name);
		}

		for (const std::string& exported_name : export_set)
		{
			if (!defined_exports.contains(exported_name))
			{
				return std::unexpected(MidoriError::GenerateModuleErrorWithContext("Symbol '"s + exported_name + "' is exported but not defined in module '"s + module_name + "'", 0, file_path));
			}
		}

		return {};
	}

	static MidoriResult::Result<CompileState> MakeCompileState(CompileEnv& env, const std::string& file_path, size_t tier_idx)
	{
		BuildGraph::BuildNode& node = env.m_build_graph.m_nodes.at(file_path);
		const ModuleDeclaration* module_decl = env.m_build_graph.m_module_declarations.contains(file_path) ? &env.m_build_graph.m_module_declarations.at(file_path) : nullptr;

		CompileState state;
		state.m_env = &env;
		state.m_file_path = file_path;
		state.m_tier_idx = tier_idx;
		state.m_node = &node;
		state.m_module_decl = module_decl;

		return state;
	}

	static MidoriResult::Result<CompileState> WithImportContext(CompileState state)
	{
		return BuildImportContext(*state.m_env, *state.m_node, state.m_file_path)
			.and_then(StateApplier<ImportContext, ApplyImportContext>{ std::move(state) });
	}

	static MidoriResult::Result<CompileState> WithSourceLines(CompileState state)
	{
		return LoadModuleSourceLines(*state.m_env, state.m_file_path)
			.and_then(StateApplier<std::vector<std::string>, ApplySourceLines>{ std::move(state) });
	}

	static MidoriResult::Result<CompileState> WithParsedModule(CompileState state)
	{
		return ParseModule(std::move(state.m_node->m_tokens), state.m_file_path, state.m_source_lines, state.m_import_context, state.m_node->m_use_imports, state.m_module_decl)
			.and_then(StateApplier<ParsedModule, ApplyParsedModule>{ std::move(state) });
	}

	static MidoriResult::Result<CompileState> WithTypeCheckedAst(CompileState state)
	{
		return TypeCheckModule(std::move(state.m_ast), state.m_file_path, state.m_source_lines, state.m_import_context)
			.and_then(StateApplier<MidoriProgramTree, ApplyAst>{ std::move(state) });
	}

	static MidoriResult::Result<CompileState> WithOptimizedAst(CompileState state)
	{
		return OptimizeModule(std::move(state.m_ast))
			.and_then(StateApplier<MidoriProgramTree, ApplyAst>{ std::move(state) });
	}

	static MidoriResult::Result<CompileState> WithBytecode(CompileState state)
	{
		state.m_export_info = BuildModuleExports(state.m_module_decl, state.m_parsed_module.m_typeclass_metadata);
		state.m_module_name = state.m_module_decl ? state.m_module_decl->m_module_name : std::filesystem::path(state.m_file_path).stem().string();

		return GenerateModuleBytecode(std::move(state.m_ast), state.m_file_path, state.m_source_lines, state.m_module_name, state.m_export_info.m_export_set, state.m_import_context)
			.and_then(StateApplier<BytecodeModule, ApplyBytecode>{ std::move(state) });
	}

	struct CompiledModuleBuilder
	{
		CompileState m_state;

		MidoriResult::CompiledModuleResult operator()()
		{
			CompiledModule compiled_module(m_state.m_module_name, m_state.m_file_path, std::move(m_state.m_export_info.m_symbols), std::move(m_state.m_parsed_module.m_type_signatures), std::move(m_state.m_parsed_module.m_typeclass_metadata));
			compiled_module.m_bytecode = std::move(m_state.m_bytecode);

			ReportCompiled(*m_state.m_env, m_state.m_file_path, m_state.m_tier_idx);

			return compiled_module;
		}
	};

	static MidoriResult::CompiledModuleResult FinalizeModule(CompileState state)
	{
		return ValidateExports(state.m_export_info.m_export_set, state.m_bytecode, state.m_parsed_module.m_typeclass_metadata, state.m_parsed_module.m_type_signatures, state.m_module_name, state.m_file_path)
			.and_then(CompiledModuleBuilder{ std::move(state) });
	}

	static MidoriResult::CompiledModuleResult PropagateCompileError(const std::string& error)
	{
		return std::unexpected(error);
	}

	static MidoriResult::CompiledModuleResult CompileModule(CompileEnv& env, const std::string& file_path, size_t tier_idx)
	{
		return MakeCompileState(env, file_path, tier_idx)
			.and_then(WithImportContext)
			.and_then(WithSourceLines)
			.and_then(WithParsedModule)
			.and_then(WithTypeCheckedAst)
			.and_then(WithOptimizedAst)
			.and_then(WithBytecode)
			.and_then(FinalizeModule)
			.or_else(PropagateCompileError);
	}
}

Compiler::Compiler(std::string&& source_code, std::string&& file_name)
	: m_source_code(std::move(source_code)), 
	m_file_name(std::move(file_name))
{
	std::istringstream stream(m_source_code);
	std::string line;
	while (std::getline(stream, line))
	{
		m_source_lines.push_back(line);
	}

#ifndef __EMSCRIPTEN__
	m_file_name = std::filesystem::absolute(m_file_name).string();
#else
	if (!m_file_name.empty() && m_file_name[0u] != '/')
	{
		m_file_name = "/" + m_file_name;
	}
#endif
}

MidoriResult::CompilerResult Compiler::Compile()
{
	return Lexer(std::move(m_source_code), m_file_name)
		.Lex()
		.and_then
		(
			[this](TokenStream&& lexer_result) -> MidoriResult::CompilerResult
			{
				return ModuleManager(std::move(lexer_result), m_file_name)
					.GenerateBuildGraph()
					.and_then
					(
						[this](BuildGraph&& build_graph) -> MidoriResult::CompilerResult
						{
							std::chrono::high_resolution_clock::time_point compile_start = std::chrono::high_resolution_clock::now();
							std::vector<std::vector<std::string>> tiers = build_graph.GetCompilationTiers();
							std::unordered_map<std::string, CompiledModule> compiled_modules;
							std::mutex modules_mutex;
							std::mutex print_mutex;
							const size_t total_modules = std::accumulate(tiers.begin(), tiers.end(), 0uz, [](const size_t sum, const std::vector<std::string>& tier) { return sum + tier.size(); });
							std::atomic<size_t> completed_modules{ 0u };

							if (tiers.size() > 1u || (tiers.size() == 1u && tiers[0u].size() > 1u))
							{
								std::lock_guard<std::mutex> lock(print_mutex);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
								Printer::PrintLabeled<Printer::Color::BRIGHT_CYAN, Printer::Color::WHITE>
								(
									"COMPILING",
									std::format
									(
										"{} module{} in {} tier{}\n",
										total_modules,
										total_modules == 1 ? "" : "s",
										tiers.size(),
										tiers.size() == 1u ? "" : "s"
									)
								);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
							}

							CompileEnv env{ m_file_name, m_source_lines, build_graph, compiled_modules, modules_mutex, print_mutex, completed_modules, tiers, total_modules };

							for (size_t tier_idx = 0u; tier_idx < tiers.size(); tier_idx += 1u)
							{
								const std::vector<std::string>& tier = tiers[tier_idx];

#ifndef __EMSCRIPTEN__
								// Native: Use async compilation for parallel builds
								struct PendingModule
								{
									std::string m_file_path;
									MidoriResult::FutureModuleResult m_future;
								};

								const size_t max_parallel = std::max<size_t>
								(
									1u,
									std::min(tier.size(), static_cast<size_t>(std::max(1u, std::thread::hardware_concurrency())))
								);
								std::deque<PendingModule> pending;

								for (const std::string& file_path : tier)
								{
									pending.emplace_back
									(
										PendingModule
										{
											file_path,
											std::async
											(
												std::launch::async,
												[&, file_path, tier_idx]() -> MidoriResult::CompiledModuleResult
												{
													return CompileModule(env, file_path, tier_idx);
												}
											)
										}
									);

									if (pending.size() >= max_parallel)
									{
										PendingModule pending_module = std::move(pending.front());
										pending.pop_front();

										MidoriResult::CompiledModuleResult result = pending_module.m_future.get();
										if (!result.has_value())
										{
											return std::unexpected<std::string>(result.error());
										}

										std::lock_guard<std::mutex> lock(modules_mutex);
										compiled_modules.emplace(pending_module.m_file_path, std::move(result).value());
									}
								}

								// Wait for all modules in this tier to complete
								for (PendingModule& pending_module : pending)
								{
									MidoriResult::CompiledModuleResult result = pending_module.m_future.get();
									if (!result.has_value())
									{
										return std::unexpected<std::string>(result.error());
									}

									std::lock_guard<std::mutex> lock(modules_mutex);
									compiled_modules.emplace(pending_module.m_file_path, std::move(result).value());
								}
#else
								// WASM: Use synchronous compilation (no thread support)
								for (const std::string& file_path : tier)
								{
								MidoriResult::CompiledModuleResult result = CompileModule(env, file_path, tier_idx);
									if (!result.has_value())
									{
										return std::unexpected<std::string>(result.error());
									}

									compiled_modules.emplace(file_path, std::move(result).value());
								}
#endif
							}

							// Collect all bytecode modules in dependency order
							std::vector<BytecodeModule> all_bytecode_modules;
							all_bytecode_modules.reserve(tiers.size());
							for (const std::vector<std::string>& tier : tiers)
							{
								for (const std::string& file_path : tier)
								{
									std::unordered_map<std::string, CompiledModule>::iterator it = compiled_modules.find(file_path);
									all_bytecode_modules.emplace_back(std::move(it->second.m_bytecode.value()));
								}
							}

							std::chrono::high_resolution_clock::time_point compile_end = std::chrono::high_resolution_clock::now();
							std::chrono::milliseconds compile_duration = std::chrono::duration_cast<std::chrono::milliseconds>(compile_end - compile_start);

							if (tiers.size() > 1u || (tiers.size() == 1u && tiers[0u].size() > 1u))
							{
								std::lock_guard<std::mutex> lock(print_mutex);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
								Printer::PrintLabeled<Printer::Color::BRIGHT_GREEN, Printer::Color::WHITE>
								(
									"SUCCESS",
									std::format("Compiled {} module{} in {} ms\n", total_modules, total_modules == 1 ? "" : "s", compile_duration.count())
								);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
							}

							// Link all module bytecode into final executable
							const std::string entry_module_name = std::filesystem::path(m_file_name).stem().string();
							return BytecodeLinker(std::move(all_bytecode_modules), entry_module_name)
								.Link()
								.and_then
								(
									[](MidoriExecutable&& linked_executable) -> MidoriResult::CompilerResult
									{
#if MIDORI_ENABLE_DISASSEMBLY
										for (size_t i : std::views::iota(0u, linked_executable.m_procedure_names.size()))
										{
											MidoriText variable_name = linked_executable.m_procedure_names[i];
											Disassembler::DisassembleBytecodeStream(linked_executable, static_cast<int>(i), variable_name.GetCString());
										}
#endif
										return linked_executable;
									}
								);
						}
					);
			}
		);
}
