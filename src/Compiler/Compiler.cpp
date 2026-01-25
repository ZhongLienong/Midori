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

#include <atomic>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <future>
#include <mutex>
#include <numeric>
#include <sstream>

using namespace std::string_literals;

#if MIDORI_ENABLE_AST_DUMP
#include "Utility/AbstractSyntaxTreePrinter/AbstractSyntaxTreePrinter.h"
#endif

#if MIDORI_ENABLE_DISASSEMBLY
#include "Utility/Disassembler/Disassembler.h"
#endif

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

							for (size_t tier_idx = 0u; tier_idx < tiers.size(); tier_idx += 1u)
							{
								const std::vector<std::string>& tier = tiers[tier_idx];

#ifndef __EMSCRIPTEN__
								// Native: Use async compilation for parallel builds
								std::vector<MidoriResult::FutureModuleResult> futures;
								futures.reserve(tier.size());

								for (const std::string& file_path : tier)
								{
									futures.emplace_back
									(
										std::async
										(
											std::launch::async,
											[&print_mutex, &completed_modules, &tiers, &tier_idx, &total_modules, &build_graph, &modules_mutex, &compiled_modules, this, file_path]() -> std::expected<CompiledModule, std::string>
											{
#else
								// WASM: Use synchronous compilation (no thread support)
								for (const std::string& file_path : tier)
								{
									std::function<MidoriResult::CompiledModuleResult()> compile_module = [&print_mutex, &completed_modules, &tiers, &tier_idx, &total_modules, &build_graph, &modules_mutex, &compiled_modules, this, &file_path]() -> std::expected<CompiledModule, std::string>
									{
#endif
												size_t current_module;
												{
													std::lock_guard<std::mutex> lock(print_mutex);
													current_module = ++completed_modules;
													std::string short_path = std::filesystem::path(file_path).filename().string();

													// Show tier info for multi-tier builds, just progress for single tier
													if (tiers.size() > 1u)
													{
														Printer::PrintLabeled<Printer::Color::BLUE, Printer::Color::WHITE>
														(
															std::format("[{}/{}]", current_module, total_modules),
															std::format("Tier {} -> {}\n", tier_idx + 1, short_path)
														);
													}
													else
													{
														Printer::PrintLabeled<Printer::Color::BLUE, Printer::Color::WHITE>
														(
															std::format("[{}/{}]", current_module, total_modules),
															std::format("{}\n", short_path)
														);
													}
												}

												BuildGraph::BuildNode& node = build_graph.m_nodes[file_path];

												// Build imports from already-compiled dependencies
												std::unordered_map<std::string, CompiledModule::SymbolTable> imports;
												std::unordered_map<std::string, TypeChecker::TypeEnvironment> imported_type_sigs;
												CompiledModule::TypeclassMetadataMap imported_typeclass_metadata;
												for (const std::string& dep_path : node.m_dependencies)
												{
													std::lock_guard<std::mutex> lock(modules_mutex);
													const CompiledModule& dep = compiled_modules.at(dep_path);
													imports[dep.m_module_name] = dep.m_symbols;
													imported_type_sigs[dep.m_module_name] = dep.m_type_signatures;
													for (const auto& [tc_name, metadata] : dep.m_typeclass_metadata)
													{
														imported_typeclass_metadata[tc_name] = metadata;
													}
												}

												const ModuleDeclaration* module_decl = build_graph.m_module_declarations.contains(file_path) ? &build_graph.m_module_declarations.at(file_path) : nullptr;
												std::vector<std::string> module_source_lines;
#ifndef __EMSCRIPTEN__
												if (std::filesystem::equivalent(file_path, m_file_name))
#else
												if (file_path == m_file_name)
#endif
												{
													module_source_lines = m_source_lines;
												}
												else
												{
													std::ifstream file(file_path);
													if (file.is_open())
													{
														std::string line;
														while (std::getline(file, line))
														{
															module_source_lines.push_back(line);
														}
													}
												}

												Parser parser(std::move(node.m_tokens), file_path, module_source_lines, imports, imported_type_sigs, node.m_use_imports, module_decl, imported_typeclass_metadata);
												std::expected<MidoriProgramTree, std::string> ast = parser.Parse();
												if (!ast.has_value())
												{
													return std::unexpected(ast.error());
												}

												// Build export set for filtering type signatures
												std::unordered_set<std::string> export_set_for_types;
												if (module_decl)
												{
													for (const ModuleExport& exp : module_decl->m_exports)
													{
														export_set_for_types.insert(exp.m_symbol_name);
													}
												}

												// Extract type signatures from parsed AST (for dependent modules)
												// Only include exported types to prevent internal type leakage
												TypeChecker::TypeEnvironment type_signatures = TypeChecker::ExtractTypeSignatures(ast.value(), module_decl ? &export_set_for_types : nullptr);

												// Extract typeclass metadata from parser (for dependent modules)
												CompiledModule::TypeclassMetadataMap typeclass_metadata = parser.GetTypeclassMetadata();

												// Build combined type environment from all dependencies
												TypeChecker::TypeEnvironment imported_types;
												for (const std::string& dep_path : node.m_dependencies)
												{
													std::lock_guard<std::mutex> lock(modules_mutex);
													const CompiledModule& dep = compiled_modules.at(dep_path);

													// Merge dependency's type signatures with module-qualified names
													for (const auto& [name, type] : dep.m_type_signatures)
													{
														imported_types[name] = type;
														imported_types[dep.m_module_name + NameSeparator.data() + name] = type;
													}
												}

												// Build imported typeclasses for type checking
												std::unordered_map<std::string, TypeChecker::ClassInfo> imported_typeclass_infos;
												for (const auto& [typeclass_name, metadata] : imported_typeclass_metadata)
												{
													TypeChecker::ClassInfo info(typeclass_name, std::vector<std::string>(metadata.m_type_param_names), std::vector<MidoriType::ClassConstraint>{}, TypeChecker::TypeEnvironment(metadata.m_method_types), std::unordered_set<std::string>{});
													imported_typeclass_infos[typeclass_name] = std::move(info);
												}

												// Type check with imported types
												MidoriResult::TypeCheckerResult type_checked_ast = TypeChecker(std::move(ast.value()), file_path, module_source_lines, imported_types, imported_typeclass_infos).TypeCheck();
												if (!type_checked_ast.has_value())
												{
													return std::unexpected(type_checked_ast.error());
												}

												// Optimize the type-checked AST
												MidoriResult::OptimizerResult optimized_ast = OptimizerManager(std::move(type_checked_ast.value())).Optimize();
												if (!optimized_ast.has_value())
												{
													return std::unexpected(optimized_ast.error());
												}

												// Extract exports
												CompiledModule::SymbolTable symbols;
												std::unordered_set<std::string> export_set;
												if (module_decl)
												{
													for (const ModuleExport& exp : module_decl->m_exports)
													{
														symbols.m_exports.insert(exp.m_symbol_name);
														symbols.m_export_visibility[exp.m_symbol_name] = exp.m_visibility;
														export_set.insert(exp.m_symbol_name);

														if (typeclass_metadata.contains(exp.m_symbol_name))
														{
															const CompiledModule::TypeclassMetadata& tc_metadata = typeclass_metadata.at(exp.m_symbol_name);
															for (const std::string& instance_method : tc_metadata.m_instance_methods)
															{
																symbols.m_exports.insert(instance_method);
																symbols.m_export_visibility[instance_method] = exp.m_visibility;
																export_set.insert(instance_method);
															}
														}
													}
												}

												const std::string module_name = module_decl ? module_decl->m_module_name : std::filesystem::path(file_path).stem().string();

												// Generate per-module bytecode
												// Convert metadata to method-only map for CodeGenerator
												CompiledModule::TypeclassMethodMap imported_typeclass_methods;
												std::unordered_map<std::string, std::vector<std::string>> imported_typeclass_instances;
												for (const auto& [tc_name, metadata] : imported_typeclass_metadata)
												{
													imported_typeclass_methods[tc_name] = metadata.m_method_names;
													imported_typeclass_instances[tc_name] = metadata.m_instance_methods;
												}

												// Collect imported generic functions
												std::unordered_map<std::string, GenericFunctionInfo> imported_generic_functions;
												for (const std::string& dep_path : node.m_dependencies)
												{
													std::lock_guard<std::mutex> lock(modules_mutex);
													const CompiledModule& dep = compiled_modules.at(dep_path);
													if (dep.m_bytecode.has_value())
													{
														for (const auto& [name, info] : dep.m_bytecode.value().m_generic_functions)
														{
															imported_generic_functions[name] = info;
															imported_generic_functions[dep.m_module_name + "::" + name] = info;
														}
													}
												}

												MidoriResult::CodeGeneratorResult module_bytecode = CodeGenerator(std::move(optimized_ast.value()), file_path, module_source_lines, module_name, export_set, imported_typeclass_methods, imported_typeclass_instances, imported_generic_functions).GenerateModuleBytecode();
												if (!module_bytecode.has_value())
												{
													return std::unexpected(module_bytecode.error());
												}

												std::unordered_set<std::string> defined_exports;
												for (const BytecodeModule::ExportedSymbol& exported_symbol : module_bytecode.value().m_exports)
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

												CompiledModule compiled_module(module_name, file_path, std::move(symbols), std::move(type_signatures), std::move(typeclass_metadata));

												compiled_module.m_bytecode = std::move(module_bytecode.value());

												return compiled_module;
#ifndef __EMSCRIPTEN__
											}
									));
								}

								// Wait for all modules in this tier to complete
								for (size_t i = 0u; i < futures.size(); i += 1u)
								{
									MidoriResult::CompiledModuleResult result = futures[i].get();
									if (!result.has_value())
									{
										return std::unexpected<std::string>(result.error());
									}

									// Insert compiled module
									const std::string& file_path = tier[i];
									std::lock_guard<std::mutex> lock(modules_mutex);
									compiled_modules.emplace(file_path, std::move(result).value());
								}
#else
									};

									// Execute synchronously and check result immediately
									MidoriResult::CompiledModuleResult result = compile_module();
									if (!result.has_value())
									{
										return std::unexpected<std::string>(result.error());
									}

									// Insert compiled module
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