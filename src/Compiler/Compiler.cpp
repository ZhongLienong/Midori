#include "Common/BuildConfig/BuildConfig.h"
#include "Common/Constant/Constant.h"
#include "Common/Printer/Printer.h"
#include "Compiler.h"
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
#include <future>
#include <mutex>
#include <numeric>
#include <sstream>

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

	m_file_name = std::filesystem::absolute(m_file_name).string();
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
							std::vector<std::vector<std::string>> streams = build_graph.GetCompilationStreams();
							std::unordered_map<std::string, CompiledModule> compiled_modules;
							std::mutex modules_mutex;
							std::mutex print_mutex;
							const size_t total_modules = std::accumulate(streams.begin(), streams.end(), 0uz, [](const size_t sum, const std::vector<std::string>& stream) { return sum + stream.size(); });
							std::atomic<size_t> completed_modules{ 0u };

							if (streams.size() > 1u || (streams.size() == 1u && streams[0u].size() > 1u))
							{
								std::lock_guard<std::mutex> lock(print_mutex);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
								Printer::PrintLabeled<Printer::Color::BRIGHT_CYAN, Printer::Color::WHITE>
								(
									"COMPILING",
									std::format
									(
										"{} module{} in {} stream{}\n",
										total_modules,
										total_modules == 1 ? "" : "s",
										streams.size(),
										streams.size() == 1u ? "" : "s"
									)
								);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
							}

							for (size_t stream_idx = 0u; stream_idx < streams.size(); stream_idx += 1u)
							{
								const std::vector<std::string>& stream = streams[stream_idx];
								std::vector<std::future<std::expected<CompiledModule, std::string>>> futures;
								futures.reserve(stream.size());

								for (const std::string& file_path : stream)
								{
									futures.emplace_back
									(
										std::async
										(
											std::launch::async,
											[&, file_path]() -> std::expected<CompiledModule, std::string>
											{
												size_t current_module;
												{
													std::lock_guard<std::mutex> lock(print_mutex);
													current_module = ++completed_modules;
													std::string short_path = std::filesystem::path(file_path).filename().string();

													// Show stream info for multi-stream builds, just progress for single stream
													if (streams.size() > 1u)
													{
														Printer::PrintLabeled<Printer::Color::BLUE, Printer::Color::WHITE>
														(
															std::format("[{}/{}]", current_module, total_modules),
															std::format("Stream {} -> {}\n", stream_idx + 1, short_path)
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
												CompiledModule::TypeclassMetadataMap imported_typeclass_metadata;
												for (const std::string& dep_path : node.m_dependencies)
												{
													std::lock_guard<std::mutex> lock(modules_mutex);
													const CompiledModule& dep = compiled_modules.at(dep_path);
													imports[dep.m_module_name] = dep.m_symbols;
													for (const auto& [tc_name, metadata] : dep.m_typeclass_metadata)
													{
														imported_typeclass_metadata[tc_name] = metadata;
													}
												}

												const ModuleDeclaration* module_decl = build_graph.m_module_declarations.contains(file_path) ? &build_graph.m_module_declarations.at(file_path) : nullptr;

												Parser parser(std::move(node.m_tokens), file_path, m_source_lines, imports, node.m_use_imports, module_decl, imported_typeclass_metadata);
												std::expected<MidoriProgramTree, std::string> ast = parser.Parse();
												if (!ast.has_value())
												{
													return std::unexpected(ast.error());
												}

												// Extract type signatures from parsed AST (for dependent modules)
												TypeChecker::TypeEnvironment type_signatures = TypeChecker::ExtractTypeSignatures(ast.value());

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
														// Add both unqualified and qualified names for compatibility
														imported_types[name] = type;
														imported_types[dep.m_module_name + NameSeparator.data() + name] = type;
													}
												}

												// Build imported typeclasses for type checking
												std::unordered_map<std::string, TypeChecker::TypeclassInfo> imported_typeclass_infos;
												for (const auto& [typeclass_name, metadata] : imported_typeclass_metadata)
												{
													// Create TypeclassInfo for imported typeclasses with method types
													TypeChecker::TypeclassInfo info(typeclass_name, std::vector<std::string>(metadata.m_type_param_names), std::vector<MidoriType::TypeclassConstraint>{}, std::unordered_map<std::string, std::shared_ptr<MidoriType>>(metadata.m_method_types), std::unordered_set<std::string>{});
													imported_typeclass_infos[typeclass_name] = std::move(info);
												}

												// Type check with imported types
												MidoriResult::TypeCheckerResult type_checked_ast = TypeChecker(std::move(ast.value()), file_path, m_source_lines, imported_types, imported_typeclass_infos).TypeCheck();
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
												MidoriResult::CodeGeneratorResult module_bytecode =CodeGenerator(std::move(optimized_ast.value()), file_path, m_source_lines, module_name, export_set, imported_typeclass_methods, imported_typeclass_instances).GenerateModuleBytecode();
												if (!module_bytecode.has_value())
												{
													return std::unexpected(module_bytecode.error());
												}

												CompiledModule compiled_module(module_name, file_path, std::move(symbols), std::move(type_signatures), std::move(typeclass_metadata));

												compiled_module.m_bytecode = std::move(module_bytecode.value());

												return compiled_module;
											}
									));
								}

								// Wait for all modules in this stream to complete
								for (size_t i = 0u; i < futures.size(); i += 1u)
								{
									MidoriResult::CompiledModuleResult result = futures[i].get();
									if (!result.has_value())
									{
										return std::unexpected<std::string>(result.error());
									}

									// Insert compiled module
									const std::string& file_path = stream[i];
									std::lock_guard<std::mutex> lock(modules_mutex);
									compiled_modules.emplace(file_path, std::move(result).value());
								}
							}

							// Collect all bytecode modules in dependency order
							std::vector<BytecodeModule> all_bytecode_modules;
							all_bytecode_modules.reserve(streams.size());
							for (const std::vector<std::string>& stream : streams)
							{
								for (const std::string& file_path : stream)
								{
									std::unordered_map<std::string, CompiledModule>::iterator it = compiled_modules.find(file_path);
									all_bytecode_modules.emplace_back(std::move(it->second.m_bytecode.value()));
								}
							}

							std::chrono::high_resolution_clock::time_point compile_end = std::chrono::high_resolution_clock::now();
							std::chrono::milliseconds compile_duration = std::chrono::duration_cast<std::chrono::milliseconds>(compile_end - compile_start);

							if (streams.size() > 1u || (streams.size() == 1u && streams[0u].size() > 1u))
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