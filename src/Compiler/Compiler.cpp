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
#include <array>
#include <atomic>
#include <chrono>
#include <deque>
#include <filesystem>
#include <fstream>
#include <future>
#include <mutex>
#include <set>
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
#if MIDORI_ENABLE_OPTIMIZER_STATS
		OptimizerLog m_optimizer_log;
#endif
	};

	struct CompilationSchedule
	{
		std::vector<std::vector<std::string>> m_tiers;
		std::unordered_map<std::string, size_t> m_tier_indices;
		std::unordered_map<std::string, size_t> m_remaining_deps;
		std::unordered_map<std::string, std::vector<std::string>> m_dependents;
		std::vector<std::string> m_all_modules;
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

	static CompilationSchedule BuildCompilationSchedule(const BuildGraph& build_graph)
	{
		CompilationSchedule schedule;
		schedule.m_tiers = build_graph.GetCompilationTiers();
		schedule.m_all_modules.reserve(build_graph.m_nodes.size());

		for (const auto& [file_path, _] : build_graph.m_nodes)
		{
			schedule.m_all_modules.push_back(file_path);
		}

		std::sort(schedule.m_all_modules.begin(), schedule.m_all_modules.end());

		schedule.m_tier_indices.reserve(schedule.m_all_modules.size());
		for (size_t tier_idx = 0u; tier_idx < schedule.m_tiers.size(); tier_idx += 1u)
		{
			for (const std::string& file_path : schedule.m_tiers[tier_idx])
			{
				schedule.m_tier_indices[file_path] = tier_idx;
			}
		}

		schedule.m_remaining_deps.reserve(schedule.m_all_modules.size());
		schedule.m_dependents.reserve(schedule.m_all_modules.size());
		for (const std::string& file_path : schedule.m_all_modules)
		{
			schedule.m_remaining_deps.emplace(file_path, 0u);
			schedule.m_dependents.emplace(file_path, std::vector<std::string>{});
		}

		for (const std::string& file_path : schedule.m_all_modules)
		{
			const BuildGraph::BuildNode& node = build_graph.m_nodes.at(file_path);
			for (const std::string& dependency : node.m_dependencies)
			{
				if (!build_graph.m_nodes.contains(dependency))
				{
					continue;
				}

				schedule.m_dependents[dependency].push_back(file_path);
				schedule.m_remaining_deps[file_path] += 1u;
			}
		}

		for (auto& [_, dependents] : schedule.m_dependents)
		{
			std::sort(dependents.begin(), dependents.end());
		}

		return schedule;
	}

#if MIDORI_ENABLE_OPTIMIZER_STATS
	static void ReportCompiled(CompileEnv& env, const std::string& file_path, size_t tier_idx, const OptimizerLog* optimizer_log)
#else
	static void ReportCompiled(CompileEnv& env, const std::string& file_path, size_t tier_idx)
#endif
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

#if MIDORI_ENABLE_OPTIMIZER_STATS
			if (optimizer_log && optimizer_log->m_enabled)
			{
				Printer::Print<Printer::Color::CYAN>("\n=== Optimization Pass ===\n");
				if (!optimizer_log->m_body.empty())
				{
					Printer::Print<Printer::Color::MAGENTA>(optimizer_log->m_body);
				}
				Printer::Print<Printer::Color::CYAN>("=========================\n\n");
			}
#endif
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

		context.m_imported_symbols.reserve(node.m_dependencies.size());
		context.m_imported_type_signatures.reserve(node.m_dependencies.size());
		imported_typeclass_sources.reserve(node.m_dependencies.size());

		{
			std::lock_guard<std::mutex> lock(env.m_modules_mutex);
			for (const std::string& dep_path : node.m_dependencies)
			{
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

	static MidoriResult::OptimizerResult OptimizeModule(MidoriProgramTree&& ast
#if MIDORI_ENABLE_OPTIMIZER_STATS
		, OptimizerLog* optimizer_log, std::mutex* print_mutex
#endif
	)
	{
#if MIDORI_ENABLE_OPTIMIZER_STATS
		return OptimizerManager(std::move(ast)).Optimize(optimizer_log, print_mutex);
#else
		return OptimizerManager(std::move(ast)).Optimize();
#endif
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
		return OptimizeModule(std::move(state.m_ast)
#if MIDORI_ENABLE_OPTIMIZER_STATS
			, &state.m_optimizer_log, state.m_env ? &state.m_env->m_print_mutex : nullptr
#endif
			)
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

			ReportCompiled
			(
				*m_state.m_env, 
				m_state.m_file_path, 
				m_state.m_tier_idx
#if MIDORI_ENABLE_OPTIMIZER_STATS
				,&m_state.m_optimizer_log
#endif
			);

			return compiled_module;
		}
	};

	static MidoriResult::CompiledModuleResult FinalizeModule(CompileState state)
	{
		return ValidateExports(state.m_export_info.m_export_set, state.m_bytecode, state.m_parsed_module.m_typeclass_metadata, state.m_parsed_module.m_type_signatures, state.m_module_name, state.m_file_path)
			.and_then(CompiledModuleBuilder{ std::move(state) });
	}

	class ModuleCompiler
	{
	public:
		MidoriResult::CompiledModuleResult Compile(CompileEnv& env, const std::string& file_path, size_t tier_idx) const
		{
			MidoriResult::Result<CompileState> state_result = MakeCompileState(env, file_path, tier_idx);
			if (!state_result.has_value())
			{
				return std::unexpected(state_result.error());
			}

			MidoriResult::Result<CompileState> pipeline_result = RunStages(std::move(state_result).value());
			if (!pipeline_result.has_value())
			{
				return std::unexpected(pipeline_result.error());
			}

			return FinalizeModule(std::move(pipeline_result).value());
		}

	private:
		using Stage = MidoriResult::Result<CompileState>(*)(CompileState);

		static MidoriResult::Result<CompileState> RunStages(CompileState state)
		{
			static const std::array<Stage, 6u> stages =
			{
				WithImportContext,
				WithSourceLines,
				WithParsedModule,
				WithTypeCheckedAst,
				WithOptimizedAst,
				WithBytecode
			};

			for (Stage stage : stages)
			{
				MidoriResult::Result<CompileState> result = stage(std::move(state));
				if (!result.has_value())
				{
					return std::unexpected(result.error());
				}

				state = std::move(result).value();
			}

			return state;
		}
	};

	struct PendingModule
	{
		std::string m_file_path;
		MidoriResult::FutureModuleResult m_future;
	};

	static size_t WaitForReadyModule(const std::deque<PendingModule>& pending)
	{
		while (true)
		{
			for (size_t i = 0u; i < pending.size(); i += 1u)
			{
				if (pending[i].m_future.wait_for(std::chrono::milliseconds(0)) == std::future_status::ready)
				{
					return i;
				}
			}

			// Avoid blocking on the oldest task while giving work time to finish.
			pending.front().m_future.wait_for(std::chrono::milliseconds(1));
		}
	}

	static MidoriResult::VoidResult CompileModulesReadyQueue(CompileEnv& env, ModuleCompiler& module_compiler, CompilationSchedule& schedule)
	{
		std::set<std::string> ready;
		for (const std::string& file_path : schedule.m_all_modules)
		{
			if (schedule.m_remaining_deps.at(file_path) == 0u)
			{
				ready.insert(file_path);
			}
		}

		size_t compiled_count = 0u;

#ifndef __EMSCRIPTEN__
		const size_t max_parallel = std::max<size_t>
		(
			1u,
			std::min(schedule.m_all_modules.size(), static_cast<size_t>(std::max(1u, std::thread::hardware_concurrency())))
		);
		std::deque<PendingModule> pending;

		while (!ready.empty() || !pending.empty())
		{
			while (!ready.empty() && pending.size() < max_parallel)
			{
				std::string file_path = *ready.begin();
				ready.erase(ready.begin());
				const size_t tier_idx = schedule.m_tier_indices.at(file_path);

				pending.emplace_back
				(
					PendingModule
					{
						file_path,
						std::async
						(
							std::launch::async,
							[&env, &module_compiler, file_path, tier_idx]() -> MidoriResult::CompiledModuleResult
							{
								return module_compiler.Compile(env, file_path, tier_idx);
							}
						)
					}
				);
			}

			if (pending.empty())
			{
				return std::unexpected("No modules are ready to compile. Check for circular dependencies.\n");
			}

			const size_t ready_idx = WaitForReadyModule(pending);
			PendingModule pending_module = std::move(pending[ready_idx]);
			pending.erase(pending.begin() + ready_idx);

			MidoriResult::CompiledModuleResult result = pending_module.m_future.get();
			if (!result.has_value())
			{
				return std::unexpected(result.error());
			}

			{
				std::lock_guard<std::mutex> lock(env.m_modules_mutex);
				env.m_compiled_modules.emplace(pending_module.m_file_path, std::move(result).value());
			}

			compiled_count += 1u;

			for (const std::string& dependent : schedule.m_dependents.at(pending_module.m_file_path))
			{
				size_t& remaining = schedule.m_remaining_deps.at(dependent);
				if (remaining > 0u)
				{
					remaining -= 1u;
					if (remaining == 0u)
					{
						ready.insert(dependent);
					}
				}
			}
		}
#else
		while (!ready.empty())
		{
			std::string file_path = *ready.begin();
			ready.erase(ready.begin());

			const size_t tier_idx = schedule.m_tier_indices.at(file_path);
			MidoriResult::CompiledModuleResult result = module_compiler.Compile(env, file_path, tier_idx);
			if (!result.has_value())
			{
				return std::unexpected(result.error());
			}

			env.m_compiled_modules.emplace(file_path, std::move(result).value());
			compiled_count += 1u;

			for (const std::string& dependent : schedule.m_dependents.at(file_path))
			{
				size_t& remaining = schedule.m_remaining_deps.at(dependent);
				if (remaining > 0u)
				{
					remaining -= 1u;
					if (remaining == 0u)
					{
						ready.emplace(dependent);
					}
				}
			}
		}
#endif

		if (compiled_count != schedule.m_all_modules.size())
		{
			return std::unexpected("Incomplete compilation: some modules never became ready.\n");
		}

		return {};
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
							CompilationSchedule schedule = BuildCompilationSchedule(build_graph);
							const size_t total_modules = schedule.m_all_modules.size();
							std::unordered_map<std::string, CompiledModule> compiled_modules;
							compiled_modules.reserve(total_modules);
							std::mutex modules_mutex;
							std::mutex print_mutex;
							std::atomic<size_t> completed_modules{ 0u };

							if (schedule.m_tiers.size() > 1u || (schedule.m_tiers.size() == 1u && schedule.m_tiers[0u].size() > 1u))
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
										schedule.m_tiers.size(),
										schedule.m_tiers.size() == 1u ? "" : "s"
									)
								);
								Printer::PrintSeparator(Printer::Color::DARK_GRAY, 60);
							}

							CompileEnv env{ m_file_name, m_source_lines, build_graph, compiled_modules, modules_mutex, print_mutex, completed_modules, schedule.m_tiers, total_modules };
							ModuleCompiler module_compiler;
							MidoriResult::VoidResult compile_result = CompileModulesReadyQueue(env, module_compiler, schedule);
							if (!compile_result.has_value())
							{
								return std::unexpected<std::string>(compile_result.error());
							}

							// Collect all bytecode modules in dependency order
							std::vector<BytecodeModule> all_bytecode_modules;
							all_bytecode_modules.reserve(total_modules);
							for (const std::vector<std::string>& tier : schedule.m_tiers)
							{
								for (const std::string& file_path : tier)
								{
									std::unordered_map<std::string, CompiledModule>::iterator it = compiled_modules.find(file_path);
									if (it == compiled_modules.end())
									{
										return std::unexpected<std::string>(std::format("Missing compiled module for '{}'\n", file_path));
									}
									all_bytecode_modules.emplace_back(std::move(it->second.m_bytecode.value()));
								}
							}

							std::chrono::high_resolution_clock::time_point compile_end = std::chrono::high_resolution_clock::now();
							std::chrono::milliseconds compile_duration = std::chrono::duration_cast<std::chrono::milliseconds>(compile_end - compile_start);

							if (schedule.m_tiers.size() > 1u || (schedule.m_tiers.size() == 1u && schedule.m_tiers[0u].size() > 1u))
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
