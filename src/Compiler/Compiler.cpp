#include "Compiler.h"
#include "Compiler/CodeGenerator/CodeGenerator.h"
#include "Compiler/ModuleManager/ModuleManager.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/Parser/Parser.h"
#include "Compiler/TypeChecker/TypeChecker.h"
#include "Compiler/OptimizerManager/OptimizerManager.h"

#include <sstream>
#include <filesystem>

#ifdef DEBUG
#include "Utility/AbstractSyntaxTreePrinter/AbstractSyntaxTreePrinter.h"
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
							return Parser(build_graph.Stitch(), m_file_name, m_source_lines, &build_graph.m_module_declarations)
								.Parse()
								.and_then
								(
									[this](MidoriProgramTree&& parser_result) -> MidoriResult::CompilerResult
									{
										return TypeChecker(std::move(parser_result), m_file_name, m_source_lines)
											.TypeCheck()
											.and_then
											(
												[this](MidoriProgramTree&& program) -> MidoriResult::CompilerResult
												{
#ifdef DEBUG
													PrintAbstractSyntaxTree ast_printer;
													std::ranges::for_each(program, [&ast_printer](auto&& statement) { std::visit(ast_printer, **statement); });
#endif
													return OptimizerManager(std::move(program))
														.Optimize()
														.and_then
														(
															[this](MidoriProgramTree&& optimized_program) -> MidoriResult::CompilerResult
															{
																return CodeGenerator(std::move(optimized_program), m_file_name, m_source_lines)
																	.GenerateCode()
																	.and_then
																	(
																		[](MidoriExecutable&& compilation_result) ->MidoriResult::CompilerResult
																		{
#ifdef DEBUG
																			for (size_t i : std::views::iota(0u, compilation_result.m_procedure_names.size()))
																			{
																				MidoriText variable_name = compilation_result.m_procedure_names[i];
																				Disassembler::DisassembleBytecodeStream(compilation_result, static_cast<int>(i), variable_name.GetCString());
																			}
#endif
																			return compilation_result;
																		}
																	);
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}