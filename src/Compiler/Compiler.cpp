#include "Compiler.h"
#include "Compiler/CodeGenerator/CodeGenerator.h"
#include "Compiler/Lexer/Lexer.h"
#include "Compiler/Parser/Parser.h"
#include "TypeChecker/TypeChecker.h"
#include "Compiler/ImportManager/ImportManager.h"


#ifdef DEBUG
#include "Utility/AbstractSyntaxTreePrinter/AbstractSyntaxTreePrinter.h"
#include "Utility/Disassembler/Disassembler.h"
#endif

Compiler::Compiler(std::string&& source_code, std::string&& file_name) : m_source_code(std::move(source_code)), m_file_name(std::move(file_name))
{
}

MidoriResult::CompilerResult Compiler::Compile()
{
	return Lexer(std::move(m_source_code))
		.Lex()
		.and_then
		(
			[this](TokenStream&& lexer_result) -> MidoriResult::CompilerResult
			{
				return ImportManager(std::move(lexer_result), std::string(m_file_name))
					.GenerateBuildGraph()
					.and_then
					(
						[this](BuildGraph&& build_graph) -> MidoriResult::CompilerResult
						{
							return Parser(build_graph.Stitch(), m_file_name)
								.Parse()
								.and_then
								(
									[](MidoriProgramTree&& parser_result) -> MidoriResult::CompilerResult
									{
										return TypeChecker()
											.TypeCheck(std::move(parser_result))
											.and_then
											(
												[](MidoriProgramTree&& program) -> MidoriResult::CompilerResult
												{
#ifdef DEBUG
													PrintAbstractSyntaxTree ast_printer;
													std::ranges::for_each(program, [&ast_printer](auto&& statement) { std::visit(ast_printer, *statement); });
#endif
													return CodeGenerator()
														.GenerateCode(std::move(program))
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