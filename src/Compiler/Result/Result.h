#pragma once

#include <expected>
#include <future>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"
#include "Compiler/BytecodeModule/BytecodeModule.h"
#include "Compiler/Module/CompiledModule.h"

namespace MidoriResult
{
	using TokenResult = std::expected<Token, std::string>;
	using TokenListResult = std::expected<std::vector<Token>, std::string>;
	using FunctionParamResult = std::expected<std::pair<Token, std::shared_ptr<MidoriType>>, std::string>;
	using FunctionParamsResult = std::expected<std::vector<std::pair<Token, std::shared_ptr<MidoriType>>>, std::string>;
	using LexerResult = std::expected<TokenStream, std::string>;
	using ModuleManagerResult = std::expected<BuildGraph, std::string>;
	using BytecodeLinkerResult = std::expected<MidoriExecutable, std::string>;
	using ExpressionResult = std::expected<std::unique_ptr<MidoriExpression>, std::string>;
	using StatementResult = std::expected<std::unique_ptr<MidoriStatement>, std::string>;
	using ParserResult = std::expected<MidoriProgramTree, std::string>;
	using TypeResult = std::expected<std::shared_ptr<MidoriType>, std::string>;
	using TypeListResult = std::expected<std::vector<std::shared_ptr<MidoriType>>, std::string>;
	using TypeCheckerResult = std::expected<MidoriProgramTree, std::string>;
	using OptimizerResult = std::expected<MidoriProgramTree, std::string>;
	using CodeGeneratorResult = std::expected<BytecodeModule, std::string>;
	using CompiledModuleResult = std::expected<CompiledModule, std::string>;
	using CompilerResult = std::expected<MidoriExecutable, std::string>;

	// Generic result types
	using VoidResult = std::expected<void, std::string>;
	using VoidResultList = std::vector<VoidResult>;

	// Async compilation result types
	using FutureModuleResult = std::future<CompiledModuleResult>;
}