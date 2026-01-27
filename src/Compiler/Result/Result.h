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
#include "Compiler/BuildGraph/BuildGraph.h"

namespace MidoriResult
{
	template<typename ValueType>
	using Result = std::expected<ValueType, std::string>;

	using TokenResult = Result<Token>;
	using TokenListResult = Result<std::vector<Token>>;
	using FunctionParamResult = Result<std::pair<Token, std::shared_ptr<MidoriType>>>;
	using FunctionParamsResult = Result<std::vector<std::pair<Token, std::shared_ptr<MidoriType>>>>;
	using LexerResult = Result<TokenStream>;
	using ModuleManagerResult = Result<BuildGraph>;
	using BytecodeLinkerResult = Result<MidoriExecutable>;
	using ExpressionResult = Result<std::unique_ptr<MidoriExpression>>;
	using StatementResult = Result<std::unique_ptr<MidoriStatement>>;
	using ParserResult = Result<MidoriProgramTree>;
	using TypeResult = Result<std::shared_ptr<MidoriType>>;
	using TypeListResult = Result<std::vector<std::shared_ptr<MidoriType>>>;
	using TypeCheckerResult = Result<MidoriProgramTree>;
	using OptimizerResult = Result<MidoriProgramTree>;
	using CodeGeneratorResult = Result<BytecodeModule>;
	using CompiledModuleResult = Result<CompiledModule>;
	using CompilerResult = Result<MidoriExecutable>;

	// Generic result types
	using VoidResult = Result<void>;
	using VoidResultList = std::vector<VoidResult>;

	// Async compilation result types
	using FutureModuleResult = std::future<CompiledModuleResult>;
}
