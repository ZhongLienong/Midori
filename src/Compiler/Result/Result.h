#pragma once

#include <expected>
#include <future>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "Common/Error/Error.h"
#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"
#include "Compiler/BytecodeModule/BytecodeModule.h"
#include "Compiler/Module/CompiledModule.h"
#include "Compiler/BuildGraph/BuildGraph.h"

namespace MidoriResult
{
	struct ParserDiagnostics
	{
		std::vector<CompilerError> m_errors;

		ParserDiagnostics() = default;

		explicit ParserDiagnostics(CompilerError error)
			: m_errors{ std::move(error) }
		{
		}

		explicit ParserDiagnostics(std::vector<CompilerError>&& errors)
			: m_errors(std::move(errors))
		{
		}

		[[nodiscard]] const CompilerError& First() const
		{
			return m_errors.front();
		}

		[[nodiscard]] CompilerError TakeFirst() &&
		{
			return std::move(m_errors.front());
		}
	};

	template<typename ValueType>
	using Result = std::expected<ValueType, CompilerError>;

	using Error = CompilerError;
	using TokenResult = Result<Token>;
	using TokenListResult = Result<std::vector<Token>>;
	using FunctionParamResult = Result<std::pair<Token, std::shared_ptr<MidoriType>>>;
	using FunctionParamsResult = Result<std::vector<std::pair<Token, std::shared_ptr<MidoriType>>>>;
	using LexerResult = Result<TokenStream>;
	using ModuleManagerResult = Result<BuildGraph>;
	using BytecodeLinkerResult = Result<MidoriExecutable>;
	using ExpressionResult = Result<std::unique_ptr<MidoriExpression>>;
	using PatternResult = Result<std::unique_ptr<MidoriPattern>>;
	using StatementResult = Result<std::unique_ptr<MidoriStatement>>;
	using ParserResult = std::expected<MidoriProgramTree, ParserDiagnostics>;
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
