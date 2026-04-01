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
	struct CompilerDiagnostics
	{
		std::vector<CompilerError> m_errors;

		CompilerDiagnostics() = default;

		explicit CompilerDiagnostics(CompilerError error)
			: m_errors{ std::move(error) }
		{
		}

		explicit CompilerDiagnostics(std::vector<CompilerError>&& errors)
			: m_errors(std::move(errors))
		{
		}

		[[nodiscard]] bool Empty() const
		{
			return m_errors.empty();
		}

		[[nodiscard]] size_t Size() const
		{
			return m_errors.size();
		}

		[[nodiscard]] const CompilerError& First() const
		{
			return m_errors.front();
		}

		[[nodiscard]] CompilerError TakeFirst() &&
		{
			return std::move(m_errors.front());
		}

		[[nodiscard]] std::string Rendered() const
		{
			std::string rendered;
			for (const CompilerError& error : m_errors)
			{
				const std::string_view error_rendered = error.Rendered();
				rendered.append(error_rendered);
				if (!rendered.empty() && rendered.back() != '\n')
				{
					rendered.push_back('\n');
				}
			}

			return rendered;
		}
	};

	template<typename ValueType>
	using Result = std::expected<ValueType, CompilerError>;

	template<typename ValueType>
	using DiagnosticsResult = std::expected<ValueType, CompilerDiagnostics>;

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
	using ParserResult = DiagnosticsResult<MidoriProgramTree>;
	using TypeResult = Result<std::shared_ptr<MidoriType>>;
	using TypeListResult = Result<std::vector<std::shared_ptr<MidoriType>>>;
	using TypeCheckerResult = DiagnosticsResult<MidoriProgramTree>;
	using OptimizerResult = Result<MidoriProgramTree>;
	using CodeGeneratorResult = DiagnosticsResult<BytecodeModule>;
	using CompiledModuleResult = DiagnosticsResult<CompiledModule>;
	using CompilerResult = DiagnosticsResult<MidoriExecutable>;

	// Generic result types
	using VoidResult = Result<void>;
	using VoidResultList = std::vector<VoidResult>;

	// Async compilation result types
	using FutureModuleResult = std::future<CompiledModuleResult>;
}
