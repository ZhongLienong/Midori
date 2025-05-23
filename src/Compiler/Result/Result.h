#pragma once

#include <vector>
#include <expected>
#include <optional>
#include <string>
#include <memory>

#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"
#include "Common/Executable/Executable.h"

namespace MidoriResult
{
	using TokenResult = std::expected<Token, std::string>;
	using LexerResult = std::expected<TokenStream, std::string>;
	using ImportManagerResult = std::expected<BuildGraph, std::string>;
	using ExpressionResult = std::expected<std::unique_ptr<MidoriExpression>, std::string>;
	using StatementResult = std::expected<std::unique_ptr<MidoriStatement>, std::string>;
	using CaseResult = std::expected<Switch::Case, std::string>;
	using ParserResult = std::expected<MidoriProgramTree, std::string>;
	using TypeResult = std::expected<std::shared_ptr<MidoriType>, std::string>;
	using TypeCheckerResult = std::expected<MidoriProgramTree, std::string>;
	using CodeGeneratorResult = std::expected<MidoriExecutable, std::string>;
	using CompilerResult = std::expected<MidoriExecutable, std::string>;
}