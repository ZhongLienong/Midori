#pragma once

#include "Common/Value/Value.h"
#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"

#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace OptimizerAnalysis
{
	struct UnitConstant
	{
		bool operator==(const UnitConstant&) const = default;
	};

	struct ConstantValue
	{
		using Variant = std::variant<MidoriBool, MidoriInteger, MidoriFloat, MidoriByte, MidoriWord, std::string, UnitConstant>;

		Variant m_value;

		ConstantValue() = default;

		template<typename T>
		ConstantValue(T&& value)
			: m_value(std::forward<T>(value))
		{
		}

		template<typename T>
		bool Is() const
		{
			return std::holds_alternative<T>(m_value);
		}

		template<typename T>
		const T& Get() const
		{
			return std::get<T>(m_value);
		}
	};

	enum class LiteralForm
	{
		None,
		Bool,
		Integer,
		Float,
		Byte,
		Word,
		Text,
		Unit
	};

	const MidoriExpression* StripRedundantGroups(const MidoriExpression* expr);

	std::unique_ptr<MidoriExpression> StripRedundantGroups(std::unique_ptr<MidoriExpression> expr);

	LiteralForm GetLiteralForm(const MidoriExpression& expr);

	bool IsLiteralExpression(const MidoriExpression& expr);

	bool IsPure(const MidoriExpression& expr);

	std::optional<ConstantValue> TryEvalConstant(const MidoriExpression& expr);

	std::optional<ConstantValue> TryEvalConstant(const MidoriExpression::Binary& expr);

	std::optional<ConstantValue> TryEvalConstant(const MidoriExpression::UnaryPrefix& expr);

	std::optional<bool> TryEvalTruthValue(const MidoriExpression& expr);

	std::unique_ptr<MidoriExpression> MakeLiteralExpression(const ConstantValue& value, const Token& source_token);

	struct LocalAccessInfo
	{
		int m_reads = 0;
		int m_assignments = 0;
	};

	struct StatementLocalAccessSummary
	{
		std::unordered_map<int, LocalAccessInfo> m_locals;
		bool m_has_nested_callable_boundary = false;

		bool UsesLocal(int local_index) const;

		bool AssignsLocal(int local_index) const;

		int GetUseCount(int local_index) const;

		int GetAssignmentCount(int local_index) const;
	};

	struct BlockLocalAccessSummary
	{
		std::vector<StatementLocalAccessSummary> m_statement_summaries;
		std::optional<StatementLocalAccessSummary> m_final_expression_summary;

		bool IsLocalUsedAfter(int local_index, std::size_t statement_index) const;

		bool IsLocalAssignedAfter(int local_index, std::size_t statement_index) const;
	};

	StatementLocalAccessSummary AnalyzeStatementLocalAccess(const MidoriStatement& stmt);

	StatementLocalAccessSummary AnalyzeExpressionLocalAccess(const MidoriExpression& expr);

	BlockLocalAccessSummary AnalyzeBlockLocalAccess(const MidoriExpression::Block& block);
}
