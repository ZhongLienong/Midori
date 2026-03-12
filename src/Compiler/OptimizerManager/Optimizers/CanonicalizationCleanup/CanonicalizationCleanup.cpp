#include "CanonicalizationCleanup.h"

#include "Compiler/OptimizerManager/Analysis/OptimizerAnalysis.h"

#include <optional>
#include <ranges>

namespace
{
	bool TryGetPureTruthValue(const MidoriExpression& expr, bool expected_value)
	{
		if (!OptimizerAnalysis::IsPure(expr))
		{
			return false;
		}

		const std::optional<bool> truth_value = OptimizerAnalysis::TryEvalTruthValue(expr);
		return truth_value.has_value() && truth_value.value() == expected_value;
	}

	bool TryGetPureNumericConstant(const MidoriExpression& expr, bool expect_zero)
	{
		if (!OptimizerAnalysis::IsPure(expr))
		{
			return false;
		}

		const std::optional<OptimizerAnalysis::ConstantValue> constant_value = OptimizerAnalysis::TryEvalConstant(expr);
		if (!constant_value.has_value())
		{
			return false;
		}

		if (constant_value->Is<MidoriInteger>())
		{
			const MidoriInteger expected_value = expect_zero ? 0ll : 1ll;
			return constant_value->Get<MidoriInteger>() == expected_value;
		}

		if (constant_value->Is<MidoriFloat>())
		{
			const MidoriFloat expected_value = expect_zero ? 0.0 : 1.0;
			return constant_value->Get<MidoriFloat>() == expected_value;
		}

		if (constant_value->Is<MidoriByte>())
		{
			const MidoriByte expected_value = expect_zero ? static_cast<MidoriByte>(0u) : static_cast<MidoriByte>(1u);
			return constant_value->Get<MidoriByte>() == expected_value;
		}

		if (constant_value->Is<MidoriWord>())
		{
			const MidoriWord expected_value = expect_zero ? 0ull : 1ull;
			return constant_value->Get<MidoriWord>() == expected_value;
		}

		return false;
	}

	bool IsPureZero(const MidoriExpression& expr)
	{
		return TryGetPureNumericConstant(expr, true);
	}

	bool IsPureOne(const MidoriExpression& expr)
	{
		return TryGetPureNumericConstant(expr, false);
	}

	bool IsPureEmptyText(const MidoriExpression& expr)
	{
		if (!OptimizerAnalysis::IsPure(expr))
		{
			return false;
		}

		const std::optional<OptimizerAnalysis::ConstantValue> constant_value = OptimizerAnalysis::TryEvalConstant(expr);
		return constant_value.has_value()
			&& constant_value->Is<std::string>()
			&& constant_value->Get<std::string>().empty();
	}

	bool IsEmptyArrayLiteral(const MidoriExpression& expr)
	{
		const MidoriExpression* stripped_expr = OptimizerAnalysis::StripRedundantGroups(&expr);
		if (stripped_expr == nullptr || !stripped_expr->IsExpression<MidoriExpression::Array>())
		{
			return false;
		}

		const MidoriExpression::Array& array_expr = stripped_expr->GetExpression<MidoriExpression::Array>();
		return array_expr.m_elems.empty();
	}

	bool IsConcatIdentityValue(const MidoriExpression& expr)
	{
		return IsPureEmptyText(expr) || IsEmptyArrayLiteral(expr);
	}

	void SetReplacementType(std::unique_ptr<MidoriExpression>& replacement, const std::shared_ptr<MidoriType>& type)
	{
		replacement->GetType() = type;
	}
}

MidoriResult::OptimizerResult CanonicalizationCleanup::Optimize(MidoriProgramTree program_tree)
{
	ResetPassState();

	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			VisitStatement(stmt);
		}
	);

	return std::move(program_tree);
}

std::string_view CanonicalizationCleanup::GetName() const
{
	return "CanonicalizationCleanup";
}

void CanonicalizationCleanup::operator()(MidoriExpression::As& as)
{
	VisitAndReplace(as.m_expr);

	if (as.m_uses_convertable)
	{
		return;
	}

	const std::shared_ptr<MidoriType> from_type = as.m_from_type.lock();
	if (!from_type || *from_type != *as.m_to_type)
	{
		return;
	}

	m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(as.m_expr));
	SetReplacementType(m_pending_replacement, as.m_type_data);
}

void CanonicalizationCleanup::operator()(MidoriExpression::Binary& binary)
{
	VisitAndReplace(binary.m_left);
	VisitAndReplace(binary.m_right);

	switch (binary.m_op.m_token_name)
	{
	case Token::Name::SINGLE_PLUS:
		if (IsPureZero(*binary.m_right))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
			return;
		}

		if (IsPureZero(*binary.m_left))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_right));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::SINGLE_MINUS:
		if (IsPureZero(*binary.m_right))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::STAR:
		if (IsPureOne(*binary.m_right))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
			return;
		}

		if (IsPureOne(*binary.m_left))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_right));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::SLASH:
		if (IsPureOne(*binary.m_right))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::LEFT_SHIFT:
	case Token::Name::RIGHT_SHIFT:
	case Token::Name::SINGLE_BAR:
	case Token::Name::CARET:
		if (IsPureZero(*binary.m_right))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
			return;
		}

		if ((binary.m_op.m_token_name == Token::Name::SINGLE_BAR || binary.m_op.m_token_name == Token::Name::CARET)
			&& IsPureZero(*binary.m_left))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_right));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::DOUBLE_AMPERSAND:
		if (TryGetPureTruthValue(*binary.m_left, true))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_right));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
			return;
		}

		if (TryGetPureTruthValue(*binary.m_right, true))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::DOUBLE_BAR:
		if (TryGetPureTruthValue(*binary.m_left, false))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_right));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
			return;
		}

		if (TryGetPureTruthValue(*binary.m_right, false))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	case Token::Name::DOUBLE_PLUS:
		if (IsConcatIdentityValue(*binary.m_left))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_right));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
			return;
		}

		if (IsConcatIdentityValue(*binary.m_right))
		{
			m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(binary.m_left));
			SetReplacementType(m_pending_replacement, binary.m_type_data);
		}
		return;
	default:
		return;
	}
}

void CanonicalizationCleanup::operator()(MidoriExpression::Group& group)
{
	VisitAndReplace(group.m_expr_in);

	m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(group.m_expr_in));
	SetReplacementType(m_pending_replacement, group.m_type_data);
}

void CanonicalizationCleanup::operator()(MidoriExpression::UnaryPrefix& unary)
{
	VisitAndReplace(unary.m_expr);

	if (unary.m_op.m_token_name == Token::Name::SINGLE_PLUS)
	{
		m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(unary.m_expr));
		SetReplacementType(m_pending_replacement, unary.m_type_data);
		return;
	}

	if (!unary.m_expr->IsExpression<MidoriExpression::UnaryPrefix>())
	{
		return;
	}

	MidoriExpression::UnaryPrefix& inner_unary = unary.m_expr->GetExpression<MidoriExpression::UnaryPrefix>();
	if (inner_unary.m_op.m_token_name != unary.m_op.m_token_name)
	{
		return;
	}

	if (unary.m_op.m_token_name != Token::Name::BANG
		&& unary.m_op.m_token_name != Token::Name::SINGLE_MINUS
		&& unary.m_op.m_token_name != Token::Name::TILDE)
	{
		return;
	}

	m_pending_replacement = OptimizerAnalysis::StripRedundantGroups(std::move(inner_unary.m_expr));
	SetReplacementType(m_pending_replacement, unary.m_type_data);
}
