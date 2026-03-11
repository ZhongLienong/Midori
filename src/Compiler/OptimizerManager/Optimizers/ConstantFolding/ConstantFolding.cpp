#include "ConstantFolding.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Compiler/OptimizerManager/Analysis/OptimizerAnalysis.h"

MidoriResult::OptimizerResult ConstantFolding::Optimize(MidoriProgramTree program_tree)
{
#if MIDORI_ENABLE_OPTIMIZER_STATS
	ResetCounter();
#endif

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

std::string_view ConstantFolding::GetName() const
{
	return "ConstantFolding";
}

void ConstantFolding::operator()(MidoriExpression::Binary& binary)
{
	VisitAndReplace(binary.m_left);
	VisitAndReplace(binary.m_right);

	std::optional<OptimizerAnalysis::ConstantValue> folded_value = OptimizerAnalysis::TryEvalConstant(binary);
	if (!folded_value.has_value())
	{
		return;
	}

	m_pending_replacement = OptimizerAnalysis::MakeLiteralExpression(folded_value.value(), binary.m_op);
	m_pending_replacement->GetType() = binary.m_type_data;
}

void ConstantFolding::operator()(MidoriExpression::UnaryPrefix& unary)
{
	VisitAndReplace(unary.m_expr);

	std::optional<OptimizerAnalysis::ConstantValue> folded_value = OptimizerAnalysis::TryEvalConstant(unary);
	if (!folded_value.has_value())
	{
		return;
	}

	m_pending_replacement = OptimizerAnalysis::MakeLiteralExpression(folded_value.value(), unary.m_op);
	m_pending_replacement->GetType() = unary.m_type_data;
}
