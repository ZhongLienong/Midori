#include "UnreachableCodeDiagnostic.h"

#include "Compiler/Analysis/SharedAnalysis.h"

std::string_view UnreachableCodeDiagnostic::GetName() const
{
	return "UnreachableCodeDiagnostic";
}

void UnreachableCodeDiagnostic::operator()(MidoriExpression::Block& block)
{
	bool reachable = true;

	for (std::unique_ptr<MidoriStatement>& statement : block.m_stmts)
	{
		if (!reachable)
		{
			const Token* token = MidoriAnalysis::GetPrimaryToken(*statement);
			if (token != nullptr)
			{
				EmitWarning
				(
					CompilerWarningCode::UnreachableCode,
					*token,
					"This code is unreachable because control flow already exits earlier in the block."
				);
			}
			continue;
		}

		VisitStatement(statement);
		reachable = !MidoriAnalysis::IsTerminatingStatement(*statement);
	}

	if (!block.m_final_expr.has_value())
	{
		return;
	}

	if (!reachable)
	{
		const Token* token = MidoriAnalysis::GetPrimaryToken(*block.m_final_expr.value());
		if (token != nullptr)
		{
			EmitWarning
			(
				CompilerWarningCode::UnreachableCode,
				*token,
				"This code is unreachable because control flow already exits earlier in the block."
			);
		}
		return;
	}

	VisitExpression(block.m_final_expr.value());
}
