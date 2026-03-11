#include "DeadCodeElimination.h"

#include "Compiler/OptimizerManager/Analysis/OptimizerAnalysis.h"

#include <cstddef>
#include <type_traits>

namespace
{
	bool IsTerminatingStatementImpl(const MidoriStatement& statement);

	bool IsTerminatingExpressionImpl(const MidoriExpression& expression)
	{
		const MidoriExpression* stripped_expression = OptimizerAnalysis::StripRedundantGroups(&expression);
		if (stripped_expression == nullptr)
		{
			return false;
		}

		return std::visit
		(
			[](const auto& node) -> bool
			{
				using T = std::decay_t<decltype(node)>;

				if constexpr (std::is_same_v<T, MidoriExpression::Return> || std::is_same_v<T, MidoriExpression::Break>)
				{
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Block>)
				{
					for (const std::unique_ptr<MidoriStatement>& statement : node.m_stmts)
					{
						if (IsTerminatingStatementImpl(*statement))
						{
							return true;
						}
					}

					return node.m_final_expr.has_value() && IsTerminatingExpressionImpl(*node.m_final_expr.value());
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IfElse>)
				{
					return IsTerminatingExpressionImpl(*node.m_true_branch)
						&& IsTerminatingExpressionImpl(*node.m_else_branch);
				}
				else
				{
					return false;
				}
			},
			**stripped_expression
		);
	}

	bool IsTerminatingStatementImpl(const MidoriStatement& statement)
	{
		return std::visit
		(
			[](const auto& node) -> bool
			{
				using T = std::decay_t<decltype(node)>;

				if constexpr (std::is_same_v<T, MidoriStatement::Continue>)
				{
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::ExpressionStatement>)
				{
					return IsTerminatingExpressionImpl(*node.m_expr);
				}
				else
				{
					return false;
				}
			},
			*statement
		);
	}
}

MidoriResult::OptimizerResult DeadCodeElimination::Optimize(MidoriProgramTree program_tree)
{
	ResetPassState();
	ProcessTopLevelStatements(program_tree);
	return std::move(program_tree);
}

std::string_view DeadCodeElimination::GetName() const
{
	return "DeadCodeElimination";
}

void DeadCodeElimination::ProcessTopLevelStatements(std::vector<std::unique_ptr<MidoriStatement>>& statements)
{
	for (std::unique_ptr<MidoriStatement>& statement : statements)
	{
		VisitStatement(statement);
	}

	RemovePureExpressionStatements(statements);
}

void DeadCodeElimination::RemovePureExpressionStatements(std::vector<std::unique_ptr<MidoriStatement>>& statements)
{
	for (std::vector<std::unique_ptr<MidoriStatement>>::iterator it = statements.begin(); it != statements.end();)
	{
		MidoriStatement* statement = it->get();
		if (!statement->IsStatement<MidoriStatement::ExpressionStatement>())
		{
			++it;
			continue;
		}

		const MidoriExpression& expression = *statement->GetStatement<MidoriStatement::ExpressionStatement>().m_expr;
		if (!OptimizerAnalysis::IsPure(expression))
		{
			++it;
			continue;
		}

		it = statements.erase(it);
		MarkOptimization();
	}
}

void DeadCodeElimination::TrimUnreachableBlockTail(MidoriExpression::Block& block)
{
	std::optional<std::size_t> terminating_index = std::nullopt;
	for (std::size_t index = 0u; index < block.m_stmts.size(); index += 1u)
	{
		if (IsTerminatingStatement(*block.m_stmts[index]))
		{
			terminating_index = index;
			break;
		}
	}

	if (!terminating_index.has_value())
	{
		return;
	}

	const std::size_t reachable_statement_count = terminating_index.value() + 1u;
	while (block.m_stmts.size() > reachable_statement_count)
	{
		block.m_stmts.pop_back();
		MarkOptimization();
	}

	if (block.m_final_expr.has_value())
	{
		block.m_final_expr.reset();
		MarkOptimization();
	}
}

void DeadCodeElimination::ElideUnusedPureLocalDefinitions(MidoriExpression::Block& block)
{
	const OptimizerAnalysis::BlockLocalAccessSummary access_summary = OptimizerAnalysis::AnalyzeBlockLocalAccess(block);

	for (std::size_t index = 0u; index < block.m_stmts.size(); index += 1u)
	{
		MidoriStatement* statement = block.m_stmts[index].get();
		if (!statement->IsStatement<MidoriStatement::VariableDefinition>())
		{
			continue;
		}

		MidoriStatement::VariableDefinition& definition = statement->GetStatement<MidoriStatement::VariableDefinition>();
		if (definition.m_is_elided || !definition.m_local_index.has_value())
		{
			continue;
		}

		if (!OptimizerAnalysis::IsPure(*definition.m_value))
		{
			continue;
		}

		const int local_index = definition.m_local_index.value();
		if (access_summary.IsLocalUsedAfter(local_index, index)
			|| access_summary.IsLocalAssignedAfter(local_index, index)
			|| HasNestedCallableBoundaryAfter(access_summary, index))
		{
			continue;
		}

		definition.m_is_elided = true;
		MarkOptimization();
	}
}

bool DeadCodeElimination::IsTerminatingStatement(const MidoriStatement& statement)
{
	return IsTerminatingStatementImpl(statement);
}

bool DeadCodeElimination::HasNestedCallableBoundaryAfter(const OptimizerAnalysis::BlockLocalAccessSummary& access_summary, std::size_t statement_index)
{
	const std::size_t start_index = statement_index + 1u;
	for (std::size_t index = start_index; index < access_summary.m_statement_summaries.size(); index += 1u)
	{
		if (access_summary.m_statement_summaries[index].m_has_nested_callable_boundary)
		{
			return true;
		}
	}

	return access_summary.m_final_expression_summary.has_value()
		&& access_summary.m_final_expression_summary->m_has_nested_callable_boundary;
}

void DeadCodeElimination::operator()(MidoriStatement::FunctionDefinition& defun)
{
	VisitAndReplace(defun.m_body);
}

void DeadCodeElimination::operator()(MidoriExpression::Block& block)
{
	for (std::unique_ptr<MidoriStatement>& statement : block.m_stmts)
	{
		VisitStatement(statement);
	}

	if (block.m_final_expr.has_value())
	{
		VisitAndReplace(block.m_final_expr.value());
	}

	TrimUnreachableBlockTail(block);
	RemovePureExpressionStatements(block.m_stmts);
	ElideUnusedPureLocalDefinitions(block);
}
