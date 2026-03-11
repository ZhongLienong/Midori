#include "SelfConcatOptimization.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Compiler/OptimizerManager/Analysis/OptimizerAnalysis.h"

#include <ranges>

namespace
{
	bool IsSameNameAccess(const MidoriExpression::NameAccess& access, const MidoriExpression::NameContext::Tag& ctx, const Token& name)
	{
		return std::visit
		(
			[&access, &name](const auto& tag) -> bool
			{
				using T = std::decay_t<decltype(tag)>;
				if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Local>)
				{
					if (!std::holds_alternative<MidoriExpression::NameContext::Local>(access.m_name_ctx))
					{
						return false;
					}
					return std::get<MidoriExpression::NameContext::Local>(access.m_name_ctx).m_index == tag.m_index;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::NameContext::Cell>)
				{
					if (!std::holds_alternative<MidoriExpression::NameContext::Cell>(access.m_name_ctx))
					{
						return false;
					}
					return std::get<MidoriExpression::NameContext::Cell>(access.m_name_ctx).m_index == tag.m_index;
				}
				else
				{
					if (!std::holds_alternative<MidoriExpression::NameContext::Global>(access.m_name_ctx))
					{
						return false;
					}
					return access.m_name.m_lexeme == name.m_lexeme;
				}
			},
			ctx
		);
	}

	bool ContainsNameAccess(const MidoriExpression& expr, const MidoriExpression::NameContext::Tag& ctx, const Token& name);

	struct NameAccessVisitor
	{
		const MidoriExpression::NameContext::Tag& m_ctx;
		const Token& m_name;

		bool operator()(const MidoriExpression::NameAccess& node) const
		{
			return IsSameNameAccess(node, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::As& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Binary& node) const
		{
			return ContainsNameAccess(*node.m_left, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_right, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::UnaryPrefix& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::UnarySuffix& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Group& node) const
		{
			return ContainsNameAccess(*node.m_expr_in, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Tuple& node) const
		{
			return std::ranges::any_of
			(
				node.m_elements,
				[this](const std::unique_ptr<MidoriExpression>& elem)
				{
					return ContainsNameAccess(*elem, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::Assignment& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::AppendAssign& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::ExtendAssign& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::PrependAssign& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::CompoundAssign& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Call& node) const
		{
			if (ContainsNameAccess(*node.m_callee, m_ctx, m_name))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_arguments,
				[this](const std::unique_ptr<MidoriExpression>& arg)
				{
					return ContainsNameAccess(*arg, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::Function& node) const
		{
			return ContainsNameAccess(*node.m_body, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Construct& node) const
		{
			return std::ranges::any_of
			(
				node.m_params,
				[this](const std::unique_ptr<MidoriExpression>& param)
				{
					return ContainsNameAccess(*param, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::IfElse& node) const
		{
			return ContainsNameAccess(*node.m_condition, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_true_branch, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_else_branch, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::MemberAccess& node) const
		{
			return ContainsNameAccess(*node.m_struct, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::MemberAssignment& node) const
		{
			return ContainsNameAccess(*node.m_struct, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Array& node) const
		{
			return std::ranges::any_of
			(
				node.m_elems,
				[this](const std::unique_ptr<MidoriExpression>& elem)
				{
					return ContainsNameAccess(*elem, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::IndexAccess& node) const
		{
			if (ContainsNameAccess(*node.m_arr_var, m_ctx, m_name))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_indices,
				[this](const std::unique_ptr<MidoriExpression>& index)
				{
					return ContainsNameAccess(*index, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::IndexAssignment& node) const
		{
			if (ContainsNameAccess(*node.m_arr_var, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_value, m_ctx, m_name))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_indices,
				[this](const std::unique_ptr<MidoriExpression>& index)
				{
					return ContainsNameAccess(*index, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::ArrayComprehension& node) const
		{
			return ContainsNameAccess(*node.m_transform_expr, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_range, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::RangeBinary& node) const
		{
			return ContainsNameAccess(*node.m_start, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_end, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::RangeTernary& node) const
		{
			return ContainsNameAccess(*node.m_start, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_step, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_end, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Block& node) const
		{
			const bool has_statement = std::ranges::any_of(
				node.m_stmts,
				[this](const std::unique_ptr<MidoriStatement>& stmt)
				{
					return stmt->IsStatement<MidoriStatement::ExpressionStatement>() && ContainsNameAccess(*stmt->GetStatement<MidoriStatement::ExpressionStatement>().m_expr, m_ctx, m_name);
				}
			);

			const bool has_final_expr = node.m_final_expr.has_value() && ContainsNameAccess(*node.m_final_expr.value(), m_ctx, m_name);

			return has_statement || has_final_expr;
		}

		bool operator()(const MidoriExpression::Match& node) const
		{
			if (ContainsNameAccess(*node.m_arg_expr, m_ctx, m_name))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_cases,
				[this](const std::unique_ptr<MidoriExpression>& case_expr)
				{
					return ContainsNameAccess(*case_expr, m_ctx, m_name);
				}
			);
		}

		bool operator()(const MidoriExpression::Case& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Default& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Loop& node) const
		{
			return ContainsNameAccess(*node.m_body, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::For& node) const
		{
			return ContainsNameAccess(*node.m_range, m_ctx, m_name)
				|| ContainsNameAccess(*node.m_body, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Return& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Break& node) const
		{
			return ContainsNameAccess(*node.m_value, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Async& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		bool operator()(const MidoriExpression::Await& node) const
		{
			return ContainsNameAccess(*node.m_expr, m_ctx, m_name);
		}

		template <typename T>
		bool operator()(const T&) const
		{
			return false;
		}
	};

	bool ContainsNameAccess(const MidoriExpression& expr, const MidoriExpression::NameContext::Tag& ctx, const Token& name)
	{
		return std::visit(NameAccessVisitor{ ctx, name }, *expr);
	}

	void CollectConcatOperands(const MidoriExpression& expr, std::vector<const MidoriExpression*>& operands)
	{
		const MidoriExpression* current = OptimizerAnalysis::StripRedundantGroups(&expr);
		if (current && current->IsExpression<MidoriExpression::Binary>())
		{
			const MidoriExpression::Binary& binary = current->GetExpression<MidoriExpression::Binary>();
			if (binary.m_op.m_token_name == Token::Name::DOUBLE_PLUS)
			{
				CollectConcatOperands(*binary.m_left, operands);
				CollectConcatOperands(*binary.m_right, operands);
				return;
			}
		}
		operands.emplace_back(current);
	}

	void CollectConcatOperands(std::unique_ptr<MidoriExpression> expr, std::vector<std::unique_ptr<MidoriExpression>>& operands)
	{
		expr = OptimizerAnalysis::StripRedundantGroups(std::move(expr));
		if (expr && expr->IsExpression<MidoriExpression::Binary>())
		{
			MidoriExpression::Binary& binary = expr->GetExpression<MidoriExpression::Binary>();
			if (binary.m_op.m_token_name == Token::Name::DOUBLE_PLUS)
			{
				CollectConcatOperands(std::move(binary.m_left), operands);
				CollectConcatOperands(std::move(binary.m_right), operands);
				return;
			}
		}
		operands.emplace_back(std::move(expr));
	}

	std::unique_ptr<MidoriExpression> BuildSelfConcatBlock(MidoriExpression::Assignment& bind)
	{
		if (!bind.m_type_data->IsType<MidoriType::TextType>() && !bind.m_type_data->IsType<MidoriType::ArrayType>())
		{
			return nullptr;
		}

		if (!std::holds_alternative<MidoriExpression::NameContext::Local>(bind.m_name_ctx))
		{
			return nullptr;
		}

		std::vector<const MidoriExpression*> operands;
		CollectConcatOperands(*bind.m_value, operands);
		if (operands.size() < 2u)
		{
			return nullptr;
		}

		const MidoriExpression* first_expr = OptimizerAnalysis::StripRedundantGroups(operands[0u]);
		if (!first_expr || !first_expr->IsExpression<MidoriExpression::NameAccess>())
		{
			return nullptr;
		}

		const MidoriExpression::NameAccess& name_access = first_expr->GetExpression<MidoriExpression::NameAccess>();
		if (!IsSameNameAccess(name_access, bind.m_name_ctx, bind.m_name))
		{
			return nullptr;
		}

		for (size_t idx = 1uz; idx < operands.size(); idx += 1uz)
		{
			if (ContainsNameAccess(*operands[idx], bind.m_name_ctx, bind.m_name))
			{
				return nullptr;
			}
		}

		std::vector<std::unique_ptr<MidoriExpression>> owned_operands;
		CollectConcatOperands(std::move(bind.m_value), owned_operands);
		if (owned_operands.size() <= 1uz)
		{
			return nullptr;
		}
		owned_operands.erase(owned_operands.begin());

		std::vector<std::unique_ptr<MidoriStatement>> statements;
		statements.reserve(owned_operands.size());

		for (std::unique_ptr<MidoriExpression>& operand : owned_operands)
		{
			MidoriExpression::NameContext::Tag name_ctx = bind.m_name_ctx;
			std::unique_ptr<MidoriExpression> update_expr;
			if (bind.m_type_data->IsType<MidoriType::TextType>())
			{
				update_expr = std::make_unique<MidoriExpression>(MidoriExpression::AppendAssign(bind.m_name, std::move(operand), std::move(name_ctx)));
			}
			else
			{
				update_expr = std::make_unique<MidoriExpression>(MidoriExpression::ExtendAssign(bind.m_name, std::move(operand), std::move(name_ctx)));
			}
			update_expr->GetType() = bind.m_type_data;

			Token semicolon_token(";", Token::Name::SINGLE_SEMICOLON, bind.m_name.m_line, bind.m_name.m_file_name);
			statements.emplace_back(std::make_unique<MidoriStatement>(MidoriStatement::ExpressionStatement(semicolon_token, std::move(update_expr))));
		}

		MidoriExpression::NameContext::Tag final_ctx = bind.m_name_ctx;
		std::unique_ptr<MidoriExpression> final_expr = std::make_unique<MidoriExpression>(MidoriExpression::NameAccess(bind.m_name, std::move(final_ctx)));
		final_expr->GetType() = bind.m_type_data;

		Token right_brace_token("}", Token::Name::RIGHT_BRACE, bind.m_name.m_line, bind.m_name.m_file_name);
		std::unique_ptr<MidoriExpression> block_expr = std::make_unique<MidoriExpression>(MidoriExpression::Block(right_brace_token, std::move(statements), 0, std::move(final_expr)));
		block_expr->GetType() = bind.m_type_data;

		return block_expr;
	}
}

MidoriResult::OptimizerResult SelfConcatOptimization::Optimize(MidoriProgramTree program_tree)
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

std::string_view SelfConcatOptimization::GetName() const
{
	return "SelfConcatOptimization";
}

void SelfConcatOptimization::operator()(MidoriExpression::Assignment& bind)
{
	VisitAndReplace(bind.m_value);

	std::unique_ptr<MidoriExpression> replacement = BuildSelfConcatBlock(bind);
	if (replacement)
	{
		m_pending_replacement = std::move(replacement);
	}
}
