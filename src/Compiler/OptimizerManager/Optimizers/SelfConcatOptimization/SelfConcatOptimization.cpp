#include "SelfConcatOptimization.h"

#include "Common/BuildConfig/BuildConfig.h"
#include "Common/Constant/Constant.h"
#include "Compiler/Analysis/SharedAnalysis.h"

#include <ranges>

namespace
{
	bool IsCompilerGeneratedLocal(const Token& name)
	{
		return !name.m_lexeme.empty() && name.m_lexeme.front() == INTERNAL_NAME_PREFIX;
	}

	bool IsConcatType(const std::shared_ptr<MidoriType>& type)
	{
		return type != nullptr
			&& (type->IsType<MidoriType::TextType>() || type->IsType<MidoriType::ArrayType>());
	}

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
			const bool has_statement = std::ranges::any_of
			(
				node.m_stmts,
				[this](const std::unique_ptr<MidoriStatement>& stmt)
				{
					return stmt->IsStatement<MidoriStatement::ExpressionStatement>()
						&& ContainsNameAccess(*stmt->GetStatement<MidoriStatement::ExpressionStatement>().m_expr, m_ctx, m_name);
				}
			);

			const bool has_final_expr = node.m_final_expr.has_value()
				&& ContainsNameAccess(*node.m_final_expr.value(), m_ctx, m_name);

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
		const MidoriExpression* current = MidoriAnalysis::StripRedundantGroups(&expr);
		if (current != nullptr && current->IsExpression<MidoriExpression::Binary>())
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
		expr = MidoriAnalysis::StripRedundantGroups(std::move(expr));
		if (expr != nullptr && expr->IsExpression<MidoriExpression::Binary>())
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

	bool ContainsCapturingFunction(const MidoriExpression& expr);

	struct CapturingFunctionVisitor
	{
		bool operator()(const MidoriExpression::Function& node) const
		{
			if (node.m_captured_count > 0)
			{
				return true;
			}

			return ContainsCapturingFunction(*node.m_body);
		}

		bool operator()(const MidoriExpression::As& node) const
		{
			return ContainsCapturingFunction(*node.m_expr);
		}

		bool operator()(const MidoriExpression::Binary& node) const
		{
			return ContainsCapturingFunction(*node.m_left) || ContainsCapturingFunction(*node.m_right);
		}

		bool operator()(const MidoriExpression::Group& node) const
		{
			return ContainsCapturingFunction(*node.m_expr_in);
		}

		bool operator()(const MidoriExpression::Tuple& node) const
		{
			return std::ranges::any_of
			(
				node.m_elements,
				[](const std::unique_ptr<MidoriExpression>& element)
				{
					return ContainsCapturingFunction(*element);
				}
			);
		}

		bool operator()(const MidoriExpression::UnaryPrefix& node) const
		{
			return ContainsCapturingFunction(*node.m_expr);
		}

		bool operator()(const MidoriExpression::UnarySuffix& node) const
		{
			return ContainsCapturingFunction(*node.m_expr);
		}

		bool operator()(const MidoriExpression::Assignment& node) const
		{
			return ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::AppendAssign& node) const
		{
			const bool has_struct = node.m_struct != nullptr && ContainsCapturingFunction(*node.m_struct);
			return has_struct || ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::ExtendAssign& node) const
		{
			return ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::PrependAssign& node) const
		{
			const bool has_struct = node.m_struct != nullptr && ContainsCapturingFunction(*node.m_struct);
			return has_struct || ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::CompoundAssign& node) const
		{
			const bool has_struct = node.m_struct != nullptr && ContainsCapturingFunction(*node.m_struct);
			return has_struct || ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::Call& node) const
		{
			if (ContainsCapturingFunction(*node.m_callee))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_arguments,
				[](const std::unique_ptr<MidoriExpression>& argument)
				{
					return ContainsCapturingFunction(*argument);
				}
			);
		}

		bool operator()(const MidoriExpression::Construct& node) const
		{
			return std::ranges::any_of
			(
				node.m_params,
				[](const std::unique_ptr<MidoriExpression>& param)
				{
					return ContainsCapturingFunction(*param);
				}
			);
		}

		bool operator()(const MidoriExpression::IfElse& node) const
		{
			return ContainsCapturingFunction(*node.m_condition)
				|| ContainsCapturingFunction(*node.m_true_branch)
				|| ContainsCapturingFunction(*node.m_else_branch);
		}

		bool operator()(const MidoriExpression::MemberAccess& node) const
		{
			return ContainsCapturingFunction(*node.m_struct);
		}

		bool operator()(const MidoriExpression::MemberAssignment& node) const
		{
			return ContainsCapturingFunction(*node.m_struct)
				|| ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::Array& node) const
		{
			return std::ranges::any_of
			(
				node.m_elems,
				[](const std::unique_ptr<MidoriExpression>& elem)
				{
					return ContainsCapturingFunction(*elem);
				}
			);
		}

		bool operator()(const MidoriExpression::IndexAccess& node) const
		{
			if (ContainsCapturingFunction(*node.m_arr_var))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_indices,
				[](const std::unique_ptr<MidoriExpression>& index)
				{
					return ContainsCapturingFunction(*index);
				}
			);
		}

		bool operator()(const MidoriExpression::IndexAssignment& node) const
		{
			if (ContainsCapturingFunction(*node.m_arr_var)
				|| ContainsCapturingFunction(*node.m_value))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_indices,
				[](const std::unique_ptr<MidoriExpression>& index)
				{
					return ContainsCapturingFunction(*index);
				}
			);
		}

		bool operator()(const MidoriExpression::ArrayComprehension& node) const
		{
			return ContainsCapturingFunction(*node.m_transform_expr)
				|| ContainsCapturingFunction(*node.m_range);
		}

		bool operator()(const MidoriExpression::RangeBinary& node) const
		{
			return ContainsCapturingFunction(*node.m_start) || ContainsCapturingFunction(*node.m_end);
		}

		bool operator()(const MidoriExpression::RangeTernary& node) const
		{
			return ContainsCapturingFunction(*node.m_start)
				|| ContainsCapturingFunction(*node.m_step)
				|| ContainsCapturingFunction(*node.m_end);
		}

		bool operator()(const MidoriExpression::Block& node) const
		{
			const bool has_statement = std::ranges::any_of
			(
				node.m_stmts,
				[](const std::unique_ptr<MidoriStatement>& stmt)
				{
					if (!stmt->IsStatement<MidoriStatement::ExpressionStatement>())
					{
						return false;
					}

					return ContainsCapturingFunction(*stmt->GetStatement<MidoriStatement::ExpressionStatement>().m_expr);
				}
			);

			const bool has_final_expr = node.m_final_expr.has_value()
				&& ContainsCapturingFunction(*node.m_final_expr.value());

			return has_statement || has_final_expr;
		}

		bool operator()(const MidoriExpression::Match& node) const
		{
			if (ContainsCapturingFunction(*node.m_arg_expr))
			{
				return true;
			}

			return std::ranges::any_of
			(
				node.m_cases,
				[](const std::unique_ptr<MidoriExpression>& case_expr)
				{
					return ContainsCapturingFunction(*case_expr);
				}
			);
		}

		bool operator()(const MidoriExpression::Case& node) const
		{
			return ContainsCapturingFunction(*node.m_expr);
		}

		bool operator()(const MidoriExpression::Default& node) const
		{
			return ContainsCapturingFunction(*node.m_expr);
		}

		bool operator()(const MidoriExpression::Loop& node) const
		{
			return ContainsCapturingFunction(*node.m_body);
		}

		bool operator()(const MidoriExpression::For& node) const
		{
			return ContainsCapturingFunction(*node.m_range) || ContainsCapturingFunction(*node.m_body);
		}

		bool operator()(const MidoriExpression::Return& node) const
		{
			return ContainsCapturingFunction(*node.m_value);
		}

		bool operator()(const MidoriExpression::Break& node) const
		{
			return ContainsCapturingFunction(*node.m_value);
		}

		template <typename T>
		bool operator()(const T&) const
		{
			return false;
		}
	};

	bool ContainsCapturingFunction(const MidoriExpression& expr)
	{
		return std::visit(CapturingFunctionVisitor{}, *expr);
	}

	bool IsFreshValue(const MidoriExpression& expr)
	{
		const MidoriExpression* stripped_expr = MidoriAnalysis::StripRedundantGroups(&expr);
		if (stripped_expr == nullptr)
		{
			return false;
		}

		if (stripped_expr->IsExpression<MidoriExpression::TextLiteral>()
			|| stripped_expr->IsExpression<MidoriExpression::Array>()
			|| stripped_expr->IsExpression<MidoriExpression::ArrayComprehension>())
		{
			return true;
		}

		if (!stripped_expr->IsExpression<MidoriExpression::Binary>())
		{
			return false;
		}

		const MidoriExpression::Binary& binary = stripped_expr->GetExpression<MidoriExpression::Binary>();
		return binary.m_op.m_token_name == Token::Name::DOUBLE_PLUS && IsConcatType(binary.m_type_data);
	}

	std::unique_ptr<MidoriExpression> BuildConcatMutationExpr(const Token& name, MidoriExpression::NameContext::Tag&& name_ctx, std::unique_ptr<MidoriExpression> operand, const std::shared_ptr<MidoriType>& target_type)
	{
		if (!IsConcatType(target_type))
		{
			return nullptr;
		}

		operand = MidoriAnalysis::StripRedundantGroups(std::move(operand));
		if (operand == nullptr)
		{
			return nullptr;
		}

		if (target_type->IsType<MidoriType::TextType>())
		{
			std::unique_ptr<MidoriExpression> update_expr = std::make_unique<MidoriExpression>(MidoriExpression::AppendAssign(name, std::move(operand), std::move(name_ctx)));
			update_expr->GetType() = target_type;
			return update_expr;
		}

		if (operand->IsExpression<MidoriExpression::Array>())
		{
			MidoriExpression::Array& array_literal = operand->GetExpression<MidoriExpression::Array>();
			if (array_literal.m_elems.size() == 1u)
			{
				std::unique_ptr<MidoriExpression> element = std::move(array_literal.m_elems[0u]);
				std::unique_ptr<MidoriExpression> update_expr = std::make_unique<MidoriExpression>(MidoriExpression::AppendAssign(name, std::move(element), std::move(name_ctx)));
				update_expr->GetType() = target_type;
				return update_expr;
			}
		}

		std::unique_ptr<MidoriExpression> update_expr = std::make_unique<MidoriExpression>(MidoriExpression::ExtendAssign(name, std::move(operand), std::move(name_ctx)));
		update_expr->GetType() = target_type;
		return update_expr;
	}

	std::unique_ptr<MidoriExpression> BuildPrependMutationExpr(const Token& name, MidoriExpression::NameContext::Tag&& name_ctx, std::unique_ptr<MidoriExpression> operand, const std::shared_ptr<MidoriType>& target_type)
	{
		if (!IsConcatType(target_type))
		{
			return nullptr;
		}

		operand = MidoriAnalysis::StripRedundantGroups(std::move(operand));
		if (operand == nullptr)
		{
			return nullptr;
		}

		if (target_type->IsType<MidoriType::TextType>())
		{
			std::unique_ptr<MidoriExpression> update_expr = std::make_unique<MidoriExpression>(MidoriExpression::PrependAssign(name, std::move(operand), std::move(name_ctx)));
			update_expr->GetType() = target_type;
			return update_expr;
		}

		if (!operand->IsExpression<MidoriExpression::Array>())
		{
			return nullptr;
		}

		MidoriExpression::Array& array_literal = operand->GetExpression<MidoriExpression::Array>();
		if (array_literal.m_elems.size() != 1u)
		{
			return nullptr;
		}

		std::unique_ptr<MidoriExpression> element = std::move(array_literal.m_elems[0u]);
		std::unique_ptr<MidoriExpression> update_expr = std::make_unique<MidoriExpression>(MidoriExpression::PrependAssign(name, std::move(element), std::move(name_ctx)));
		update_expr->GetType() = target_type;
		return update_expr;
	}

	std::unique_ptr<MidoriExpression> BuildSuffixConcatBlock(MidoriExpression::Assignment& bind)
	{
		std::vector<const MidoriExpression*> operands;
		CollectConcatOperands(*bind.m_value, operands);
		if (operands.size() < 2u)
		{
			return nullptr;
		}

		const MidoriExpression* first_expr = MidoriAnalysis::StripRedundantGroups(operands[0u]);
		if (first_expr == nullptr || !first_expr->IsExpression<MidoriExpression::NameAccess>())
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
			std::unique_ptr<MidoriExpression> update_expr = BuildConcatMutationExpr(bind.m_name, std::move(name_ctx), std::move(operand), bind.m_type_data);
			if (update_expr == nullptr)
			{
				return nullptr;
			}

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

	std::unique_ptr<MidoriExpression> BuildPrefixConcatBlock(MidoriExpression::Assignment& bind)
	{
		std::vector<const MidoriExpression*> operands;
		CollectConcatOperands(*bind.m_value, operands);
		if (operands.size() < 2u)
		{
			return nullptr;
		}

		const MidoriExpression* last_expr = MidoriAnalysis::StripRedundantGroups(operands.back());
		if (last_expr == nullptr || !last_expr->IsExpression<MidoriExpression::NameAccess>())
		{
			return nullptr;
		}

		const MidoriExpression::NameAccess& name_access = last_expr->GetExpression<MidoriExpression::NameAccess>();
		if (!IsSameNameAccess(name_access, bind.m_name_ctx, bind.m_name))
		{
			return nullptr;
		}

		for (size_t idx = 0uz; idx + 1uz < operands.size(); idx += 1uz)
		{
			if (ContainsNameAccess(*operands[idx], bind.m_name_ctx, bind.m_name))
			{
				return nullptr;
			}

			if (!MidoriAnalysis::IsPure(*operands[idx]))
			{
				return nullptr;
			}

			if (bind.m_type_data->IsType<MidoriType::ArrayType>())
			{
				const MidoriExpression* stripped_operand = MidoriAnalysis::StripRedundantGroups(operands[idx]);
				if (stripped_operand == nullptr || !stripped_operand->IsExpression<MidoriExpression::Array>())
				{
					return nullptr;
				}

				const MidoriExpression::Array& array_literal = stripped_operand->GetExpression<MidoriExpression::Array>();
				if (array_literal.m_elems.size() != 1u)
				{
					return nullptr;
				}
			}
		}

		std::vector<std::unique_ptr<MidoriExpression>> owned_operands;
		CollectConcatOperands(std::move(bind.m_value), owned_operands);
		if (owned_operands.size() <= 1uz)
		{
			return nullptr;
		}

		owned_operands.pop_back();

		std::vector<std::unique_ptr<MidoriStatement>> statements;
		statements.reserve(owned_operands.size());

		for (auto it = owned_operands.rbegin(); it != owned_operands.rend(); it += 1)
		{
			MidoriExpression::NameContext::Tag name_ctx = bind.m_name_ctx;
			std::unique_ptr<MidoriExpression> update_expr = BuildPrependMutationExpr(bind.m_name, std::move(name_ctx), std::move(*it), bind.m_type_data);
			if (update_expr == nullptr)
			{
				return nullptr;
			}

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
	ResetPassState();
	m_unique_locals_stack.clear();
	m_local_scope_stack.clear();

	PushFunctionState();
	PushLocalScope();

	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			VisitStatement(stmt);
		}
	);

	PopLocalScope();
	PopFunctionState();

	return std::move(program_tree);
}

std::string_view SelfConcatOptimization::GetName() const
{
	return "SelfConcatOptimization";
}

void SelfConcatOptimization::PushFunctionState()
{
	m_unique_locals_stack.emplace_back();
}

void SelfConcatOptimization::PopFunctionState()
{
	m_unique_locals_stack.pop_back();
}

void SelfConcatOptimization::PushLocalScope()
{
	m_local_scope_stack.emplace_back();
}

void SelfConcatOptimization::PopLocalScope()
{
	std::unordered_map<int, UniqueLocalInfo>& unique_locals = CurrentUniqueLocals();
	for (const int local_index : m_local_scope_stack.back())
	{
		unique_locals.erase(local_index);
	}

	m_local_scope_stack.pop_back();
}

std::unordered_map<int, SelfConcatOptimization::UniqueLocalInfo>& SelfConcatOptimization::CurrentUniqueLocals()
{
	return m_unique_locals_stack.back();
}

const std::unordered_map<int, SelfConcatOptimization::UniqueLocalInfo>& SelfConcatOptimization::CurrentUniqueLocals() const
{
	return m_unique_locals_stack.back();
}

void SelfConcatOptimization::InvalidateUniqueLocal(int local_index)
{
	CurrentUniqueLocals().erase(local_index);
}

void SelfConcatOptimization::InvalidateAllUniqueLocals()
{
	CurrentUniqueLocals().clear();
}

void SelfConcatOptimization::RefreshUniqueLocal(int local_index, const Token& name, const std::shared_ptr<MidoriType>& type)
{
	CurrentUniqueLocals().insert_or_assign(local_index, UniqueLocalInfo{ name, type });
}

SelfConcatOptimization::RootRewriteEffect SelfConcatOptimization::TryRewriteRoot(std::unique_ptr<MidoriExpression>& expr, RootContext context)
{
	RootRewriteEffect effect;

	expr = MidoriAnalysis::StripRedundantGroups(std::move(expr));
	if (expr == nullptr)
	{
		return effect;
	}

	const MidoriAnalysis::StatementLocalAccessSummary access_summary = MidoriAnalysis::AnalyzeExpressionLocalAccess(*expr);
	if (access_summary.m_has_nested_callable_boundary && ContainsCapturingFunction(*expr))
	{
		return effect;
	}

	if (expr->IsExpression<MidoriExpression::CompoundAssign>())
	{
		MidoriExpression::CompoundAssign& compound_assign = expr->GetExpression<MidoriExpression::CompoundAssign>();
		if (compound_assign.m_struct != nullptr || compound_assign.m_op.m_token_name != Token::Name::PLUS_PLUS_EQUAL)
		{
			return effect;
		}

		const std::optional<int> local_index = MidoriAnalysis::TryGetLocalIndex(compound_assign.m_name_ctx);
		if (!local_index.has_value() || !IsConcatType(compound_assign.m_type_data))
		{
			return effect;
		}

		const bool is_compiler_generated = IsCompilerGeneratedLocal(compound_assign.m_name);
		if (!is_compiler_generated && !CurrentUniqueLocals().contains(local_index.value()))
		{
			return effect;
		}

		MidoriExpression::NameContext::Tag name_ctx = compound_assign.m_name_ctx;
		std::unique_ptr<MidoriExpression> replacement = BuildConcatMutationExpr(compound_assign.m_name, std::move(name_ctx), std::move(compound_assign.m_value), compound_assign.m_type_data);
		if (replacement == nullptr)
		{
			return effect;
		}

		Replace(std::move(replacement), expr);
		if (!is_compiler_generated && context == RootContext::Discarded)
		{
			effect.m_preserved_local = local_index;
		}

		return effect;
	}

	if (!expr->IsExpression<MidoriExpression::Assignment>())
	{
		return effect;
	}

	MidoriExpression::Assignment& bind = expr->GetExpression<MidoriExpression::Assignment>();
	const std::optional<int> local_index = MidoriAnalysis::TryGetLocalIndex(bind.m_name_ctx);
	if (!local_index.has_value() || !IsConcatType(bind.m_type_data))
	{
		return effect;
	}

	const bool is_compiler_generated = IsCompilerGeneratedLocal(bind.m_name);
	if (!is_compiler_generated && !CurrentUniqueLocals().contains(local_index.value()))
	{
		return effect;
	}

	std::unique_ptr<MidoriExpression> replacement = BuildSuffixConcatBlock(bind);
	if (replacement == nullptr)
	{
		replacement = BuildPrefixConcatBlock(bind);
	}
	if (replacement == nullptr)
	{
		return effect;
	}

	Replace(std::move(replacement), expr);
	if (!is_compiler_generated && context == RootContext::Discarded)
	{
		effect.m_preserved_local = local_index;
	}

	return effect;
}

SelfConcatOptimization::RootRewriteEffect SelfConcatOptimization::AnalyzeRootEffects(const MidoriExpression& expr, RootContext context) const
{
	RootRewriteEffect effect;
	if (context != RootContext::Discarded)
	{
		return effect;
	}

	const MidoriExpression* stripped_expr = MidoriAnalysis::StripRedundantGroups(&expr);
	if (stripped_expr == nullptr || !stripped_expr->IsExpression<MidoriExpression::Assignment>())
	{
		return effect;
	}

	const MidoriExpression::Assignment& bind = stripped_expr->GetExpression<MidoriExpression::Assignment>();
	const std::optional<int> local_index = MidoriAnalysis::TryGetLocalIndex(bind.m_name_ctx);
	if (!local_index.has_value() || !IsConcatType(bind.m_type_data) || !IsFreshValue(*bind.m_value))
	{
		return effect;
	}

	effect.m_refreshed_local = local_index;
	effect.m_refreshed_info = UniqueLocalInfo{ bind.m_name, bind.m_type_data };
	return effect;
}

void SelfConcatOptimization::ApplyExpressionEffects(const MidoriExpression& expr, const RootRewriteEffect& effect)
{
	const MidoriAnalysis::StatementLocalAccessSummary access_summary = MidoriAnalysis::AnalyzeExpressionLocalAccess(expr);
	if (access_summary.m_has_nested_callable_boundary && ContainsCapturingFunction(expr))
	{
		InvalidateAllUniqueLocals();
	}
	else
	{
		std::vector<int> to_invalidate;
		for (const auto& [local_index, info] : CurrentUniqueLocals())
		{
			static_cast<void>(info);

			if (effect.m_preserved_local.has_value() && effect.m_preserved_local.value() == local_index)
			{
				continue;
			}

			if (access_summary.UsesLocal(local_index) || access_summary.AssignsLocal(local_index))
			{
				to_invalidate.emplace_back(local_index);
			}
		}

		for (const int local_index : to_invalidate)
		{
			InvalidateUniqueLocal(local_index);
		}
	}

	if (effect.m_refreshed_local.has_value() && effect.m_refreshed_info.has_value())
	{
		RefreshUniqueLocal
		(
			effect.m_refreshed_local.value(),
			effect.m_refreshed_info->m_name,
			effect.m_refreshed_info->m_type
		);
	}
}

void SelfConcatOptimization::OptimizeRootExpression(std::unique_ptr<MidoriExpression>& expr, RootContext context)
{
	VisitAndReplace(expr);

	RootRewriteEffect effect = TryRewriteRoot(expr, context);
	RootRewriteEffect analyzed_effect = AnalyzeRootEffects(*expr, context);
	if (!effect.m_refreshed_local.has_value() && analyzed_effect.m_refreshed_local.has_value())
	{
		effect.m_refreshed_local = analyzed_effect.m_refreshed_local;
		effect.m_refreshed_info = analyzed_effect.m_refreshed_info;
	}

	ApplyExpressionEffects(*expr, effect);
}

void SelfConcatOptimization::TrackFreshDefinition(const MidoriStatement::VariableDefinition& def)
{
	if (def.m_is_elided || !def.m_local_index.has_value())
	{
		return;
	}

	if (!IsConcatType(def.m_value->GetType()) || !IsFreshValue(*def.m_value))
	{
		return;
	}

	RefreshUniqueLocal(def.m_local_index.value(), def.m_name, def.m_value->GetType());
	m_local_scope_stack.back().emplace_back(def.m_local_index.value());
}

void SelfConcatOptimization::operator()(MidoriStatement::ExpressionStatement& simple)
{
	OptimizeRootExpression(simple.m_expr, RootContext::Discarded);
}

void SelfConcatOptimization::operator()(MidoriStatement::VariableDefinition& def)
{
	if (def.m_is_elided)
	{
		return;
	}

	OptimizeRootExpression(def.m_value, RootContext::Escaping);
	TrackFreshDefinition(def);
}

void SelfConcatOptimization::operator()(MidoriStatement::TupleDefinition& def_tuple)
{
	OptimizeRootExpression(def_tuple.m_value, RootContext::Escaping);
}

void SelfConcatOptimization::operator()(MidoriStatement::FunctionDefinition& defun)
{
	if (defun.m_captured_count > 0)
	{
		InvalidateAllUniqueLocals();
	}

	PushFunctionState();
	PushLocalScope();
	OptimizeRootExpression(defun.m_body, RootContext::Terminal);
	PopLocalScope();
	PopFunctionState();
}

void SelfConcatOptimization::operator()(MidoriExpression::Block& block)
{
	PushLocalScope();

	for (std::unique_ptr<MidoriStatement>& statement : block.m_stmts)
	{
		VisitStatement(statement);
	}

	if (block.m_final_expr.has_value())
	{
		OptimizeRootExpression(block.m_final_expr.value(), RootContext::Terminal);
	}

	PopLocalScope();
}

void SelfConcatOptimization::operator()(MidoriExpression::Function& function)
{
	if (function.m_captured_count > 0)
	{
		InvalidateAllUniqueLocals();
	}

	PushFunctionState();
	PushLocalScope();
	OptimizeRootExpression(function.m_body, RootContext::Terminal);
	PopLocalScope();
	PopFunctionState();
}

void SelfConcatOptimization::operator()(MidoriExpression::Return& return_expr)
{
	OptimizeRootExpression(return_expr.m_value, RootContext::Terminal);
}

void SelfConcatOptimization::operator()(MidoriExpression::Break& break_expr)
{
	OptimizeRootExpression(break_expr.m_value, RootContext::Terminal);
}
