#include <algorithm>
#include <format>
#include <iterator>
#include <ranges>

#include "Common/Error/Error.h"
#include "TypeChecker.h"

void TypeChecker::Unify(std::shared_ptr<MidoriType>& left, std::shared_ptr<MidoriType>& right)
{
	if (left->IsType<MidoriType::UndecidedType>() && !right->IsType<MidoriType::UndecidedType>())
	{
		*left = *right;
		return;
	}
	else if (!left->IsType<MidoriType::UndecidedType>() && right->IsType<MidoriType::UndecidedType>())
	{
		*right = *left;
		return;
	}
	else
	{
		if (left->IsType<MidoriType::ArrayType>() && right->IsType<MidoriType::ArrayType>())
		{
			Unify(left->GetType<MidoriType::ArrayType>().m_element_type, right->GetType<MidoriType::ArrayType>().m_element_type);
		}
		else if (left->IsType<MidoriType::FunctionType>() && right->IsType<MidoriType::FunctionType>())
		{
			MidoriType::FunctionType& left_func = left->GetType<MidoriType::FunctionType>();
			MidoriType::FunctionType& right_func = right->GetType<MidoriType::FunctionType>();
			std::ranges::for_each
			(
				std::views::iota(0u, left_func.m_param_types.size()), 
				[&left_func, &right_func, this](size_t idx) { Unify(left_func.m_param_types[idx], right_func.m_param_types[idx]); }
			);
			Unify(left_func.m_return_type, right_func.m_return_type);
		}
	}
}

void TypeChecker::AddError(std::string&& error)
{
	m_errors.append(std::move(error)).append("\n");
}

void TypeChecker::BeginScope()
{
	m_name_type_table.emplace_back();
}

void TypeChecker::EndScope()
{
	m_name_type_table.pop_back();
}

void TypeChecker::UpdateConditionOperandType(MidoriExpression::ConditionOperandType& op_type, const std::unique_ptr<MidoriExpression>& expr)
{
	if (expr->IsExpression<MidoriExpression::Binary>())
	{
		const MidoriExpression::Binary& binary = expr->GetExpression<MidoriExpression::Binary>();
		const std::shared_ptr<MidoriType>& left_type = binary.m_left->GetType();

		if (left_type->IsType<MidoriType::IntegerType>())
		{
			op_type = MidoriExpression::ConditionOperandType::INTEGER;
		}
		else if (left_type->IsType<MidoriType::FloatType>())
		{
			op_type = MidoriExpression::ConditionOperandType::FLOAT;
		}
		else
		{
			op_type = MidoriExpression::ConditionOperandType::OTHER;
		}
	}
}

MidoriResult::TypeCheckerResult TypeChecker::TypeCheck(MidoriProgramTree&& program_tree)
{
	BeginScope();
	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, *statement);
		}
	);
	EndScope();

	if (m_errors.empty())
	{
		return program_tree;
	}
	else
	{
		return std::unexpected<std::string>(std::move(m_errors));
	}
}

void TypeChecker::operator()(Block& block)
{
	BeginScope();
	std::ranges::for_each
	(
		block.m_stmts,
		[this](std::unique_ptr<MidoriStatement>& statement) { std::visit([this](auto&& arg) { (*this)(arg); }, *statement); }
	);
	EndScope();
}

void TypeChecker::operator()(Simple& simple)
{
	MidoriResult::TypeResult result = std::visit
	(
		[this](auto&& arg) -> MidoriResult::TypeResult
		{
			return (*this)(arg);
		},
		**simple.m_expr
	);

	if (!result.has_value())
	{
		AddError(std::move(result.error()));
	}
}

void TypeChecker::operator()(Define& def)
{
	if (def.m_value->IsExpression<MidoriExpression::Function>())
	{
		MidoriExpression::Function& function = def.m_value->GetExpression<MidoriExpression::Function>();
		def.m_value->GetType() = MidoriType::MakeFunctionType(std::move(function.m_param_types), std::move(function.m_return_type), false);
		m_name_type_table.back().emplace(def.m_name.m_lexeme, def.m_value->GetType());
		MidoriType::FunctionType& function_type = def.m_value->GetType()->GetType<MidoriType::FunctionType>();

		std::shared_ptr<MidoriType>& actual_type = m_name_type_table.back()[def.m_name.m_lexeme];

		BeginScope();
		std::ranges::for_each
		(
			std::views::iota(0u, function_type.m_param_types.size()), 
			[&function, &function_type, this](size_t idx) { m_name_type_table.back().emplace(function.m_params[idx].m_lexeme, function_type.m_param_types[idx]); }
		);

		if (def.m_annotated_type.has_value())
		{
			std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
			if (*annotated_type != *actual_type)
			{
				AddError(MidoriError::GenerateTypeCheckerError("Define statement type error", def.m_name, actual_type, annotated_type));
				return;
			}
		}

		MidoriResult::TypeResult function_type_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value);
		if (!function_type_result.has_value())
		{
			AddError(std::move(function_type_result.error()));
			return;
		}

		EndScope();
		return;
	}
	else if (def.m_value->IsExpression<MidoriExpression::Construct>())
	{
		MidoriResult::TypeResult construct_type_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value);
		if (!construct_type_result.has_value())
		{
			AddError(std::move(construct_type_result.error()));
			return;
		}
		std::shared_ptr<MidoriType>& actual_type = construct_type_result.value();

		if (def.m_annotated_type.has_value())
		{
			std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
			if (*annotated_type != *actual_type)
			{
				AddError(MidoriError::GenerateTypeCheckerError("Define statement type error", def.m_name, actual_type, annotated_type));
				return;
			}
			m_name_type_table.back().emplace(def.m_name.m_lexeme, annotated_type);
		}
		else
		{
			m_name_type_table.back().emplace(def.m_name.m_lexeme, actual_type);
		}

		return;
	}
	else if (def.m_value->IsExpression<MidoriExpression::Array>())
	{
		MidoriResult::TypeResult init_expr_type = std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value);
		if (!init_expr_type.has_value())
		{
			AddError(std::move(init_expr_type.error()));
			return;
		}

		if (def.m_annotated_type.has_value())
		{
			std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
			std::shared_ptr<MidoriType>& actual_type = init_expr_type.value();
			Unify(annotated_type, actual_type);

			if (*annotated_type != *actual_type)
			{
				AddError(MidoriError::GenerateTypeCheckerError("Define statement type error", def.m_name, actual_type, annotated_type));
				return;
			}
		}

		m_name_type_table.back().emplace(def.m_name.m_lexeme, init_expr_type.value());
	}
	else
	{
		MidoriResult::TypeResult init_expr_type = std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value);
		if (!init_expr_type.has_value())
		{
			AddError(std::move(init_expr_type.error()));
			return;
		}

		if (def.m_annotated_type.has_value())
		{
			std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
			std::shared_ptr<MidoriType>& actual_type = init_expr_type.value();
			Unify(annotated_type, actual_type);

			if (*def.m_annotated_type.value() != *init_expr_type.value())
			{
				AddError(MidoriError::GenerateTypeCheckerError("Define statement type error", def.m_name, actual_type, annotated_type));
				return;
			}

			m_name_type_table.back().emplace(def.m_name.m_lexeme, annotated_type);
		}

		m_name_type_table.back().emplace(def.m_name.m_lexeme, init_expr_type.value());
	}
}

void TypeChecker::operator()(If& if_stmt)
{
	MidoriResult::TypeResult condition_type = std::visit([this](auto&& arg) { return (*this)(arg); }, **if_stmt.m_condition);
	if (condition_type.has_value())
	{
		if (!condition_type.value()->IsType<MidoriType::BoolType>())
		{
			std::shared_ptr<MidoriType>& actual_type = condition_type.value();
			AddError(MidoriError::GenerateTypeCheckerError("If statement condition must be of type bool.", if_stmt.m_if_keyword, actual_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
		}

		UpdateConditionOperandType(if_stmt.m_condition_operand_type, if_stmt.m_condition);
	}
	else
	{
		AddError(std::move(condition_type.error()));
	}

	std::visit([this](auto&& arg) { (*this)(arg); }, *if_stmt.m_true_branch);

	if (if_stmt.m_else_branch.has_value())
	{
		std::visit([this](auto&& arg) { (*this)(arg); }, *(*if_stmt.m_else_branch));
	}
}

void TypeChecker::operator()(While& while_stmt)
{
	MidoriResult::TypeResult condition_type = std::visit([this](auto&& arg) { return (*this)(arg); }, **while_stmt.m_condition);
	if (condition_type.has_value())
	{
		std::shared_ptr<MidoriType>& actual_type = condition_type.value();
		if (!actual_type->IsType<MidoriType::BoolType>())
		{
			AddError(MidoriError::GenerateTypeCheckerError("While statement condition must be of type bool.", while_stmt.m_loop_keyword, actual_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
			return;
		}
	}
	else
	{
		AddError(std::move(condition_type.error()));
		return;
	}

	std::visit([this](auto&& arg) { (*this)(arg); }, *while_stmt.m_body);
}

void TypeChecker::operator()(For& for_stmt)
{
	BeginScope();
	std::visit([this](auto&& arg) { (*this)(arg); }, *for_stmt.m_condition_intializer);

	MidoriResult::TypeResult condition_type = std::visit([this](auto&& arg) { return (*this)(arg); }, **for_stmt.m_condition);
	if (condition_type.has_value())
	{
		std::shared_ptr<MidoriType>& actual_type = condition_type.value();
		if (!actual_type->IsType<MidoriType::BoolType>())
		{
			AddError(MidoriError::GenerateTypeCheckerError("For statement condition must be of type bool.", for_stmt.m_loop_keyword, actual_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
			return;
		}
	}
	else
	{
		AddError(std::move(condition_type.error()));
		return;
	}

	std::visit([this](auto&& arg) { (*this)(arg); }, *for_stmt.m_condition_incrementer);

	std::visit([this](auto&& arg) { (*this)(arg); }, *for_stmt.m_body);
	EndScope();
}

void TypeChecker::operator()(Break&)
{
	return;
}

void TypeChecker::operator()(Continue&)
{
	return;
}

void TypeChecker::operator()(Return& return_stmt)
{
	MidoriResult::TypeResult return_type = std::visit([this](auto&& arg) { return (*this)(arg); }, **return_stmt.m_value);

	if (!return_type.has_value())
	{
		AddError(std::move(return_type.error()));
		return;
	}

	std::shared_ptr<MidoriType>& actual_type = return_type.value();
	std::shared_ptr<MidoriType> expected_type = m_curr_function_return_type.lock();
	Unify(actual_type, expected_type);

	if (*actual_type != *expected_type)
	{
		AddError(MidoriError::GenerateTypeCheckerError("Return statement expression type error.", return_stmt.m_keyword, actual_type, expected_type));
		return;
	}
}

void TypeChecker::operator()(Foreign& foreign)
{
	m_name_type_table.back()[foreign.m_function_name.m_lexeme] = foreign.m_type;
}

void TypeChecker::operator()(Struct& struct_stmt)
{
	std::shared_ptr<MidoriType> struct_constructor_type = MidoriType::MakeFunctionType(struct_stmt.m_self_type->GetType<MidoriType::StructType>().m_member_types, std::move(struct_stmt.m_self_type));
	m_name_type_table.back()[struct_stmt.m_name.m_lexeme] = struct_constructor_type;
}

void TypeChecker::operator()(Union& union_stmt)
{
	MidoriType::UnionType& union_type = union_stmt.m_self_type->GetType<MidoriType::UnionType>();
	for (auto& [member_name, member_ctx] : union_type.m_member_info)
	{
		std::shared_ptr<MidoriType> union_constructor_type = MidoriType::MakeFunctionType(std::move(member_ctx.m_member_types), std::shared_ptr(union_stmt.m_self_type));
		m_name_type_table.back()[member_name] = union_constructor_type;
	}
}

void TypeChecker::operator()(Switch& switch_stmt)
{
	MidoriResult::TypeResult switch_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **switch_stmt.m_arg_expr);
	if (!switch_result.has_value())
	{
		AddError(std::move(switch_result.error()));
		return;
	}

	std::shared_ptr<MidoriType>& switch_type = switch_result.value();
	if (!switch_type->IsType<MidoriType::UnionType>())
	{
		std::shared_ptr<MidoriType>& actual_type = switch_type;
		AddError(MidoriError::GenerateTypeCheckerError("Switch statement type error", switch_stmt.m_switch_keyword, {}, actual_type));
		return;
	}

	const MidoriType::UnionType& union_type = switch_type->GetType<MidoriType::UnionType>();

	auto key_view = union_type.m_member_info | std::views::transform([](const auto& pair) { return pair.first; });
	std::unordered_set<std::string> expected_member_names(key_view.begin(), key_view.end());
	bool has_default_case = false;

	for (Switch::Case& branch : switch_stmt.m_cases)
	{
		BeginScope();

		if (Switch::IsDefaultCase(branch))
		{
			has_default_case = true;
		}
		else
		{
			if (has_default_case)
			{
				// TODO: Add warning for unreachable code
			}

			Switch::MemberCase& member_case = Switch::GetMemberCase(branch);
			const std::string& branch_name = member_case.m_member_name;
			if (!expected_member_names.contains(branch_name))
			{
				AddError(MidoriError::GenerateTypeCheckerError("Switch statement type error: unrecognized member", Switch::GetKeyword(branch).m_line));
				return;
			}
			else
			{
				if (member_case.m_binding_names.size() != union_type.m_member_info.at(branch_name).m_member_types.size())
				{
					AddError(MidoriError::GenerateTypeCheckerError("Switch statement type error: incorrect case arity", Switch::GetKeyword(branch).m_line));
					return;
				}

				member_case.m_tag = union_type.m_member_info.at(branch_name).m_tag;

				expected_member_names.erase(branch_name);
				std::ranges::for_each
				(
					std::views::iota(0u, member_case.m_binding_names.size()),
					[&member_case, &union_type, &branch_name, this](size_t idx)
					{
						const std::string& binding_name = member_case.m_binding_names[idx];
						m_name_type_table.back()[binding_name] = union_type.m_member_info.at(branch_name).m_member_types[idx];
					}
				);
			}
		}

		std::visit([this](auto&& arg) { return (*this)(arg); }, *Switch::GetCaseStatement(branch));

		EndScope();
	}

	if (!expected_member_names.empty() && !has_default_case)
	{
		AddError(MidoriError::GenerateTypeCheckerError("Not all union members are matched", switch_stmt.m_switch_keyword.m_line));
		return;
	}
}

void TypeChecker::operator()(Namespace& namespace_stmt)
{
	std::ranges::for_each
	(
		namespace_stmt.m_stmts,
		[this](const std::unique_ptr<MidoriStatement>& stmt) { std::visit([this](auto&& arg) { return (*this)(arg); }, *stmt); }
	);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::As& as)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **as.m_expr)
		.and_then
		(
			[&as](std::shared_ptr<MidoriType>&& expr_type) ->MidoriResult::TypeResult
			{
				if (as.m_to_type->IsType<MidoriType::StructType>() && expr_type->IsType<MidoriType::StructType>())
				{
					const MidoriType::StructType& from_struct_type = expr_type->GetType<MidoriType::StructType>();
					const MidoriType::StructType& to_struct_type = as.m_to_type->GetType<MidoriType::StructType>();
					if (to_struct_type.m_member_types.size() != from_struct_type.m_member_types.size())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Type cast expression type error.", as.m_as_keyword, {}, as.m_to_type));
					}

					for (size_t i : std::views::iota(0u, to_struct_type.m_member_types.size()))
					{
						if (*from_struct_type.m_member_types[i] != *to_struct_type.m_member_types[i])
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Type cast expression type error.", as.m_as_keyword, {}, as.m_to_type));
						}
					}
				}
				else if (as.m_to_type->IsType<MidoriType::StructType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Type cast expression type error.", as.m_as_keyword, {}, as.m_to_type));
				}

				as.m_from_type = expr_type;
				as.m_type_data = as.m_to_type;
				return as.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Binary& binary)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **binary.m_left)
		.and_then
		(
			[&binary, this](std::shared_ptr<MidoriType>&& left_type) ->MidoriResult::TypeResult
			{
				return std::visit([this](auto&& arg) { return (*this)(arg); }, **binary.m_right)
					.and_then
					(
						[&left_type, &binary, this](std::shared_ptr<MidoriType>&& right_type) ->MidoriResult::TypeResult
						{
							Unify(left_type, right_type);
							std::shared_ptr<MidoriType>& self_type = binary.m_type_data;

							// Handle array operations
							if (binary.m_op.m_token_name == Token::Name::STAR && left_type->IsType<MidoriType::ArrayType>() && right_type->IsType<MidoriType::IntegerType>())
							{
								self_type = left_type;
								return self_type;
							}

							if (*left_type != *right_type)
							{
								std::string left_type_str = left_type->ToString();
								std::string right_type_str = right_type->ToString();
								std::string error_message = std::format("Binary expression type error: left type is {}, right type is {}", left_type_str, right_type_str);
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError(error_message, binary.m_op, left_type, right_type));
							}

							self_type = left_type;

							if (std::ranges::contains(m_binary_partial_order_comparison_operators.cbegin(), m_binary_partial_order_comparison_operators.cend(), binary.m_op.m_token_name))
							{
								if (!self_type->IsNumericType())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error", binary.m_op, self_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
								}

								self_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
							}
							else if (std::ranges::contains(m_binary_arithmetic_operators.cbegin(), m_binary_arithmetic_operators.cend(), binary.m_op.m_token_name))
							{
								if (!self_type->IsNumericType())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error", binary.m_op, self_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
								}
							}
							else if (std::ranges::contains(m_binary_bitwise_operators.cbegin(), m_binary_bitwise_operators.cend(), binary.m_op.m_token_name))
							{
								if (!self_type->IsType<MidoriType::IntegerType>())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error", binary.m_op, self_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
								}
							}
							else if (std::ranges::contains(m_binary_equality_operators.cbegin(), m_binary_equality_operators.cend(), binary.m_op.m_token_name))
							{
								if (!self_type->IsNumericType() && !self_type->IsType<MidoriType::TextType>())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error", binary.m_op, self_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>(), MidoriType::MakeLiteralType<MidoriType::TextType>()));
								}

								self_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
							}
							else if (std::ranges::contains(m_binary_logical_operators.cbegin(), m_binary_logical_operators.cend(), binary.m_op.m_token_name))
							{
								if (!left_type->IsType<MidoriType::BoolType>())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error", binary.m_op, self_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
								}

								self_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
							}
							else if (std::ranges::contains(m_binary_concatenation_operators.cbegin(), m_binary_concatenation_operators.cend(), binary.m_op.m_token_name))
							{
								if (!left_type->IsType<MidoriType::TextType>() && !left_type->IsType<MidoriType::ArrayType>())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error: expected array or text", binary.m_op, self_type));
								}
							}

							return self_type;
						}
					);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Group& group)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **group.m_expr_in)
		.and_then
		(
			[&group](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				group.m_type_data = std::move(actual_type);
				return group.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::UnaryPrefix& unary)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **unary.m_expr)
		.and_then
		(
			[&unary](std::shared_ptr<MidoriType>&& actual_type) -> MidoriResult::TypeResult
			{
				if (unary.m_op.m_token_name == Token::Name::AT)
				{
					if (!actual_type->IsType<MidoriType::ArrayType>())
					{
						// TODO: Generic array type
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Unary prefix expression type error", unary.m_op, {}, actual_type));
					}
				}
				else if (unary.m_op.m_token_name == Token::Name::SINGLE_MINUS || unary.m_op.m_token_name == Token::Name::SINGLE_PLUS)
				{
					if (!actual_type->IsNumericType())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Unary prefix expression type error", unary.m_op, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
					}
				}
				else if (unary.m_op.m_token_name == Token::Name::BANG)
				{
					if (!actual_type->IsType<MidoriType::BoolType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Unary prefix expression type error", unary.m_op, actual_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
					}
				}
				else if (unary.m_op.m_token_name == Token::Name::TILDE)
				{
					if (!actual_type->IsType<MidoriType::IntegerType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Unary prefix expression type error", unary.m_op, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
					}
				}

				unary.m_type_data = std::move(actual_type);
				return unary.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::UnarySuffix&)
{
	// TODO: Not yet implemented, no suffix operators at the moment
	return {};
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Call& call)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **call.m_callee)
		.and_then
		(
			[&call, this](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				if (!actual_type->IsType<MidoriType::FunctionType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Call expression type error: not a callable", call.m_paren.m_line));
				}

				MidoriType::FunctionType& function_type = actual_type->GetType<MidoriType::FunctionType>();
				if (function_type.m_param_types.size() != call.m_arguments.size())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Call expression type error: incorrect arity", call.m_paren.m_line));
				}

				std::vector<std::shared_ptr<MidoriType>> arg_results;
				for (std::unique_ptr<MidoriExpression>& call_arg : call.m_arguments)
				{
					MidoriResult::TypeResult arg_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **call_arg);
					if (!arg_result.has_value())
					{
						return arg_result;
					}

					arg_results.emplace_back(std::move(arg_result.value()));
				}

				std::vector<std::shared_ptr<MidoriType>>& param_types = function_type.m_param_types;
				for (size_t idx : std::views::iota(0u, arg_results.size()))
				{
					std::shared_ptr<MidoriType>& actual_param_type = arg_results[idx];
					std::shared_ptr<MidoriType>& param_type = param_types[idx];
					Unify(actual_param_type, param_type);

					if (*param_type != *actual_param_type)
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Call expression type error", call.m_paren, actual_param_type, &*param_types[idx]));
					}
				}

				call.m_is_foreign = function_type.m_is_foreign;
				call.m_type_data = function_type.m_return_type;

				return call.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Get& get)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **get.m_struct)
		.and_then
		(
			[&get](std::shared_ptr<MidoriType>&& actual_type) -> MidoriResult::TypeResult
			{
				if (!actual_type->IsType<MidoriType::StructType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Get expression type error: not a struct", get.m_member_name, {}, actual_type));
				}

				const MidoriType::StructType& struct_type = actual_type->GetType<MidoriType::StructType>();
				std::vector<std::string>::const_iterator find_result = std::find(struct_type.m_member_names.cbegin(), struct_type.m_member_names.cend(), get.m_member_name.m_lexeme);
				if (find_result == struct_type.m_member_names.cend())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Get expression type error: struct does not have member", get.m_member_name, {}, actual_type));
				}

				get.m_index = static_cast<int>(find_result - struct_type.m_member_names.cbegin());

				get.m_type_data = struct_type.m_member_types[static_cast<size_t>(get.m_index)];
				return get.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Set& set)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **set.m_struct)
		.and_then
		(
			[&set](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				if (!actual_type->IsType<MidoriType::StructType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Set expression type error: not a struct", set.m_member_name, {}, actual_type));
				}

				const MidoriType::StructType& struct_type = actual_type->GetType<MidoriType::StructType>();
				std::vector<std::string>::const_iterator find_result = std::find(struct_type.m_member_names.cbegin(), struct_type.m_member_names.cend(), set.m_member_name.m_lexeme);
				if (find_result == struct_type.m_member_names.cend())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Get expression type error: struct does not have member", set.m_member_name, {}, actual_type));
				}

				set.m_index = static_cast<int>(find_result - struct_type.m_member_names.cbegin());

				const std::shared_ptr<MidoriType>& member_type = struct_type.m_member_types[static_cast<size_t>(set.m_index)];

				if (*actual_type != *member_type)
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Set expression type error", set.m_member_name, actual_type, &*member_type));
				}

				set.m_type_data = std::move(actual_type);
				return set.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::BoundedName& variable)
{
	for (TypeChecker::TypeEnvironmentStack::reverse_iterator it = m_name_type_table.rbegin(); it != m_name_type_table.rend(); ++it)
	{
		TypeEnvironment::const_iterator var = it->find(variable.m_name.m_lexeme);
		if (var != it->end())
		{
			variable.m_type_data = var->second;
			return variable.m_type_data;
		}
	}

	return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("BoundedName expression type error: variable not found", variable.m_name.m_line));
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Bind& bind)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **bind.m_value)
		.and_then
		(
			[&bind, this](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				for (TypeChecker::TypeEnvironmentStack::reverse_iterator it = m_name_type_table.rbegin(); it != m_name_type_table.rend(); ++it)
				{
					TypeEnvironment::iterator var = it->find(bind.m_name.m_lexeme);
					if (var != it->end())
					{
						Unify(var->second, actual_type);
						if (*var->second != *actual_type)
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Bind expression type error", bind.m_name, actual_type, &*var->second));
						}
						else
						{
							actual_type = var->second;
							break;
						}
					}
				}

				bind.m_type_data = std::move(actual_type);
				return bind.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::TextLiteral& text)
{
	text.m_type_data = MidoriType::MakeLiteralType<MidoriType::TextType>();
	return text.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::BoolLiteral& bool_expr)
{
	bool_expr.m_type_data = MidoriType::MakeLiteralType<MidoriType::BoolType>();
	return	bool_expr.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::FloatLiteral& float_literal)
{
	float_literal.m_type_data = MidoriType::MakeLiteralType<MidoriType::FloatType>();
	return float_literal.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::IntegerLiteral& integer)
{
	integer.m_type_data = MidoriType::MakeLiteralType<MidoriType::IntegerType>();
	return integer.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::UnitLiteral& unit)
{
	unit.m_type_data = MidoriType::MakeLiteralType<MidoriType::UnitType>();
	return unit.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Function& function)
{
	std::shared_ptr<MidoriType> prev_return_type = m_curr_function_return_type.lock();
	m_curr_function_return_type = function.m_return_type;

	BeginScope();
	std::ranges::for_each
	(
		std::views::iota(0u, function.m_param_types.size()),
		[&function, this](size_t idx) {m_name_type_table.back().emplace(function.m_params[idx].m_lexeme, function.m_param_types[idx]); }
	);

	std::visit([this](auto&& arg) { (*this)(arg); }, *function.m_body);

	EndScope();

	m_curr_function_return_type = prev_return_type;
	function.m_type_data = MidoriType::MakeFunctionType(std::move(function.m_param_types), std::move(function.m_return_type));
	return function.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Construct& construct)
{
	const std::shared_ptr<MidoriType>& return_type = construct.m_return_type;
	std::optional<const MidoriType::FunctionType*> constructor_type = std::nullopt;

	for (TypeChecker::TypeEnvironmentStack::const_reverse_iterator it = m_name_type_table.crbegin(); it != m_name_type_table.crend(); ++it)
	{
		const TypeEnvironment& env = *it;
		TypeEnvironment::const_iterator var;
		if (construct.IsConstructTypeOf<MidoriExpression::Construct::Struct>())
		{
			var = env.find(return_type->ToString());
		}
		else
		{
			var = env.find(construct.m_data_name.m_lexeme);
		}
		if (var != env.end())
		{
			constructor_type.emplace(&var->second->GetType<MidoriType::FunctionType>());
			break;
		}
	}

	if (constructor_type == std::nullopt)
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Construct expression type error: struct or union not found", construct.m_data_name.m_line));
	}

	if (constructor_type.value()->m_param_types.size() != construct.m_params.size())
	{
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Construct expression type error: incorrect arity", construct.m_data_name.m_line));
		}
	}

	for (size_t idx : std::views::iota(0u, construct.m_params.size()))
	{
		std::unique_ptr<MidoriExpression>& param = construct.m_params[idx];
		MidoriResult::TypeResult param_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **param);
		if (!param_result.has_value())
		{
			return param_result;
		}

		if (*param_result.value() != *constructor_type.value()->m_param_types[idx])
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Construct expression type error", construct.m_data_name, param_result.value(), constructor_type.value()->m_param_types[idx]));
		}
	}

	construct.m_type_data = construct.m_return_type;
	return construct.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Array& array)
{
	if (array.m_elems.empty())
	{
		array.m_type_data = MidoriType::MakeArrayType(MidoriType::MakeUndecidedType());
		return array.m_type_data;
	}

	std::vector<std::shared_ptr<MidoriType>> element_results;
	element_results.reserve(array.m_elems.size());

	for (std::unique_ptr<MidoriExpression>& element : array.m_elems)
	{
		MidoriResult::TypeResult result = std::visit([this](auto&& arg) { return (*this)(arg); }, **element);
		if (!result.has_value())
		{
			return result;
		}

		element_results.emplace_back(std::move(result.value()));
	}

	for (size_t idx : std::views::iota(0u, element_results.size()))
	{
		if (*element_results[0u] != *element_results[idx])
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Array expression type error", array.m_op, element_results[idx], &*element_results[0u]));
		}
	}

	array.m_type_data = MidoriType::MakeArrayType(element_results[0u]);
	return array.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::ArrayGet& array_get)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **array_get.m_arr_var)
		.and_then
		(
			[&array_get, this](std::shared_ptr<MidoriType>&& array_var_type) ->MidoriResult::TypeResult
			{
				size_t indices_size = array_get.m_indices.size();
				for (size_t idx : std::views::iota(0u, indices_size))
				{
					std::unique_ptr<MidoriExpression>& index_expr = array_get.m_indices[idx];
					MidoriResult::TypeResult index_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **index_expr);
					if (!index_result.has_value())
					{
						return index_result;
					}
					const std::shared_ptr<MidoriType>& actual_type = index_result.value();

					if (!index_result.value()->IsType<MidoriType::IntegerType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Array get expression type error", array_get.m_op, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
					}
				}

				for (auto _ : std::views::repeat(0, indices_size))
				{
					if (!array_var_type->IsType<MidoriType::ArrayType>())
					{
						// TODO: improve error message, expect generic array type
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Array get expression type error", array_get.m_op, array_var_type, MidoriType::MakeLiteralType<MidoriType::UnitType>()));
					}

					array_var_type = array_var_type->GetType<MidoriType::ArrayType>().m_element_type;
				}

				array_get.m_type_data = std::move(array_var_type);
				return array_get.m_type_data;
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::ArraySet& array_set)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **array_set.m_arr_var)
		.and_then
		(
			[&array_set, this](std::shared_ptr<MidoriType>&& array_var_type) -> MidoriResult::TypeResult
			{
				return std::visit([this](auto&& arg) { return (*this)(arg); }, **array_set.m_value)
					.and_then
					(
						[&array_set, &array_var_type, this](std::shared_ptr<MidoriType>&& value_type) -> MidoriResult::TypeResult
						{

							for (size_t idx : std::views::iota(0u, array_set.m_indices.size()))
							{
								std::unique_ptr<MidoriExpression>& index_expr = array_set.m_indices[idx];
								MidoriResult::TypeResult index_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **index_expr);
								if (!index_result.has_value())
								{
									return index_result;
								}

								const std::shared_ptr<MidoriType>& actual_type = index_result.value();
								if (!index_result.value()->IsType<MidoriType::IntegerType>())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Array get expression type error", array_set.m_op, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
								}
							}

							for (size_t _ : std::views::iota(0u, array_set.m_indices.size()))
							{
								if (!array_var_type->IsType<MidoriType::ArrayType>())
								{
									// TODO: improve error message, expect generic array type
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Array get expression type error", array_set.m_op, array_var_type, MidoriType::MakeLiteralType<MidoriType::UnitType>()));
								}

								array_var_type = array_var_type->GetType<MidoriType::ArrayType>().m_element_type;
							}

							if (*array_var_type != *value_type)
							{
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Array set expression type error", array_set.m_op, value_type, &*array_var_type));
							}

							array_set.m_type_data = std::move(value_type);
							return array_set.m_type_data;
						}
					);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Ternary& ternary)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **ternary.m_condition)
		.and_then
		(
			[&ternary, this](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				std::shared_ptr<MidoriType> bool_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
				Unify(bool_type, actual_type);

				if (!actual_type->IsType<MidoriType::BoolType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Ternary expression type error", ternary.m_question, actual_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
				}

				UpdateConditionOperandType(ternary.m_condition_operand_type, ternary.m_condition);

				return std::visit([this](auto&& arg) { return (*this)(arg); }, **ternary.m_true_branch)
					.and_then
					(
						[&ternary, this](std::shared_ptr<MidoriType>&& true_branch_type) ->MidoriResult::TypeResult
						{
							return std::visit([this](auto&& arg) { return (*this)(arg); }, **ternary.m_else_branch)
								.and_then
								(
									[&true_branch_type, &ternary, this](std::shared_ptr<MidoriType>&& else_branch_type)->MidoriResult::TypeResult
									{
										Unify(true_branch_type, else_branch_type);
										if (*true_branch_type != *else_branch_type)
										{
											return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Ternary expression type error", ternary.m_question, else_branch_type, true_branch_type));
										}

										ternary.m_type_data = std::move(true_branch_type);
										return ternary.m_type_data;
									}
								);
						}
					);
			}
		);
}