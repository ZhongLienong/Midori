#include <algorithm>
#include <format>
#include <iterator>
#include <ranges>

#include "Common/Error/Error.h"
#include "TypeChecker.h"

MidoriResult::TypeResult TypeChecker::Unify(const Token& token, std::shared_ptr<MidoriType>& left, std::shared_ptr<MidoriType>& right)
{
	if (*left == *right)
	{
		return left;
	}
	else if (left->IsType<MidoriType::UndecidedType>() && !right->IsType<MidoriType::UndecidedType>())
	{
		*left = *right;
		return left;
	}
	else if (!left->IsType<MidoriType::UndecidedType>() && right->IsType<MidoriType::UndecidedType>())
	{
		*right = *left;
		return left;
	}
	else if (left->IsType<MidoriType::ArrayType>() && right->IsType<MidoriType::ArrayType>())
	{
		return Unify(token, left->GetType<MidoriType::ArrayType>().m_element_type, right->GetType<MidoriType::ArrayType>().m_element_type);
	}
	else if (left->IsType<MidoriType::FunctionType>() && right->IsType<MidoriType::FunctionType>())
	{
		MidoriType::FunctionType& left_func = left->GetType<MidoriType::FunctionType>();
		MidoriType::FunctionType& right_func = right->GetType<MidoriType::FunctionType>();
		MidoriResult::TypeResult result;

		for (size_t idx : std::views::iota(0u, left_func.m_param_types.size()))
		{
			result = Unify(token, left_func.m_param_types[idx], right_func.m_param_types[idx]);
			if (!result.has_value())
			{
				return result;
			}
		}

		return Unify(token, left_func.m_return_type, right_func.m_return_type);
	}
	else
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Unable to unify: ", token, left, right));
	}
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

TypeChecker::TypeChecker(MidoriProgramTree&& parser_result)
	: m_program_tree(std::move(parser_result))
{
}

MidoriResult::TypeCheckerResult TypeChecker::TypeCheck()
{
	std::string errors;

	BeginScope();
	std::ranges::for_each
	(
		m_program_tree,
		[&errors, this](std::unique_ptr<MidoriStatement>& statement)
		{
			MidoriResult::TypeResult result = std::visit([this](auto&& arg) { return (*this)(arg); }, **statement);
			if (!result.has_value())
			{
				errors.append(result.error()).append("\n");
			}
		}
	);
	EndScope();

	if (errors.empty())
	{
		return std::move(m_program_tree);
	}
	else
	{
		return std::unexpected<std::string>(std::move(errors));
	}
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Simple& simple)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **simple.m_expr)
		.and_then
		(
			[&simple, this](std::shared_ptr<MidoriType>&& type) ->MidoriResult::TypeResult
			{
				if (simple.m_expr->IsExpression<MidoriExpression::Break>() || simple.m_expr->IsExpression<MidoriExpression::Return>())
				{
					return type;
				}
				else
				{
					return MidoriType::MakeUndecidedType();
				}
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Define& def)
{
	if (def.m_value->IsExpression<MidoriExpression::Function>())
	{
		MidoriExpression::Function& function = def.m_value->GetExpression<MidoriExpression::Function>();
		function.m_type_data = def.m_annotated_type.value();
		def.m_value->GetType() = function.m_type_data;
		m_name_type_table.back().emplace(def.m_name.m_lexeme, def.m_value->GetType());
		MidoriType::FunctionType& function_type = def.m_value->GetType()->GetType<MidoriType::FunctionType>();

		std::shared_ptr<MidoriType> actual_type = m_name_type_table.back()[def.m_name.m_lexeme];

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
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Define statement type error", def.m_name, actual_type, annotated_type));
			}
		}

		return std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value)
			.and_then
			(
				[this](std::shared_ptr<MidoriType>&& type) -> MidoriResult::TypeResult
				{
					EndScope();
					return MidoriType::MakeUndecidedType();
				}
			)
			.or_else
			(
				[this](std::string&& error)->MidoriResult::TypeResult
				{
					EndScope();
					return std::unexpected<std::string>(std::move(error));
				}
			);
	}
	else if (def.m_value->IsExpression<MidoriExpression::Construct>())
	{
		return std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value)
			.and_then
			(
				[&def, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
				{
					if (def.m_annotated_type.has_value())
					{
						std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
						if (*annotated_type != *type)
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Define statement type error", def.m_name, type, annotated_type));
						}
						m_name_type_table.back().emplace(def.m_name.m_lexeme, annotated_type);
					}
					else
					{
						m_name_type_table.back().emplace(def.m_name.m_lexeme, type);
					}

					return MidoriType::MakeUndecidedType();
				}
			);
	}
	else if (def.m_value->IsExpression<MidoriExpression::Array>())
	{
		return std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value)
			.and_then
			(
				[&def, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
				{
					if (def.m_annotated_type.has_value())
					{
						std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
						return Unify(def.m_name, annotated_type, type);
					}

					m_name_type_table.back().emplace(def.m_name.m_lexeme, type);
					return MidoriType::MakeUndecidedType();
				}
			);
	}
	else if (def.m_value->IsExpression<MidoriExpression::Loop>())
	{
		return std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value)
			.and_then
			(
				[&def, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
				{
					if (def.m_annotated_type.has_value())
					{
						std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
						return Unify(def.m_name, annotated_type, type)
							.and_then
							(
								[&def, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
								{
									// TODO: Should not instantiate a Never type
									m_name_type_table.back().emplace(def.m_name.m_lexeme, type);
									return MidoriType::MakeUndecidedType();
								}
							);
					}
					m_name_type_table.back().emplace(def.m_name.m_lexeme, type);
					return MidoriType::MakeUndecidedType();
				}
			);
	}
	else
	{
		return std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value)
			.and_then
			(
				[&def, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
				{
					if (def.m_annotated_type.has_value())
					{
						std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
						return Unify(def.m_name, annotated_type, type)
							.and_then
							(
								[&def, &annotated_type, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
								{
									m_name_type_table.back().emplace(def.m_name.m_lexeme, annotated_type);
									return type;
								}
							);
					}

					m_name_type_table.back().emplace(def.m_name.m_lexeme, type);
					return MidoriType::MakeUndecidedType();
				}
			);
	}
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Continue&)
{
	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Foreign& foreign)
{
	m_name_type_table.back()[foreign.m_function_name.m_lexeme] = foreign.m_type;
	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Struct& struct_stmt)
{
	std::shared_ptr<MidoriType> struct_constructor_type = MidoriType::MakeFunctionType(struct_stmt.m_self_type->GetType<MidoriType::StructType>().m_member_types, std::move(struct_stmt.m_self_type));
	m_name_type_table.back()[struct_stmt.m_name.m_lexeme] = struct_constructor_type;

	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Union& union_stmt)
{
	MidoriType::UnionType& union_type = union_stmt.m_self_type->GetType<MidoriType::UnionType>();
	for (auto& [member_name, member_ctx] : union_type.m_member_info)
	{
		std::shared_ptr<MidoriType> union_constructor_type = MidoriType::MakeFunctionType(std::move(member_ctx.m_member_types), std::shared_ptr(union_stmt.m_self_type));
		m_name_type_table.back()[member_name] = union_constructor_type;
	}

	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Namespace& namespace_stmt)
{
	std::ranges::for_each
	(
		namespace_stmt.m_stmts,
		[this](const std::unique_ptr<MidoriStatement>& stmt) { std::visit([this](auto&& arg) { return (*this)(arg); }, **stmt); }
	);

	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Match& match)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **match.m_arg_expr)
		.and_then
		(
			[&match, this](std::shared_ptr<MidoriType>&& arg_type) -> MidoriResult::TypeResult
			{
				// Check if the expression is a union type
				if (!arg_type->IsType<MidoriType::UnionType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Match expression type error", match.m_match_keyword, {}, arg_type));
				}

				const MidoriType::UnionType& union_type = arg_type->GetType<MidoriType::UnionType>();
				auto key_view = union_type.m_member_info | std::views::transform([](const auto& pair) { return pair.first; });
				std::unordered_set<std::string> expected_member_names(key_view.begin(), key_view.end());
				bool has_default_case = false;
				std::shared_ptr<MidoriType>* prev_case_type = nullptr;

				for (const std::unique_ptr<MidoriExpression>& case_expr : match.m_cases)
				{
					BeginScope();
					std::string error;
					MidoriResult::TypeResult case_result;

					if (case_expr->IsExpression<MidoriExpression::Default>())
					{
						has_default_case = true;
						case_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **case_expr);
					}
					else if (case_expr->IsExpression<MidoriExpression::Case>())
					{
						if (has_default_case)
						{
							// TODO: Add warning for unreachable code
						}

						MidoriExpression::Case& member_case = case_expr->GetExpression<MidoriExpression::Case>();
						const std::string& branch_name = member_case.m_member_name;
						if (!expected_member_names.contains(branch_name))
						{
							error = MidoriError::GenerateTypeCheckerError(std::format("Match expression type error: unrecognized member '{}'", branch_name), member_case.m_keyword.m_line);
						}
						else
						{
							if (member_case.m_binding_names.size() != union_type.m_member_info.at(branch_name).m_member_types.size())
							{
								error = MidoriError::GenerateTypeCheckerError("Match expression type error: incorrect case arity", member_case.m_keyword.m_line);
							}
							else
							{
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
								case_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **member_case.m_expr);
							}
						}
					}

					EndScope();
					if (!error.empty())
					{
						return std::unexpected<std::string>(std::move(error));
					}
					else if (!case_result.has_value())
					{
						return std::unexpected<std::string>(std::move(case_result.error()));
					}
					else
					{
						if (prev_case_type == nullptr)
						{
							prev_case_type = &case_result.value();
						}
						else if (**prev_case_type != *case_result.value())
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Match expression type error: case types do not match", match.m_match_keyword, *prev_case_type, case_result.value()));
						}
					}
				}

				if (!expected_member_names.empty() && !has_default_case)
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Not all union members are matched", match.m_match_keyword.m_line));
				}
				else
				{
					match.m_type_data = *prev_case_type;
					return match.m_type_data;
				}
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Case& case_expr)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **case_expr.m_expr)
		.and_then
		(
			[&case_expr, this](std::shared_ptr<MidoriType>&& expr_type) ->MidoriResult::TypeResult
			{
				return Unify(case_expr.m_keyword, case_expr.m_type_data, expr_type);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Default& default_expr)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **default_expr.m_expr)
		.and_then
		(
			[&default_expr, this](std::shared_ptr<MidoriType>&& expr_type) ->MidoriResult::TypeResult
			{
				return Unify(default_expr.m_keyword, default_expr.m_type_data, expr_type);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Loop& loop)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **loop.m_body)
		.and_then
		(
			[&loop, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
			{
				if (loop.m_body->IsExpression<MidoriExpression::Break>())
				{
					return Unify(loop.m_loop_keyword, loop.m_type_data, type);
				}
				else if (loop.m_body->IsExpression<MidoriExpression::Block>())
				{
					if (loop.m_body->Contains<MidoriExpression::Break>())
					{
						return Unify(loop.m_loop_keyword, loop.m_type_data, type);
					}
				}

				loop.m_type_data = MidoriType::MakeLiteralType<MidoriType::NeverType>();
				return loop.m_type_data;
			}
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
							return Unify(binary.m_op, left_type, right_type)
								.and_then
								(
									[&binary, &left_type, &right_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
									{
										std::shared_ptr<MidoriType>& self_type = binary.m_type_data;

										// Handle array operations
										if (binary.m_op.m_token_name == Token::Name::STAR && left_type->IsType<MidoriType::ArrayType>() && right_type->IsType<MidoriType::IntegerType>())
										{
											self_type = left_type;
											return self_type;
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
											if (!self_type->IsNumericType() && !self_type->IsType<MidoriType::TextType>() && !self_type->IsType<MidoriType::BoolType>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Binary expression type error", binary.m_op, self_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>(), MidoriType::MakeLiteralType<MidoriType::TextType>(), MidoriType::MakeLiteralType<MidoriType::BoolType>()));
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
					MidoriResult::TypeResult result = Unify(call.m_paren, actual_param_type, param_type);
					if (!result.has_value())
					{
						return result;
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
						return Unify(bind.m_name, var->second, actual_type)
							.and_then
							(
								[&bind, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
								{
									bind.m_type_data = type;
									return bind.m_type_data;
								}
							);
					}
				}

				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Bind expression type error: variable not found", bind.m_name.m_line));
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
	const MidoriType::FunctionType& type_data = function.m_type_data->GetType<MidoriType::FunctionType>();

	BeginScope();
	std::ranges::for_each
	(
		std::views::iota(0u, type_data.m_param_types.size()),
		[&type_data, &function, this](size_t idx) {m_name_type_table.back().emplace(function.m_params[idx].m_lexeme, type_data.m_param_types[idx]); }
	);

    return std::visit([this](auto&& arg) { return (*this)(arg); }, **function.m_return_value)
		.and_then
		(
			[&function, &type_data, this](std::shared_ptr<MidoriType>&& function_return_value_type) ->MidoriResult::TypeResult
			{
				EndScope();

				if (type_data.m_return_type)
				{
					if (*type_data.m_return_type != *function_return_value_type)
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError(std::format("Function expression type error: the type of this function expression conflicts with the type annotation, annotation: {}, deduced: {}", type_data.m_return_type->ToString(), function_return_value_type->ToString()), function.m_function_keyword.m_line));
					}
				}

				return function.m_type_data;
			}
		);
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
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("Construct expression type error: incorrect arity", construct.m_data_name.m_line));
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

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::IfElse& if_else)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **if_else.m_condition)
		.and_then
		(
			[&if_else, this](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				std::shared_ptr<MidoriType> bool_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
				return Unify(if_else.m_if_token, bool_type, actual_type)
					.and_then
					(
						[&if_else, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
						{
							if (!type->IsType<MidoriType::BoolType>())
							{
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerError("IfElse expression type error", if_else.m_if_token, type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
							}

							UpdateConditionOperandType(if_else.m_condition_operand_type, if_else.m_condition);

							return std::visit([this](auto&& arg) { return (*this)(arg); }, **if_else.m_true_branch)
								.and_then
								(
									[&if_else, this](std::shared_ptr<MidoriType>&& true_branch_type) ->MidoriResult::TypeResult
									{
										return std::visit([this](auto&& arg) { return (*this)(arg); }, **if_else.m_else_branch)
											.and_then
											(
												[&true_branch_type, &if_else, this](std::shared_ptr<MidoriType>&& else_branch_type)->MidoriResult::TypeResult
												{
													return Unify(if_else.m_else_token, true_branch_type, else_branch_type)
														.and_then
														(
															[&if_else](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
															{
																if_else.m_type_data = type;
																return if_else.m_type_data;
															}
														);
												}
											);
									}
								);
						}
					);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Block& block)
{
	BeginScope();

	for (const std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
	{
		MidoriResult::TypeResult result = std::visit([this](auto&& arg) { return (*this)(arg); }, **stmt);
		if (!result.has_value())
		{
			EndScope();
			return result;
		}
		else
		{
			result = Unify(block.m_right_brace, block.m_type_data, result.value());
			if (!result.has_value())
			{
				EndScope();
				return result;
			}
		}
	}

	if (block.m_final_expr.has_value())
	{
		return std::visit([this](auto&& arg) { return (*this)(arg); }, ***block.m_final_expr)
			.and_then
			(
				[&block, this](std::shared_ptr<MidoriType>&& final_value)->MidoriResult::TypeResult
				{
					EndScope();

					return Unify(block.m_right_brace, block.m_type_data, final_value);
				}
			);
	}
	else
	{
		EndScope();
		if (block.m_type_data->IsType<MidoriType::UndecidedType>())
		{
			block.m_type_data = MidoriType::MakeLiteralType<MidoriType::UnitType>();
		}
		return block.m_type_data;
	}
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Break& break_expr)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **break_expr.m_value)
		.and_then
		(
			[&break_expr, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
			{
				return Unify(break_expr.m_keyword, break_expr.m_type_data, type);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Return& return_expr)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **return_expr.m_value)
		.and_then
		(
			[&return_expr, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
			{
				return Unify(return_expr.m_keyword, return_expr.m_type_data, type);
			}
		);
}