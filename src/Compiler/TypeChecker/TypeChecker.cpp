#include <algorithm>
#include <format>
#include <iterator>
#include <ranges>

#include "Common/Error/Error.h"
#include "TypeChecker.h"

MidoriResult::TypeResult TypeChecker::Unify(const Token& token, std::shared_ptr<MidoriType>& left, std::shared_ptr<MidoriType>& right)
{
	// Apply current substitutions first
	std::shared_ptr<MidoriType> left_subst = ApplySubstitution(left);
	std::shared_ptr<MidoriType> right_subst = ApplySubstitution(right);

	if (*left_subst == *right_subst)
	{
		return left_subst;
	}
	// Never type unifies with any type (it's the bottom type)
	else if (left_subst->IsType<MidoriType::NeverType>())
	{
		return right_subst;
	}
	else if (right_subst->IsType<MidoriType::NeverType>())
	{
		return left_subst;
	}
	else if (left_subst->IsType<MidoriType::TypeVariable>())
	{
		int var_id = left_subst->GetType<MidoriType::TypeVariable>().m_id;
		if (OccursCheck(var_id, right_subst))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Infinite type detected (occurs check failed)", token, m_file_name, m_source_lines, left_subst, right_subst));
		}
		m_type_substitution[var_id] = right_subst;
		*left = *right_subst;
		return left;
	}
	else if (right_subst->IsType<MidoriType::TypeVariable>())
	{
		int var_id = right_subst->GetType<MidoriType::TypeVariable>().m_id;
		if (OccursCheck(var_id, left_subst))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Infinite type detected (occurs check failed)", token, m_file_name, m_source_lines, left_subst, right_subst));
		}
		m_type_substitution[var_id] = left_subst;
		*right = *left_subst;
		return left;
	}
	else if (left_subst->IsType<MidoriType::UndecidedType>() && !right_subst->IsType<MidoriType::UndecidedType>())
	{
		*left = *right_subst;
		return left;
	}
	else if (!left_subst->IsType<MidoriType::UndecidedType>() && right_subst->IsType<MidoriType::UndecidedType>())
	{
		*right = *left_subst;
		return left;
	}
	else if (left_subst->IsType<MidoriType::ArrayType>() && right_subst->IsType<MidoriType::ArrayType>())
	{
		return Unify(token, left_subst->GetType<MidoriType::ArrayType>().m_element_type, right_subst->GetType<MidoriType::ArrayType>().m_element_type);
	}
	else if (left_subst->IsType<MidoriType::FunctionType>() && right_subst->IsType<MidoriType::FunctionType>())
	{
		MidoriType::FunctionType& left_func = left_subst->GetType<MidoriType::FunctionType>();
		MidoriType::FunctionType& right_func = right_subst->GetType<MidoriType::FunctionType>();

		// Function types must have the same number of parameters
		if (left_func.m_param_types.size() != right_func.m_param_types.size())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify function types with different parameter counts", token, m_file_name, m_source_lines, left_subst, right_subst));
		}

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
	else if (left_subst->IsType<MidoriType::StructType>() && right_subst->IsType<MidoriType::StructType>())
	{
		MidoriType::StructType& left_struct = left_subst->GetType<MidoriType::StructType>();
		MidoriType::StructType& right_struct = right_subst->GetType<MidoriType::StructType>();

		// Struct types must have the same name and same number of members
		if (left_struct.m_name != right_struct.m_name || left_struct.m_member_types.size() != right_struct.m_member_types.size())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify", token, m_file_name, m_source_lines, left_subst, right_subst));
		}

		// Unify each member type
		for (size_t idx : std::views::iota(0u, left_struct.m_member_types.size()))
		{
			MidoriResult::TypeResult result = Unify(token, left_struct.m_member_types[idx], right_struct.m_member_types[idx]);
			if (!result.has_value())
			{
				return result;
			}
		}

		return left;
	}
	else if (left_subst->IsType<MidoriType::UnionType>() && right_subst->IsType<MidoriType::UnionType>())
	{
		MidoriType::UnionType& left_union = left_subst->GetType<MidoriType::UnionType>();
		MidoriType::UnionType& right_union = right_subst->GetType<MidoriType::UnionType>();

		// Union types must have the same name and same members
		if (left_union.m_name != right_union.m_name || left_union.m_member_info.size() != right_union.m_member_info.size())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify", token, m_file_name, m_source_lines, left_subst, right_subst));
		}

		// Unify each member's types
		for (const auto& [member_name, left_ctx] : left_union.m_member_info)
		{
			std::unordered_map<std::string, MidoriType::UnionType::UnionMemberContext>::iterator right_it = right_union.m_member_info.find(member_name);
			if (right_it == right_union.m_member_info.end())
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify", token, m_file_name, m_source_lines, left_subst, right_subst));
			}

			const MidoriType::UnionType::UnionMemberContext& right_ctx = right_it->second;
			if (left_ctx.m_member_types.size() != right_ctx.m_member_types.size())
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify", token, m_file_name, m_source_lines, left_subst, right_subst));
			}

			for (size_t idx : std::views::iota(0u, left_ctx.m_member_types.size()))
			{
				std::shared_ptr<MidoriType> left_member = left_ctx.m_member_types[idx];
				std::shared_ptr<MidoriType> right_member = right_ctx.m_member_types[idx];
				MidoriResult::TypeResult result = Unify(token, left_member, right_member);
				if (!result.has_value())
				{
					return result;
				}
			}
		}

		return left;
	}
	else
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify", token, m_file_name, m_source_lines, left_subst, right_subst));
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

std::shared_ptr<MidoriType> TypeChecker::FreshTypeVar()
{
	return MidoriType::MakeTypeVariable(m_next_type_var_id++);
}

std::shared_ptr<MidoriType> TypeChecker::Freshen(const std::shared_ptr<MidoriType>& type)
{
	FresheningContext context;
	return Freshen(type, context);
}

std::shared_ptr<MidoriType> TypeChecker::Freshen(const std::shared_ptr<MidoriType>& type, FresheningContext& context)
{
	// Check cache first to handle recursive types
	std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>::iterator cache_it = context.m_type_cache.find(type.get());
	if (cache_it != context.m_type_cache.end())
	{
		return cache_it->second;
	}

	if (type->IsType<MidoriType::UndecidedType>() || type->IsType<MidoriType::TypeVariable>())
	{
		return FreshTypeVar();
	}
	else if (type->IsType<MidoriType::GenericParam>())
	{
		// For GenericParam, check if we've already freshened this parameter name
		const std::string& param_name = type->GetType<MidoriType::GenericParam>().m_name;
		std::unordered_map<std::string, std::shared_ptr<MidoriType>>::iterator it = context.m_generic_params.find(param_name);
		if (it != context.m_generic_params.end())
		{
			// We've already freshened this generic parameter - return the same type variable
			return it->second;
		}
		else
		{
			// First time seeing this generic parameter - create a fresh type variable and store it
			std::shared_ptr<MidoriType> fresh_var = FreshTypeVar();
			context.m_generic_params[param_name] = fresh_var;
			return fresh_var;
		}
	}
	else if (type->IsType<MidoriType::ArrayType>())
	{
		MidoriType::ArrayType& array_type = type->GetType<MidoriType::ArrayType>();
		return MidoriType::MakeArrayType(Freshen(array_type.m_element_type, context));
	}
	else if (type->IsType<MidoriType::FunctionType>())
	{
		MidoriType::FunctionType& func_type = type->GetType<MidoriType::FunctionType>();
		std::vector<std::shared_ptr<MidoriType>> fresh_params;
		std::ranges::for_each
		(
			func_type.m_param_types,
			[&fresh_params, &context, this](const std::shared_ptr<MidoriType>& param_type)
			{
				fresh_params.emplace_back(Freshen(param_type, context));
			}
		);

		std::shared_ptr<MidoriType> fresh_return = Freshen(func_type.m_return_type, context);
		return MidoriType::MakeFunctionType(fresh_params, std::move(fresh_return), func_type.m_is_foreign);
	}
	else if (type->IsType<MidoriType::StructType>())
	{
		MidoriType::StructType& struct_type = type->GetType<MidoriType::StructType>();

		// Create fresh struct and add to cache BEFORE recursing to handle cycles
		std::vector<std::shared_ptr<MidoriType>> empty_member_types;
		std::vector<std::string> member_names_copy = struct_type.m_member_names;
		std::shared_ptr<MidoriType> fresh_struct = MidoriType::MakeStructType(struct_type.m_name, std::move(empty_member_types), std::move(member_names_copy));
		context.m_type_cache[type.get()] = fresh_struct;

		// Now freshen members
		std::vector<std::shared_ptr<MidoriType>> fresh_member_types;
		std::ranges::for_each
		(
			struct_type.m_member_types,
			[&fresh_member_types, &context, this](const std::shared_ptr<MidoriType>& member_type)
			{
				fresh_member_types.push_back(Freshen(member_type, context));
			}
		);

		// Update the fresh struct with freshened members
		fresh_struct->GetType<MidoriType::StructType>().m_member_types = std::move(fresh_member_types);

		return fresh_struct;
	}
	else if (type->IsType<MidoriType::UnionType>())
	{
		MidoriType::UnionType& union_type = type->GetType<MidoriType::UnionType>();

		// Create fresh union and add to cache BEFORE recursing to handle cycles
		std::shared_ptr<MidoriType> fresh_union = MidoriType::MakeUnionType(union_type.m_name);
		context.m_type_cache[type.get()] = fresh_union;
		MidoriType::UnionType& fresh_union_ref = fresh_union->GetType<MidoriType::UnionType>();

		// Now freshen member types
		for (const auto& [member_name, member_ctx] : union_type.m_member_info)
		{
			std::vector<std::shared_ptr<MidoriType>> fresh_member_types;
			std::ranges::for_each
			(
				member_ctx.m_member_types,
				[&fresh_member_types, &context, this](const std::shared_ptr<MidoriType>& member_type)
				{
					fresh_member_types.emplace_back(Freshen(member_type, context));
				}
			);
			fresh_union_ref.m_member_info.emplace(member_name, MidoriType::UnionType::UnionMemberContext(std::move(fresh_member_types), member_ctx.m_tag));
		}

		return fresh_union;
	}
	return type;
}

std::shared_ptr<MidoriType> TypeChecker::ApplySubstitution(const std::shared_ptr<MidoriType>& type)
{
	std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>> cache;
	return ApplySubstitution(type, cache);
}

std::shared_ptr<MidoriType> TypeChecker::ApplySubstitution(const std::shared_ptr<MidoriType>& type, std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>& cache)
{
	// Check cache first to handle recursive types
	std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>::iterator cache_it = cache.find(type.get());
	if (cache_it != cache.end())
	{
		return cache_it->second;
	}

	if (type->IsType<MidoriType::TypeVariable>())
	{
		int var_id = type->GetType<MidoriType::TypeVariable>().m_id;
		TypeSubstitution::iterator it = m_type_substitution.find(var_id);
		if (it != m_type_substitution.end())
		{
			// Recursively apply substitution in case the substitution itself contains type variables
			return ApplySubstitution(it->second, cache);
		}
		return type;
	}
	else if (type->IsType<MidoriType::ArrayType>())
	{
		MidoriType::ArrayType& array_type = type->GetType<MidoriType::ArrayType>();
		std::shared_ptr<MidoriType> element_type = ApplySubstitution(array_type.m_element_type, cache);
		if (element_type != array_type.m_element_type)
		{
			return MidoriType::MakeArrayType(element_type);
		}
		return type;
	}
	else if (type->IsType<MidoriType::FunctionType>())
	{
		MidoriType::FunctionType& func_type = type->GetType<MidoriType::FunctionType>();
		bool changed = false;
		std::vector<std::shared_ptr<MidoriType>> new_param_types;

		for (const std::shared_ptr<MidoriType>& param_type : func_type.m_param_types)
		{
			std::shared_ptr<MidoriType> subst_param = ApplySubstitution(param_type, cache);
			new_param_types.push_back(subst_param);
			if (subst_param != param_type)
			{
				changed = true;
			}
		}

		std::shared_ptr<MidoriType> new_return_type = ApplySubstitution(func_type.m_return_type, cache);
		if (new_return_type != func_type.m_return_type)
		{
			changed = true;
		}

		if (changed)
		{
			return MidoriType::MakeFunctionType(new_param_types, std::move(new_return_type), func_type.m_is_foreign);
		}
		return type;
	}
	else if (type->IsType<MidoriType::StructType>())
	{
		MidoriType::StructType& struct_type = type->GetType<MidoriType::StructType>();

		// Create new struct and add to cache BEFORE recursing to handle cycles
		std::vector<std::shared_ptr<MidoriType>> empty_member_types;
		std::vector<std::string> member_names_copy = struct_type.m_member_names;
		std::shared_ptr<MidoriType> new_struct = MidoriType::MakeStructType(struct_type.m_name, std::move(empty_member_types), std::move(member_names_copy));
		cache[type.get()] = new_struct;

		bool changed = false;
		std::vector<std::shared_ptr<MidoriType>> new_member_types;

		for (const std::shared_ptr<MidoriType>& member_type : struct_type.m_member_types)
		{
			std::shared_ptr<MidoriType> subst_member = ApplySubstitution(member_type, cache);
			new_member_types.push_back(subst_member);
			if (subst_member != member_type)
			{
				changed = true;
			}
		}

		if (changed)
		{
			new_struct->GetType<MidoriType::StructType>().m_member_types = std::move(new_member_types);
			return new_struct;
		}
		return type;
	}
	else if (type->IsType<MidoriType::UnionType>())
	{
		MidoriType::UnionType& union_type = type->GetType<MidoriType::UnionType>();

		// Create new union and add to cache BEFORE recursing to handle cycles
		std::shared_ptr<MidoriType> new_union = MidoriType::MakeUnionType(union_type.m_name);
		cache[type.get()] = new_union;
		MidoriType::UnionType& new_union_ref = new_union->GetType<MidoriType::UnionType>();

		bool changed = false;

		for (const auto& [member_name, member_ctx] : union_type.m_member_info)
		{
			std::vector<std::shared_ptr<MidoriType>> new_member_types;
			for (const std::shared_ptr<MidoriType>& member_type : member_ctx.m_member_types)
			{
				std::shared_ptr<MidoriType> subst_member = ApplySubstitution(member_type, cache);
				new_member_types.push_back(subst_member);
				if (subst_member != member_type)
				{
					changed = true;
				}
			}
			new_union_ref.m_member_info.emplace(member_name, MidoriType::UnionType::UnionMemberContext(std::move(new_member_types), member_ctx.m_tag));
		}

		if (changed)
		{
			return new_union;
		}
		return type;
	}

	return type;
}

bool TypeChecker::OccursCheck(int var_id, const std::shared_ptr<MidoriType>& type)
{
	std::shared_ptr<MidoriType> subst_type = ApplySubstitution(type);

	if (subst_type->IsType<MidoriType::TypeVariable>())
	{
		return subst_type->GetType<MidoriType::TypeVariable>().m_id == var_id;
	}
	else if (subst_type->IsType<MidoriType::ArrayType>())
	{
		return OccursCheck(var_id, subst_type->GetType<MidoriType::ArrayType>().m_element_type);
	}
	else if (subst_type->IsType<MidoriType::FunctionType>())
	{
		MidoriType::FunctionType& func_type = subst_type->GetType<MidoriType::FunctionType>();
		for (const std::shared_ptr<MidoriType>& param_type : func_type.m_param_types)
		{
			if (OccursCheck(var_id, param_type))
			{
				return true;
			}
		}
		return OccursCheck(var_id, func_type.m_return_type);
	}
	else if (subst_type->IsType<MidoriType::StructType>())
	{
		MidoriType::StructType& struct_type = subst_type->GetType<MidoriType::StructType>();
		for (const std::shared_ptr<MidoriType>& member_type : struct_type.m_member_types)
		{
			if (OccursCheck(var_id, member_type))
			{
				return true;
			}
		}
		return false;
	}
	else if (subst_type->IsType<MidoriType::UnionType>())
	{
		MidoriType::UnionType& union_type = subst_type->GetType<MidoriType::UnionType>();
		for (const auto& [member_name, member_ctx] : union_type.m_member_info)
		{
			for (const std::shared_ptr<MidoriType>& member_type : member_ctx.m_member_types)
			{
				if (OccursCheck(var_id, member_type))
				{
					return true;
				}
			}
		}
		return false;
	}

	return false;
}

TypeChecker::TypeChecker(MidoriProgramTree&& parser_result, std::string_view file_name, const std::vector<std::string>& source_lines)
	: m_program_tree(std::move(parser_result)), 
	m_source_lines(source_lines),
	m_next_type_var_id(0),
	m_file_name(file_name)
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
	// Special handling for functions (scope management required)
	if (def.m_value->IsExpression<MidoriExpression::Function>())
	{
		MidoriExpression::Function& function = def.m_value->GetExpression<MidoriExpression::Function>();

		// Freshen any UndecidedType parameters to TypeVariables
		for (std::shared_ptr<MidoriType>& param_type : function.m_param_types)
		{
			param_type = Freshen(param_type);
		}
		function.m_return_type = Freshen(function.m_return_type);

		std::shared_ptr<MidoriType> function_type = MidoriType::MakeFunctionType(function.m_param_types, std::move(function.m_return_type));
		function.m_type_data = function_type;
		def.m_value->GetType() = function.m_type_data;

		m_name_type_table.back().emplace(def.m_name.m_lexeme, def.m_value->GetType());
		MidoriType::FunctionType& function_type_ref = def.m_value->GetType()->GetType<MidoriType::FunctionType>();
		if (def.m_annotated_type.has_value())
		{
			std::shared_ptr<MidoriType>& annotated_type = def.m_annotated_type.value();
			if (*annotated_type != *function_type)
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Define statement type error: function type annotation doesn't match function signature", def.m_name, m_file_name, m_source_lines, function_type, annotated_type));
			}
		}

		BeginScope();
		std::ranges::for_each
		(
			std::views::iota(0u, function_type_ref.m_param_types.size()),
			[&function, &function_type_ref, this](size_t idx) { m_name_type_table.back().emplace(function.m_params[idx].m_lexeme, function_type_ref.m_param_types[idx]); }
		);

		return std::visit([this](auto&& arg) { return (*this)(arg); }, **def.m_value)
			.and_then
			(
				[this](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
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

	// General case
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
							[&def, &annotated_type, this](std::shared_ptr<MidoriType>&& unified_type)->MidoriResult::TypeResult
							{
								m_name_type_table.back().emplace(def.m_name.m_lexeme, annotated_type);
								return unified_type;
							}
						);
				}

				// Check if this is an empty array without type annotation
				if (type->IsType<MidoriType::ArrayType>())
				{
					const MidoriType::ArrayType& array_type = type->GetType<MidoriType::ArrayType>();
					if (array_type.m_element_type->IsType<MidoriType::UndecidedType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Define statement type error: empty arrays require type annotations. Use 'def " + def.m_name.m_lexeme + " : Array<ElementType> = [];'", def.m_name, m_file_name, m_source_lines));
					}
				}

				m_name_type_table.back().emplace(def.m_name.m_lexeme, type);
				return MidoriType::MakeUndecidedType();
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::DefineFunction& defun)
{
	// If function has generic parameters, check for lambda syntax error
	if (!defun.m_generic_params.empty())
	{
		// Validate that generic parameter names are unique
		std::unordered_set<std::string> generic_param_names;
		for (const Token& generic_param : defun.m_generic_params)
		{
			if (!generic_param_names.insert(generic_param.m_lexeme).second)
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("DefineFunction type error: duplicate generic parameter name", generic_param, m_file_name, m_source_lines));
			}
		}

		// Validate that generic parameters don't conflict with function parameters
		for (const Token& param : defun.m_params)
		{
			if (generic_param_names.contains(param.m_lexeme))
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("DefineFunction type error: generic parameter conflicts with function parameter", param, m_file_name, m_source_lines));
			}
		}
	}

	// Freshen any UndecidedType parameters to TypeVariables
	for (std::shared_ptr<MidoriType>& param_type : defun.m_param_types)
	{
		param_type = Freshen(param_type);
	}
	defun.m_return_type = Freshen(defun.m_return_type);

	std::shared_ptr<MidoriType> return_type_copy = defun.m_return_type;
	m_name_type_table.back()[defun.m_name.m_lexeme] = MidoriType::MakeFunctionType(defun.m_param_types, std::move(return_type_copy));
	if (!defun.m_generic_params.empty())
	{
		m_generic_functions.insert(defun.m_name.m_lexeme);
	}

	BeginScope();

	for (const Token& generic_param : defun.m_generic_params)
	{
		m_name_type_table.back().emplace(generic_param.m_lexeme, FreshTypeVar());
	}
	std::ranges::for_each
	(
		std::views::iota(0u, defun.m_params.size()),
		[&defun, this](size_t idx) 
		{
			m_name_type_table.back().emplace(defun.m_params[idx].m_lexeme, defun.m_param_types[idx]);
		}
	);

	std::shared_ptr<MidoriType> saved_expected_return_type = m_expected_return_type;
	m_expected_return_type = defun.m_return_type;

	return std::visit([this](auto&& arg) { return (*this)(arg); }, **defun.m_body)
		.and_then
		(
			[&defun, &saved_expected_return_type, this](std::shared_ptr<MidoriType>&& function_return_value_type) ->MidoriResult::TypeResult
			{
				EndScope();
				m_expected_return_type = saved_expected_return_type;

				// If the body contains a return statement, the return statement itself
				// validates the return type, so we don't need to check the body's natural type
				bool body_contains_return = defun.m_body->Contains<MidoriExpression::Return>();

				if (body_contains_return)
				{
					// Return statements handle their own type checking
					return defun.m_return_type;
				}
				else
				{
					// Body completes normally - verify it returns the correct type
					return Unify(defun.m_name, defun.m_return_type, function_return_value_type)
						.and_then
						(
							[&defun](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
							{
								return defun.m_return_type;
							}
						);
				}
			}
		).or_else
		(
			[&saved_expected_return_type, this](std::string&& error) -> MidoriResult::TypeResult
			{
				m_expected_return_type = saved_expected_return_type;  // Restore on error too
				return std::unexpected<std::string>(std::move(error));
			}
		);
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
	if (!struct_stmt.m_generic_params.empty())
	{
		std::unordered_set<std::string> generic_param_names;
		for (const Token& generic_param : struct_stmt.m_generic_params)
		{
			if (!generic_param_names.insert(generic_param.m_lexeme).second)
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Struct declaration type error: duplicate generic parameter name", generic_param, m_file_name, m_source_lines));
			}
		}

		m_generic_structs.insert(struct_stmt.m_name.m_lexeme);
	}

	std::shared_ptr<MidoriType> struct_constructor_type = MidoriType::MakeFunctionType(struct_stmt.m_self_type->GetType<MidoriType::StructType>().m_member_types, std::move(struct_stmt.m_self_type));
	m_name_type_table.back()[struct_stmt.m_name.m_lexeme] = struct_constructor_type;

	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Union& union_stmt)
{
	if (!union_stmt.m_generic_params.empty())
	{
		std::unordered_set<std::string> generic_param_names;
		for (const Token& generic_param : union_stmt.m_generic_params)
		{
			if (!generic_param_names.insert(generic_param.m_lexeme).second)
			{
				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Union declaration type error: duplicate generic parameter name", generic_param, m_file_name, m_source_lines));
			}
		}

		m_generic_unions.insert(union_stmt.m_name.m_lexeme);
	}

	MidoriType::UnionType& union_type = union_stmt.m_self_type->GetType<MidoriType::UnionType>();
	for (auto& [member_name, member_ctx] : union_type.m_member_info)
	{
		std::vector<std::shared_ptr<MidoriType>> member_types_copy = member_ctx.m_member_types;
		std::shared_ptr<MidoriType> union_constructor_type = MidoriType::MakeFunctionType(std::move(member_types_copy), std::shared_ptr(union_stmt.m_self_type));
		m_name_type_table.back()[member_name] = union_constructor_type;
	}

	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Namespace& namespace_stmt)
{
	std::ranges::for_each
	(
		namespace_stmt.m_stmts,
		[this](const std::unique_ptr<MidoriStatement>& stmt) { (void)std::visit([this](auto&& arg) { return (*this)(arg); }, **stmt); }
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
				if (!arg_type->IsType<MidoriType::UnionType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Match expression type error: expected union type", match.m_match_keyword, m_file_name, m_source_lines, arg_type));
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
						MidoriExpression::Case& member_case = case_expr->GetExpression<MidoriExpression::Case>();
						const std::string& branch_name = member_case.m_member_name;
						if (!expected_member_names.contains(branch_name))
						{
							error = MidoriError::GenerateTypeCheckerErrorWithContext(std::format("Match expression type error: unrecognized member '{}'", branch_name), member_case.m_keyword, m_file_name, m_source_lines);
						}
						else
						{
							if (member_case.m_binding_names.size() != union_type.m_member_info.at(branch_name).m_member_types.size())
							{
								error = MidoriError::GenerateTypeCheckerErrorWithContext("Match expression type error: incorrect case arity", member_case.m_keyword, m_file_name, m_source_lines);
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
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Match expression type error: case types do not match", match.m_match_keyword, m_file_name, m_source_lines, *prev_case_type, case_result.value()));
						}
					}
				}

				if (!expected_member_names.empty() && !has_default_case)
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Not all union members are matched", match.m_match_keyword, m_file_name, m_source_lines));
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
			[&as, this](std::shared_ptr<MidoriType>&& expr_type) ->MidoriResult::TypeResult
			{
				if (as.m_to_type->IsType<MidoriType::StructType>() && expr_type->IsType<MidoriType::StructType>())
				{
					const MidoriType::StructType& from_struct_type = expr_type->GetType<MidoriType::StructType>();
					const MidoriType::StructType& to_struct_type = as.m_to_type->GetType<MidoriType::StructType>();
					if (to_struct_type.m_member_types.size() != from_struct_type.m_member_types.size())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Type cast expression type error: struct member count mismatch", as.m_as_keyword, m_file_name, m_source_lines, expr_type, as.m_to_type));
					}

					for (size_t i : std::views::iota(0u, to_struct_type.m_member_types.size()))
					{
						if (*from_struct_type.m_member_types[i] != *to_struct_type.m_member_types[i])
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Type cast expression type error: struct member type mismatch", as.m_as_keyword, m_file_name, m_source_lines, from_struct_type.m_member_types[i], to_struct_type.m_member_types[i]));
						}
					}
				}
				else if (as.m_to_type->IsType<MidoriType::StructType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Type cast expression type error: cannot cast to struct type", as.m_as_keyword, m_file_name, m_source_lines, expr_type, as.m_to_type));
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

										// Apply substitution to get concrete types if available
										std::shared_ptr<MidoriType> resolved_left = ApplySubstitution(left_type);
										std::shared_ptr<MidoriType> resolved_right = ApplySubstitution(right_type);

										// Handle array operations
										if (binary.m_op.m_token_name == Token::Name::STAR && resolved_left->IsType<MidoriType::ArrayType>() && resolved_right->IsType<MidoriType::IntegerType>())
										{
											self_type = left_type;
											return self_type;
										}

										self_type = left_type;

										if (std::ranges::contains(m_binary_partial_order_comparison_operators.cbegin(), m_binary_partial_order_comparison_operators.cend(), binary.m_op.m_token_name))
										{
											// Allow type variables (will be constrained by usage) or concrete numeric types
											std::shared_ptr<MidoriType> resolved_self = ApplySubstitution(self_type);
											if (!resolved_self->IsNumericType() && !resolved_self->IsType<MidoriType::TypeVariable>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Binary expression type error: expected numeric type", binary.m_op, m_file_name, m_source_lines, resolved_self, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
											}

											self_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
										}
										else if (std::ranges::contains(m_binary_arithmetic_operators.cbegin(), m_binary_arithmetic_operators.cend(), binary.m_op.m_token_name))
										{
											// Allow type variables (will be constrained by usage) or concrete numeric types
											std::shared_ptr<MidoriType> resolved_self = ApplySubstitution(self_type);
											if (!resolved_self->IsNumericType() && !resolved_self->IsType<MidoriType::TypeVariable>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Binary expression type error: expected numeric type", binary.m_op, m_file_name, m_source_lines, resolved_self, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
											}
										}
										else if (std::ranges::contains(m_binary_bitwise_operators.cbegin(), m_binary_bitwise_operators.cend(), binary.m_op.m_token_name))
										{
											std::shared_ptr<MidoriType> resolved_self = ApplySubstitution(self_type);
											if (!resolved_self->IsType<MidoriType::IntegerType>() && !resolved_self->IsType<MidoriType::TypeVariable>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Binary expression type error: expected integer type", binary.m_op, m_file_name, m_source_lines, resolved_self, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
											}
										}
										else if (std::ranges::contains(m_binary_equality_operators.cbegin(), m_binary_equality_operators.cend(), binary.m_op.m_token_name))
										{
											std::shared_ptr<MidoriType> resolved_self = ApplySubstitution(self_type);
											if (!resolved_self->IsNumericType() && !resolved_self->IsType<MidoriType::TextType>() && !resolved_self->IsType<MidoriType::BoolType>() && !resolved_self->IsType<MidoriType::TypeVariable>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Binary expression type error: expected comparable type", binary.m_op, m_file_name, m_source_lines, resolved_self, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>(), MidoriType::MakeLiteralType<MidoriType::TextType>(), MidoriType::MakeLiteralType<MidoriType::BoolType>()));
											}

											self_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
										}
										else if (std::ranges::contains(m_binary_logical_operators.cbegin(), m_binary_logical_operators.cend(), binary.m_op.m_token_name))
										{
											std::shared_ptr<MidoriType> resolved_left_logical = ApplySubstitution(left_type);
											if (!resolved_left_logical->IsType<MidoriType::BoolType>() && !resolved_left_logical->IsType<MidoriType::TypeVariable>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Binary expression type error: expected boolean type", binary.m_op, m_file_name, m_source_lines, resolved_left_logical, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
											}

											self_type = MidoriType::MakeLiteralType<MidoriType::BoolType>();
										}
										else if (std::ranges::contains(m_binary_concatenation_operators.cbegin(), m_binary_concatenation_operators.cend(), binary.m_op.m_token_name))
										{
											if (!resolved_left->IsType<MidoriType::TextType>() && !resolved_left->IsType<MidoriType::ArrayType>() && !resolved_left->IsType<MidoriType::TypeVariable>())
											{
												return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Binary expression type error: expected array or text", binary.m_op, m_file_name, m_source_lines, resolved_left));
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
			[this, &unary](std::shared_ptr<MidoriType>&& actual_type) -> MidoriResult::TypeResult
			{
				if (unary.m_op.m_token_name == Token::Name::AT)
				{
					if (!actual_type->IsType<MidoriType::ArrayType>())
					{
						// TODO: Generic array type
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unary prefix expression type error: expected array type", unary.m_op, m_file_name, m_source_lines, actual_type));
					}
				}
				else if (unary.m_op.m_token_name == Token::Name::SINGLE_MINUS || unary.m_op.m_token_name == Token::Name::SINGLE_PLUS)
				{
					if (!actual_type->IsNumericType())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unary operator requires numeric type", unary.m_op, m_file_name, m_source_lines, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
					}
				}
				else if (unary.m_op.m_token_name == Token::Name::BANG)
				{
					if (!actual_type->IsType<MidoriType::BoolType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Logical NOT operator requires boolean type", unary.m_op, m_file_name, m_source_lines, actual_type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
					}
				}
				else if (unary.m_op.m_token_name == Token::Name::TILDE)
				{
					if (!actual_type->IsType<MidoriType::IntegerType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Bitwise NOT operator requires integer type", unary.m_op, m_file_name, m_source_lines, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
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
				std::shared_ptr<MidoriType> resolved_type = ApplySubstitution(actual_type);

				if (!resolved_type->IsType<MidoriType::FunctionType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: not a callable", call.m_paren, m_file_name, m_source_lines, resolved_type));
				}

				MidoriType::FunctionType& function_type = resolved_type->GetType<MidoriType::FunctionType>();
				if (function_type.m_param_types.size() != call.m_arguments.size())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: incorrect arity", call.m_paren, m_file_name, m_source_lines));
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
			[this, &get](std::shared_ptr<MidoriType>&& actual_type) -> MidoriResult::TypeResult
			{
				if (!actual_type->IsType<MidoriType::StructType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Cannot access member on non-struct type", get.m_member_name, m_file_name, m_source_lines));
				}

				const MidoriType::StructType& struct_type = actual_type->GetType<MidoriType::StructType>();
				std::vector<std::string>::const_iterator find_result = std::find(struct_type.m_member_names.cbegin(), struct_type.m_member_names.cend(), get.m_member_name.m_lexeme);
				if (find_result == struct_type.m_member_names.cend())
				{
					std::string suggestion = std::format("Struct '{}' does not have a member named '{}'", struct_type.m_name, get.m_member_name.m_lexeme);
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unknown struct member", get.m_member_name, m_file_name, m_source_lines, suggestion));
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
			[&set, this](std::shared_ptr<MidoriType>&& actual_type) ->MidoriResult::TypeResult
			{
				if (!actual_type->IsType<MidoriType::StructType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Set expression type error: not a struct", set.m_member_name, m_file_name, m_source_lines, actual_type));
				}

				const MidoriType::StructType& struct_type = actual_type->GetType<MidoriType::StructType>();
				std::vector<std::string>::const_iterator find_result = std::find(struct_type.m_member_names.cbegin(), struct_type.m_member_names.cend(), set.m_member_name.m_lexeme);
				if (find_result == struct_type.m_member_names.cend())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Set expression type error: struct does not have member", set.m_member_name, m_file_name, m_source_lines, actual_type));
				}

				set.m_index = static_cast<int>(find_result - struct_type.m_member_names.cbegin());

				const std::shared_ptr<MidoriType>& member_type = struct_type.m_member_types[static_cast<size_t>(set.m_index)];

				if (*actual_type != *member_type)
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Set expression type error: type mismatch", set.m_member_name, m_file_name, m_source_lines, actual_type, member_type));
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
			if (m_generic_functions.contains(variable.m_name.m_lexeme))
			{
				variable.m_type_data = Freshen(var->second);
			}
			else
			{
				// Apply substitution to get the most up-to-date type
				// This handles cases where the type contains type variables that have been unified
				variable.m_type_data = var->second;  // Just use the type from environment directly for now
			}
			return variable.m_type_data;
		}
	}

	return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("BoundedName expression type error: variable not found", variable.m_name, m_file_name, m_source_lines));
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

				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Bind expression type error: variable not found", bind.m_name, m_file_name, m_source_lines));
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
	// Lambda expressions (fn) cannot have generic parameters
	if (!function.m_generic_params.empty())
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Function expression type error: lambda expressions cannot have generic parameters. Use 'defun' instead.", function.m_function_keyword, m_file_name, m_source_lines));
	}

	// Freshen any UndecidedType parameters to TypeVariables
	for (std::shared_ptr<MidoriType>& param_type : function.m_param_types)
	{
		param_type = Freshen(param_type);
	}
	function.m_return_type = Freshen(function.m_return_type);

	// Create the function type and store it so it can be returned
	std::shared_ptr<MidoriType> return_type_copy = function.m_return_type;
	function.m_type_data = MidoriType::MakeFunctionType(function.m_param_types, std::move(return_type_copy));

	BeginScope();
	std::ranges::for_each
	(
		std::views::iota(0u, function.m_params.size()),
		[&function, this](size_t idx) {m_name_type_table.back().emplace(function.m_params[idx].m_lexeme, function.m_param_types[idx]); }
	);

    return std::visit([this](auto&& arg) { return (*this)(arg); }, **function.m_body)
		.and_then
		(
			[&function, this](std::shared_ptr<MidoriType>&& function_return_value_type) ->MidoriResult::TypeResult
			{
				EndScope();

				return Unify(function.m_function_keyword, function.m_return_type, function_return_value_type)
					.and_then
					(
						[&function](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
						{
							return function.m_type_data;
						}
					);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Construct& construct)
{
	const std::shared_ptr<MidoriType>& return_type = construct.m_return_type;
	std::optional<std::shared_ptr<MidoriType>> constructor_type_shared = std::nullopt;
	std::string constructor_name;
	std::string actual_type_name; 

	for (TypeChecker::TypeEnvironmentStack::const_reverse_iterator it = m_name_type_table.crbegin(); it != m_name_type_table.crend(); ++it)
	{
		const TypeEnvironment& env = *it;
		TypeEnvironment::const_iterator var;
		if (construct.IsConstructTypeOf<MidoriExpression::Construct::Struct>())
		{
			actual_type_name = return_type->GetType<MidoriType::StructType>().m_name;
			constructor_name = actual_type_name;
			var = env.find(constructor_name);
		}
		else
		{
			constructor_name = construct.m_data_name.m_lexeme;
			actual_type_name = return_type->GetType<MidoriType::UnionType>().m_name;
			var = env.find(constructor_name);
		}
		if (var != env.end())
		{
			bool is_generic = false;
			if (construct.IsConstructTypeOf<MidoriExpression::Construct::Struct>())
			{
				is_generic = m_generic_structs.contains(actual_type_name);
			}
			else
			{
				is_generic = m_generic_unions.contains(actual_type_name);
			}

			if (is_generic)
			{
				constructor_type_shared = Freshen(var->second);
			}
			else
			{
				constructor_type_shared = var->second;
			}
			break;
		}
	}

	if (!constructor_type_shared.has_value())
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Construct expression type error: struct or union not found", construct.m_data_name, m_file_name, m_source_lines));
	}

	const MidoriType::FunctionType& constructor_type = constructor_type_shared.value()->GetType<MidoriType::FunctionType>();

	if (constructor_type.m_param_types.size() != construct.m_params.size())
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Construct expression type error: incorrect arity", construct.m_data_name, m_file_name, m_source_lines));
	}

	for (size_t idx : std::views::iota(0u, construct.m_params.size()))
	{
		std::unique_ptr<MidoriExpression>& param = construct.m_params[idx];
		MidoriResult::TypeResult param_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **param);
		if (!param_result.has_value())
		{
			return param_result;
		}

		std::shared_ptr<MidoriType> param_type = constructor_type.m_param_types[idx];
		MidoriResult::TypeResult unify_result = Unify(construct.m_data_name, param_result.value(), param_type);
		if (!unify_result.has_value())
		{
			return unify_result;
		}
	}

	// For generic structs, apply substitution to the return type to get the monomorphized type
	// After unifying parameters, type variables have been substituted with concrete types
	construct.m_type_data = ApplySubstitution(constructor_type.m_return_type);
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
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Array expression type error: inconsistent element types", array.m_op, m_file_name, m_source_lines, element_results[idx], element_results[0u]));
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
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Array get expression type error: index must be integer", array_get.m_op, m_file_name, m_source_lines, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
					}
				}

				for (int i = 0; i < indices_size; i += 1)
				{
					array_var_type = ApplySubstitution(array_var_type);

					if (!array_var_type->IsType<MidoriType::ArrayType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Array get expression type error: expected array type", array_get.m_op, m_file_name, m_source_lines, array_var_type));
					}

					array_var_type = array_var_type->GetType<MidoriType::ArrayType>().m_element_type;
				}

				// Apply final substitution to resolve the element type
				array_get.m_type_data = ApplySubstitution(array_var_type);
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
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Array set expression type error: index must be integer", array_set.m_op, m_file_name, m_source_lines, actual_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>()));
								}
							}

							for (size_t i = 0u; i < array_set.m_indices.size(); i += 1u)
							{
								if (!array_var_type->IsType<MidoriType::ArrayType>())
								{
									return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Array set expression type error: expected array type", array_set.m_op, m_file_name, m_source_lines, array_var_type));
								}

								array_var_type = array_var_type->GetType<MidoriType::ArrayType>().m_element_type;
							}

							if (*array_var_type != *value_type)
							{
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Array set expression type error: value type mismatch", array_set.m_op, m_file_name, m_source_lines, value_type, array_var_type));
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
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("IfElse expression type error: condition must be boolean", if_else.m_if_token, m_file_name, m_source_lines, type, MidoriType::MakeLiteralType<MidoriType::BoolType>()));
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
		// Note: Statement types are checked but do NOT contribute to the block's type
		// Only the final expression (if present) determines the block type
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

		// Blocks without final expressions have Unit type
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
				if (m_expected_return_type)
				{
					return Unify(return_expr.m_keyword, m_expected_return_type, type)
						.and_then
						(
							[&return_expr](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
							{
								return_expr.m_type_data = MidoriType::MakeLiteralType<MidoriType::NeverType>();
								return return_expr.m_type_data;
							}
						);
				}
				else
				{
					return Unify(return_expr.m_keyword, return_expr.m_type_data, type);
				}
			}
		);
}