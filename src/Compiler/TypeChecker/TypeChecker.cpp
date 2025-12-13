#include <algorithm>
#include <format>
#include <iterator>
#include <ranges>

#include "Common/Constant/Constant.h"
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
	else if (left_subst->IsType<MidoriType::TupleType>() && right_subst->IsType<MidoriType::TupleType>())
	{
		MidoriType::TupleType& left_tuple = left_subst->GetType<MidoriType::TupleType>();
		MidoriType::TupleType& right_tuple = right_subst->GetType<MidoriType::TupleType>();

		// Tuple types must have the same number of elements
		if (left_tuple.m_element_types.size() != right_tuple.m_element_types.size())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Unable to unify tuples with different element counts", token, m_file_name, m_source_lines, left_subst, right_subst));
		}

		// Unify each element type
		for (size_t idx : std::views::iota(0u, left_tuple.m_element_types.size()))
		{
			MidoriResult::TypeResult result = Unify(token, left_tuple.m_element_types[idx], right_tuple.m_element_types[idx]);
			if (!result.has_value())
			{
				return result;
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
	else if (type->IsType<MidoriType::TupleType>())
	{
		MidoriType::TupleType& tuple_type = type->GetType<MidoriType::TupleType>();
		std::vector<std::shared_ptr<MidoriType>> fresh_element_types;
		std::ranges::for_each
		(
			tuple_type.m_element_types,
			[&fresh_element_types, &context, this](const std::shared_ptr<MidoriType>& element_type)
			{
				fresh_element_types.emplace_back(Freshen(element_type, context));
			}
		);
		return MidoriType::MakeTupleType(std::move(fresh_element_types));
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
			// Mark this as a generic instantiation if the original had generic params
			if (!struct_type.m_generic_params.empty() || struct_type.m_is_generic_instantiation)
			{
				new_struct->GetType<MidoriType::StructType>().m_is_generic_instantiation = true;
			}
			return new_struct;
		}
		cache[type.get()] = type;
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
		cache[type.get()] = type;
		return type;
	}
	else if (type->IsType<MidoriType::TupleType>())
	{
		MidoriType::TupleType& tuple_type = type->GetType<MidoriType::TupleType>();
		bool changed = false;
		std::vector<std::shared_ptr<MidoriType>> new_element_types;

		for (const std::shared_ptr<MidoriType>& element_type : tuple_type.m_element_types)
		{
			std::shared_ptr<MidoriType> subst_element = ApplySubstitution(element_type, cache);
			new_element_types.push_back(subst_element);
			if (subst_element != element_type)
			{
				changed = true;
			}
		}

		if (changed)
		{
			return MidoriType::MakeTupleType(std::move(new_element_types));
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
	else if (subst_type->IsType<MidoriType::TupleType>())
	{
		MidoriType::TupleType& tuple_type = subst_type->GetType<MidoriType::TupleType>();
		for (const std::shared_ptr<MidoriType>& element_type : tuple_type.m_element_types)
		{
			if (OccursCheck(var_id, element_type))
			{
				return true;
			}
		}
		return false;
	}

	return false;
}

TypeChecker::ClassInfo::ClassInfo(const std::string& name, std::vector<std::string>&& params, std::vector<MidoriType::ClassConstraint>&& supers, std::unordered_map<std::string, std::shared_ptr<MidoriType>>&& methods, std::unordered_set<std::string>&& defaults)
	: m_name(name),
	m_type_param_names(std::move(params)),
	m_superclasses(std::move(supers)),
	m_method_types(std::move(methods)),
	m_methods_with_defaults(std::move(defaults))
{
}

TypeChecker::InstanceInfo::InstanceInfo(const std::string& tc_name, std::vector<std::shared_ptr<MidoriType>>&& args, std::vector<MidoriType::ClassConstraint>&& constraints, std::unordered_map<std::string, std::unique_ptr<MidoriStatement>>&& methods)
	: m_class_name(tc_name),
	m_type_args(std::move(args)),
	m_constraints(std::move(constraints)),
	m_method_impls(std::move(methods))
{
}

bool TypeChecker::InstanceKey::operator==(const InstanceKey& other) const
{
	return m_class_name == other.m_class_name && m_concrete_types == other.m_concrete_types;
}

// InstanceKey hash function
std::size_t TypeChecker::InstanceKeyHash::operator()(const InstanceKey& key) const
{
	std::size_t hash = std::hash<std::string>{}(key.m_class_name);
	for (const auto& type : key.m_concrete_types)
	{
		hash ^= std::hash<std::string>{}(type) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
	}
	return hash;
}

TypeChecker::TypeChecker(
	MidoriProgramTree&& parser_result,
	std::string_view file_name,
	const std::vector<std::string>& source_lines,
	TypeEnvironment imported_types,
	const std::unordered_map<std::string, ClassInfo>& imported_typeclasses
)
	: m_program_tree(std::move(parser_result)),
	m_source_lines(source_lines),
	m_next_type_var_id(0),
	m_file_name(file_name),
	m_classes(imported_typeclasses)
{
	// Pre-populate type environment with imported types
	if (!imported_types.empty())
	{
		m_name_type_table.push_back(std::move(imported_types));
	}
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

// Extract type signatures from parsed AST without full type checking
// This allows parallel type checking of dependent modules
TypeChecker::TypeEnvironment TypeChecker::ExtractTypeSignatures(const MidoriProgramTree& ast)
{
	TypeEnvironment signatures;

	for (const std::unique_ptr<MidoriStatement>& statement : ast)
	{
		std::visit(
			[&signatures](const auto& stmt) {
				using T = std::decay_t<decltype(stmt)>;

				if constexpr (std::is_same_v<T, MidoriStatement::DefineFunction>)
				{
					// Extract function signature
					signatures[stmt.m_name.m_lexeme] = MidoriType::MakeFunctionType(
						stmt.m_param_types,
						std::shared_ptr<MidoriType>(stmt.m_return_type)
					);
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::Struct>)
				{
					// Struct already has its complete type in m_self_type
					signatures[stmt.m_name.m_lexeme] = stmt.m_self_type;
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::Union>)
				{
					// Union already has its complete type in m_self_type
					signatures[stmt.m_name.m_lexeme] = stmt.m_self_type;
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::Foreign>)
				{
					// Extract foreign function signature
					signatures[stmt.m_function_name.m_lexeme] = stmt.m_type;
				}
				// Other statement types (Simple, Define, etc.) don't export types
			},
			**statement
		);
	}

	return signatures;
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

					// Reject NeverType values with concrete type annotations
					// NeverType represents expressions that never return (infinite loops, unconditional returns/breaks)
					// They cannot be assigned to variables with concrete types
					if (type->IsType<MidoriType::NeverType>() && !annotated_type->IsType<MidoriType::NeverType>())
					{
						return std::unexpected<std::string>(
							MidoriError::GenerateTypeCheckerErrorWithContext(
								"Cannot assign a never-returning expression to a variable with type " + annotated_type->ToString(),
								def.m_name,
								m_file_name,
								m_source_lines
							)
						);
					}

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

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::DefineTuple& def_tuple)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **def_tuple.m_value)
		.and_then
		(
			[&def_tuple, this](std::shared_ptr<MidoriType>&& type)->MidoriResult::TypeResult
			{
				if (!type->IsType<MidoriType::TupleType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("DefineTuple statement type error: expected tuple expression on right-hand side", def_tuple.m_names[0], m_file_name, m_source_lines));
				}

				const MidoriType::TupleType& tuple_type = type->GetType<MidoriType::TupleType>();


				if (def_tuple.m_names.size() != tuple_type.m_element_types.size())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("DefineTuple statement type error: tuple pattern has " + std::to_string(def_tuple.m_names.size()) + " bindings but tuple has " + std::to_string(tuple_type.m_element_types.size()) + " elements", def_tuple.m_names[0u], m_file_name, m_source_lines));
				}

				for (size_t i = 0u; i < def_tuple.m_names.size(); i += 1u)
				{
					m_name_type_table.back().emplace(def_tuple.m_names[i].m_lexeme, tuple_type.m_element_types[i]);
				}

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

	for (const MidoriType::ClassConstraint& constraint : defun.m_constraints)
	{
		if (!m_classes.contains(constraint.m_class_name))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("DefineFunction type error: undefined class '" + constraint.m_class_name + "' in constraint", defun.m_name, m_file_name, m_source_lines));
		}

		const ClassInfo& tc_info = m_classes.at(constraint.m_class_name);

		if (constraint.m_type_args.size() != tc_info.m_type_param_names.size())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("DefineFunction type error: class '" + constraint.m_class_name + "' expects " + std::to_string(tc_info.m_type_param_names.size()) + " type argument(s) but got " + std::to_string(constraint.m_type_args.size()), defun.m_name, m_file_name, m_source_lines));
		}
	}

	FresheningContext freshening_context;
	for (std::shared_ptr<MidoriType>& param_type : defun.m_param_types)
	{
		param_type = Freshen(param_type, freshening_context);
	}
	defun.m_return_type = Freshen(defun.m_return_type, freshening_context);

	std::shared_ptr<MidoriType> return_type_copy = defun.m_return_type;
	m_name_type_table.back()[defun.m_name.m_lexeme] = MidoriType::MakeFunctionType(defun.m_param_types, std::move(return_type_copy));
	if (!defun.m_generic_params.empty())
	{
		m_generic_functions.insert(defun.m_name.m_lexeme);
	}

	BeginScope();

	for (const Token& generic_param : defun.m_generic_params)
	{
		const std::string& param_name = generic_param.m_lexeme;
		std::unordered_map<std::string, std::shared_ptr<MidoriType>>::iterator it = freshening_context.m_generic_params.find(param_name);
		if (it == freshening_context.m_generic_params.end())
		{
			std::shared_ptr<MidoriType> fresh_var = FreshTypeVar();
			it = freshening_context.m_generic_params.emplace(param_name, std::move(fresh_var)).first;
		}
		m_name_type_table.back().emplace(param_name, it->second);
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

	size_t prev_constraints_size = m_active_constraints.size();
	m_active_constraints.insert(m_active_constraints.end(), defun.m_constraints.begin(), defun.m_constraints.end());

	return std::visit([this](auto&& arg) { return (*this)(arg); }, **defun.m_body)
		.and_then
		(
			[&defun, &saved_expected_return_type, prev_constraints_size, this](std::shared_ptr<MidoriType>&& function_return_value_type) ->MidoriResult::TypeResult
			{
				EndScope();
				m_expected_return_type = saved_expected_return_type;
				m_active_constraints.resize(prev_constraints_size);

				// If the body contains a return statement, the return statement itself
				// validates the return type, so we don't need to check the body's natural type
				bool body_contains_return = defun.m_body->Contains<MidoriExpression::Return>();

				if (body_contains_return)
				{
					return defun.m_return_type;
				}
				else
				{
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
			[&saved_expected_return_type, prev_constraints_size, this](std::string&& error) -> MidoriResult::TypeResult
			{
				m_expected_return_type = saved_expected_return_type; 
				m_active_constraints.resize(prev_constraints_size);
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

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Class& class_stmt)
{
	std::unordered_set<std::string> type_param_names;
	for (const Token& type_param : class_stmt.m_type_params)
	{
		if (!type_param_names.insert(type_param.m_lexeme).second)
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Class declaration error: duplicate type parameter name", type_param, m_file_name, m_source_lines));
		}
	}

	if (m_classes.contains(class_stmt.m_name.m_lexeme))
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Class declaration error: class already defined", class_stmt.m_name, m_file_name, m_source_lines));
	}

	std::unordered_map<std::string, std::shared_ptr<MidoriType>> method_types;
	std::unordered_set<std::string> methods_with_defaults;
	for (std::unique_ptr<MidoriStatement>& method : class_stmt.m_methods)
	{
		if (!method->IsStatement<MidoriStatement::DefineFunction>())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Class declaration error: methods must be function definitions", class_stmt.m_name, m_file_name, m_source_lines));
		}

		MidoriStatement::DefineFunction& defun = method->GetStatement<MidoriStatement::DefineFunction>();
		if (method_types.contains(defun.m_name.m_lexeme))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Class declaration error: duplicate method name", defun.m_name, m_file_name, m_source_lines));
		}

		std::shared_ptr<MidoriType> method_type = MidoriType::MakeFunctionType(defun.m_param_types, std::shared_ptr<MidoriType>(defun.m_return_type));

		method_types[defun.m_name.m_lexeme] = method_type;
	}

	std::vector<std::string> param_names;
	for (const Token& param : class_stmt.m_type_params)
	{
		param_names.emplace_back(param.m_lexeme);
	}

	m_classes[class_stmt.m_name.m_lexeme] = ClassInfo(class_stmt.m_name.m_lexeme, std::move(param_names), std::vector<MidoriType::ClassConstraint>(class_stmt.m_superclasses), std::move(method_types), std::move(methods_with_defaults));

	return MidoriType::MakeUndecidedType();
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriStatement::Instance& instance_stmt)
{
	std::unordered_map<std::string, ClassInfo>::iterator tc_it = m_classes.find(instance_stmt.m_class_name.m_lexeme);
	if (tc_it == m_classes.end())
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: unknown class '" + instance_stmt.m_class_name.m_lexeme + "'", instance_stmt.m_class_name, m_file_name, m_source_lines));
	}

	const ClassInfo& tc_info = tc_it->second;
	if (instance_stmt.m_type_args.size() != tc_info.m_type_param_names.size())
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: type argument count mismatch", instance_stmt.m_class_name, m_file_name, m_source_lines));
	}

	std::vector<std::string> concrete_type_names;
	for (const std::shared_ptr<MidoriType>& type_arg : instance_stmt.m_type_args)
	{
		concrete_type_names.push_back(type_arg->ToString());
	}

	InstanceKey instance_key{instance_stmt.m_class_name.m_lexeme, concrete_type_names};

	if (m_instances.contains(instance_key))
	{
		return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: instance already defined for this type", instance_stmt.m_class_name, m_file_name, m_source_lines));
	}

	std::unordered_set<std::string> implemented_methods;
	for (std::unique_ptr<MidoriStatement>& method : instance_stmt.m_methods)
	{
		if (!method->IsStatement<MidoriStatement::DefineFunction>())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: methods must be function definitions", instance_stmt.m_class_name, m_file_name, m_source_lines));
		}

		MidoriStatement::DefineFunction& defun = method->GetStatement<MidoriStatement::DefineFunction>();
		std::string mangled_name = defun.m_name.m_lexeme;
		std::string method_name = MidoriType::DemangleInstanceMethodName(mangled_name, instance_stmt.m_class_name.m_lexeme);

		if (!tc_info.m_method_types.contains(method_name))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: method '" + method_name + "' not defined in typeclass", defun.m_name, m_file_name, m_source_lines));
		}

		if (implemented_methods.contains(method_name))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: duplicate method implementation", defun.m_name, m_file_name, m_source_lines));
		}

		implemented_methods.emplace(method_name);
	}

	for (const auto& [method_name, method_type] : tc_info.m_method_types)
	{
		if (!implemented_methods.contains(method_name))
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: missing implementation for method '" + method_name + "'", instance_stmt.m_class_name, m_file_name, m_source_lines));
		}
	}

	std::unordered_map<std::string, std::shared_ptr<MidoriType>> type_param_substitutions;
	for (size_t i = 0u; i < tc_info.m_type_param_names.size(); i += 1u)
	{
		type_param_substitutions.emplace(tc_info.m_type_param_names[i], instance_stmt.m_type_args[i]);
	}

	for (const std::unique_ptr<MidoriStatement>& method : instance_stmt.m_methods)
	{
		const MidoriStatement::DefineFunction& defun = method->GetStatement<MidoriStatement::DefineFunction>();
		const std::string mangled_name = defun.m_name.m_lexeme;
		const std::string method_name = MidoriType::DemangleInstanceMethodName(mangled_name, instance_stmt.m_class_name.m_lexeme);

		if (!defun.m_generic_params.empty())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: instance methods cannot declare generic parameters", defun.m_name, m_file_name, m_source_lines));
		}

		std::unordered_map<std::string, std::shared_ptr<MidoriType>>::const_iterator expected_it = tc_info.m_method_types.find(method_name);
		if (expected_it == tc_info.m_method_types.cend())
		{
			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: method '" + method_name + "' not defined in class", defun.m_name, m_file_name, m_source_lines));
		}

		std::shared_ptr<MidoriType> expected_signature = MidoriType::SubstituteTypeParams(expected_it->second, type_param_substitutions);
		std::shared_ptr<MidoriType> actual_signature = MidoriType::MakeFunctionType(defun.m_param_types, std::shared_ptr<MidoriType>(defun.m_return_type));

		if (*expected_signature != *actual_signature)
		{
			if (expected_signature->IsType<MidoriType::FunctionType>() && actual_signature->IsType<MidoriType::FunctionType>())
			{
				const MidoriType::FunctionType& expected_type = expected_signature->GetType<MidoriType::FunctionType>();
				const MidoriType::FunctionType& actual_type = actual_signature->GetType<MidoriType::FunctionType>();

				if (expected_type.m_param_types.size() != actual_type.m_param_types.size())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: method '" + method_name + "' parameter count does not match class declaration", defun.m_name, m_file_name, m_source_lines, actual_signature, expected_signature));
				}

				for (size_t i = 0u; i < expected_type.m_param_types.size(); i += 1u)
				{
					const std::shared_ptr<MidoriType>& expected_param = expected_type.m_param_types[i];
					const std::shared_ptr<MidoriType>& actual_param = actual_type.m_param_types[i];
					if (*expected_param != *actual_param)
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext(std::format("Instance declaration error: method '{}' parameter {} type does not match class declaration", method_name, i + 1u), defun.m_name, m_file_name, m_source_lines, actual_param, expected_param));
					}
				}

				if (*expected_type.m_return_type != *actual_type.m_return_type)
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: method '" + method_name + "' return type does not match class declaration", defun.m_name, m_file_name, m_source_lines, actual_type.m_return_type, expected_type.m_return_type));
				}
			}

			return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Instance declaration error: method '" + method_name + "' signature does not match class declaration", defun.m_name, m_file_name, m_source_lines, actual_signature, expected_signature));
		}
	}

	for (const std::unique_ptr<MidoriStatement>& method : instance_stmt.m_methods)
	{
		MidoriResult::TypeResult result = std::visit([this](auto&& arg) { return (*this)(arg); }, **method);
		if (!result.has_value())
		{
			return result;
		}
	}

	// Store instance metadata (methods will be compiled separately by CodeGenerator)
	m_instances[instance_key] = InstanceInfo(instance_stmt.m_class_name.m_lexeme, std::vector<std::shared_ptr<MidoriType>>(instance_stmt.m_type_args), std::vector<MidoriType::ClassConstraint>(instance_stmt.m_constraints), std::unordered_map<std::string, std::unique_ptr<MidoriStatement>>());
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
	// Save the outer loop's expected break type (for nested loops)
	std::shared_ptr<MidoriType> outer_break_type = m_expected_break_type;

	// Set the expected break type for this loop (initially undecided)
	m_expected_break_type = loop.m_type_data;

	MidoriResult::TypeResult result = std::visit([this](auto&& arg) { return (*this)(arg); }, **loop.m_body)
		.and_then
		(
			[&loop, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
			{
				// The loop's type is determined by the expected break type
				// If no breaks occurred, m_expected_break_type is still undecided
				if (m_expected_break_type->IsType<MidoriType::UndecidedType>())
				{
					// No breaks in this loop - it's an infinite loop with NeverType
					loop.m_type_data = MidoriType::MakeLiteralType<MidoriType::NeverType>();
				}
				else
				{
					// Loop has breaks - use the unified break type
					loop.m_type_data = m_expected_break_type;
				}

				return loop.m_type_data;
			}
		);

	// Restore the outer loop's expected break type
	m_expected_break_type = outer_break_type;

	return result;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::For& for_expr)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **for_expr.m_range)
		.and_then
		(
			[&for_expr, this](std::shared_ptr<MidoriType>&& range_type)->MidoriResult::TypeResult
			{
				std::shared_ptr<MidoriType> resolved_range = ApplySubstitution(range_type);

				if (!resolved_range->IsType<MidoriType::RangeType>())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("For loop expression type error: expected Range type for iteration", for_expr.m_in_keyword, m_file_name, m_source_lines, resolved_range, MidoriType::MakeRangeType(MidoriType::MakeLiteralType<MidoriType::IntegerType>())));
				}

				std::shared_ptr<MidoriType> element_type = resolved_range->GetType<MidoriType::RangeType>().m_element_type;

				BeginScope();

				std::string var_name(for_expr.m_loop_variable.m_lexeme);
				m_name_type_table.back()[var_name] = element_type;

				std::shared_ptr<MidoriType> outer_break_type = m_expected_break_type;

				m_expected_break_type = for_expr.m_type_data;

				return std::visit([this](auto&& arg) { return (*this)(arg); }, **for_expr.m_body)
					.and_then
					(
						[&for_expr, outer_break_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
						{
							// The for loop's type is determined by the expected break type
							// If no breaks occurred, the loop completes normally and returns Unit
							if (m_expected_break_type->IsType<MidoriType::UndecidedType>())
							{
								for_expr.m_type_data = MidoriType::MakeLiteralType<MidoriType::UnitType>();
							}
							else
							{
								for_expr.m_type_data = m_expected_break_type;
							}

							EndScope();

							m_expected_break_type = outer_break_type;

							return for_expr.m_type_data;
						}
					);
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
				// Check for Convertable<From, To> instance
				InstanceKey conversion_key{
					"Convertable",
					{expr_type->ToString(), as.m_to_type->ToString()}
				};

				std::unordered_map<InstanceKey, InstanceInfo, InstanceKeyHash>::iterator instance_it =
					m_instances.find(conversion_key);
				bool has_convertable_instance = (instance_it != m_instances.end());

				// Check if this is a built-in conversion
				bool is_builtin_conversion = false;

				// Struct to struct conversions - only check structural compatibility if NO Convertable instance
				if (as.m_to_type->IsType<MidoriType::StructType>() && expr_type->IsType<MidoriType::StructType>())
				{
					// If there's a Convertable instance, use that instead of structural conversion
					if (!has_convertable_instance)
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
						is_builtin_conversion = true;
					}
				}
				else if (as.m_to_type->IsType<MidoriType::StructType>() && !has_convertable_instance)
				{
					// Only error if no Convertable instance exists
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Type cast expression type error: cannot cast to struct type", as.m_as_keyword, m_file_name, m_source_lines, expr_type, as.m_to_type));
				}

				// Check for built-in primitive conversions
				if ((expr_type->IsType<MidoriType::IntegerType>() && (as.m_to_type->IsType<MidoriType::FloatType>() || as.m_to_type->IsType<MidoriType::TextType>())) ||
					(expr_type->IsType<MidoriType::FloatType>() && (as.m_to_type->IsType<MidoriType::IntegerType>() || as.m_to_type->IsType<MidoriType::TextType>())) ||
					(expr_type->IsType<MidoriType::TextType>() && (as.m_to_type->IsType<MidoriType::IntegerType>() || as.m_to_type->IsType<MidoriType::FloatType>())))
				{
					is_builtin_conversion = true;
				}

				// Verify that either Convertable instance exists or it's a built-in conversion
				if (!has_convertable_instance && !is_builtin_conversion)
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext(
						std::format("No conversion from {} to {}. Define 'instance Convertable<{}, {}>' to enable this conversion.",
							expr_type->ToString(), as.m_to_type->ToString(),
							expr_type->ToString(), as.m_to_type->ToString()),
						as.m_as_keyword, m_file_name, m_source_lines, expr_type, as.m_to_type));
				}

				as.m_from_type = expr_type;
				as.m_type_data = as.m_to_type;
				as.m_uses_convertable = has_convertable_instance;
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

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::Tuple& tuple)
{
	if (tuple.m_elements.empty())
	{
		// Empty tuple is Unit type
		tuple.m_type_data = MidoriType::MakeLiteralType<MidoriType::UnitType>();
		return tuple.m_type_data;
	}

	std::vector<std::shared_ptr<MidoriType>> element_types;
	element_types.reserve(tuple.m_elements.size());

	for (std::unique_ptr<MidoriExpression>& element : tuple.m_elements)
	{
		MidoriResult::TypeResult result = std::visit([this](auto&& arg) { return (*this)(arg); }, **element);
		if (!result.has_value())
		{
			return result;
		}

		element_types.emplace_back(std::move(result.value()));
	}

	tuple.m_type_data = MidoriType::MakeTupleType(std::move(element_types));
	return tuple.m_type_data;
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::UnaryPrefix& unary)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **unary.m_expr)
		.and_then
		(
			[this, &unary](std::shared_ptr<MidoriType>&& actual_type) -> MidoriResult::TypeResult
			{
				if (unary.m_op.m_token_name == Token::Name::SINGLE_MINUS || unary.m_op.m_token_name == Token::Name::SINGLE_PLUS)
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
	if (call.m_callee->IsExpression<MidoriExpression::BoundedName>())
	{
		MidoriExpression::BoundedName& callee_name = call.m_callee->GetExpression<MidoriExpression::BoundedName>();
		const std::string& full_name = callee_name.m_name.m_lexeme;
		size_t separator_pos = full_name.rfind(NameSeparator.data());
		if (separator_pos != std::string::npos)
		{
			std::string qualifier = full_name.substr(0u, separator_pos);
			std::string method_name = full_name.substr(separator_pos + NameSeparator.length());

			std::vector<const MidoriType::ClassConstraint*> matching_constraints;
			for (const MidoriType::ClassConstraint& constraint : m_active_constraints)
			{
				if (constraint.m_class_name == qualifier)
				{
					matching_constraints.emplace_back(&constraint);
				}
			}

			if (!matching_constraints.empty())
			{
				std::unordered_map<std::string, ClassInfo>::iterator tc_it = m_classes.find(qualifier);
				if (tc_it == m_classes.end())
				{
					return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: unknown class '" + qualifier + "'", call.m_paren, m_file_name, m_source_lines));
				}

				const ClassInfo& tc_info = tc_it->second;
				std::unordered_map<std::string, std::shared_ptr<MidoriType>>::const_iterator method_it = tc_info.m_method_types.find(method_name);
				if (method_it != tc_info.m_method_types.cend())
				{
					std::vector<std::shared_ptr<MidoriType>> arg_results;
					arg_results.reserve(call.m_arguments.size());
					for (std::unique_ptr<MidoriExpression>& call_arg : call.m_arguments)
					{
						MidoriResult::TypeResult arg_result = std::visit([this](auto&& arg) { return (*this)(arg); }, **call_arg);
						if (!arg_result.has_value())
						{
							return arg_result;
						}
						arg_results.emplace_back(std::move(arg_result.value()));
					}

					std::unordered_map<std::string, std::shared_ptr<MidoriType>> env_substitutions;
					for (TypeChecker::TypeEnvironmentStack::reverse_iterator it = m_name_type_table.rbegin(); it != m_name_type_table.rend(); ++it)
					{
						for (const TypeEnvironment::value_type& entry : *it)
						{
							const std::string& name = entry.first;
							const std::shared_ptr<MidoriType>& type = entry.second;

							if (!env_substitutions.contains(name))
							{
								env_substitutions.emplace(name, type);
							}
						}
					}

					std::vector<const MidoriType::ClassConstraint*> selected_constraints;
					if (!arg_results.empty())
					{
						std::shared_ptr<MidoriType> first_arg_type = ApplySubstitution(arg_results[0u]);
						for (const MidoriType::ClassConstraint* constraint : matching_constraints)
						{
							if (constraint->m_type_args.empty())
							{
								continue;
							}

							std::shared_ptr<MidoriType> substituted_first = MidoriType::SubstituteTypeParams(constraint->m_type_args[0u], env_substitutions);
							std::shared_ptr<MidoriType> resolved_first = ApplySubstitution(substituted_first);

							if (*resolved_first == *first_arg_type)
							{
								selected_constraints.emplace_back(constraint);
							}
						}
					}
					else
					{
						selected_constraints = matching_constraints;
					}

					if (selected_constraints.empty())
					{
						std::string first_arg_name = arg_results.empty() ? std::string("no arguments") : ApplySubstitution(arg_results[0u])->ToString();
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: no matching class constraint for '" + qualifier + NameSeparator.data() + method_name + "' and argument type '" + first_arg_name + "'", call.m_paren, m_file_name, m_source_lines));
					}
					if (selected_constraints.size() != 1u)
					{
						std::string first_arg_name = arg_results.empty() ? std::string("no arguments") : ApplySubstitution(arg_results[0u])->ToString();
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: ambiguous class method '" + qualifier + NameSeparator.data() + method_name + "' for argument type '" + first_arg_name + "'", call.m_paren, m_file_name, m_source_lines));
					}

					const MidoriType::ClassConstraint& selected_constraint = *selected_constraints[0u];
					if (selected_constraint.m_type_args.size() != tc_info.m_type_param_names.size())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: constraint type argument count mismatch for class '" + qualifier + "'", call.m_paren, m_file_name, m_source_lines));
					}

					std::unordered_map<std::string, std::shared_ptr<MidoriType>> class_substitutions;
					for (size_t i = 0u; i < tc_info.m_type_param_names.size(); i += 1u)
					{
						std::shared_ptr<MidoriType> resolved_type_arg = MidoriType::SubstituteTypeParams(selected_constraint.m_type_args[i], env_substitutions);
						class_substitutions.emplace(tc_info.m_type_param_names[i], ApplySubstitution(resolved_type_arg));
					}

					std::shared_ptr<MidoriType> substituted_method_type = MidoriType::SubstituteTypeParams(method_it->second, class_substitutions);
					std::shared_ptr<MidoriType> resolved_method_type = ApplySubstitution(substituted_method_type);
					if (!resolved_method_type->IsType<MidoriType::FunctionType>())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: not a callable", call.m_paren, m_file_name, m_source_lines, resolved_method_type));
					}

					MidoriType::FunctionType& function_type = resolved_method_type->GetType<MidoriType::FunctionType>();
					if (function_type.m_param_types.size() != arg_results.size())
					{
						return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Call expression type error: incorrect arity", call.m_paren, m_file_name, m_source_lines));
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
					call.m_type_data = ApplySubstitution(function_type.m_return_type);
					return call.m_type_data;
				}
			}
		}
	}

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
	size_t separator_pos = variable.m_name.m_lexeme.rfind(NameSeparator.data());
	if (separator_pos != std::string::npos)
	{
		std::string qualifier = variable.m_name.m_lexeme.substr(0u, separator_pos);
		std::string symbol_name = variable.m_name.m_lexeme.substr(separator_pos + NameSeparator.length());

		for (const MidoriType::ClassConstraint& constraint : m_active_constraints)
		{
			if (constraint.m_class_name != qualifier)
			{
				continue;
			}

			std::unordered_map<std::string, ClassInfo>::iterator tc_it = m_classes.find(constraint.m_class_name);
			if (tc_it == m_classes.end())
			{
				continue;
			}

			const ClassInfo& tc_info = tc_it->second;
			std::unordered_map<std::string, std::shared_ptr<MidoriType>>::const_iterator method_it = tc_info.m_method_types.find(symbol_name);
			if (method_it != tc_info.m_method_types.cend())
			{
				variable.m_type_data = Freshen(method_it->second);
				return variable.m_type_data;
			}
		}
	}

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

	for (const MidoriType::ClassConstraint& constraint : m_active_constraints)
	{
		std::unordered_map<std::string, ClassInfo>::iterator tc_it = m_classes.find(constraint.m_class_name);
		if (tc_it != m_classes.end())
		{
			const ClassInfo& tc_info = tc_it->second;
			std::unordered_map<std::string, std::shared_ptr<MidoriType>>::const_iterator method_it = tc_info.m_method_types.find(variable.m_name.m_lexeme);
			if (method_it != tc_info.m_method_types.cend())
			{
				// This is a valid class method - return its type
				// The actual name resolution to the mangled method happens in code generation
				variable.m_type_data = Freshen(method_it->second);
				return variable.m_type_data;
			}
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

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::AppendAssign& append_assign)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **append_assign.m_value)
		.and_then
		(
			[&append_assign, this](std::shared_ptr<MidoriType>&& value_type) ->MidoriResult::TypeResult
			{
				for (TypeChecker::TypeEnvironmentStack::reverse_iterator it = m_name_type_table.rbegin(); it != m_name_type_table.rend(); ++it)
				{
					TypeEnvironment::iterator var = it->find(append_assign.m_name.m_lexeme);
					if (var != it->end())
					{
						std::shared_ptr<MidoriType> target_type = ApplySubstitution(var->second);
						if (!target_type->IsType<MidoriType::ArrayType>() &&
						    !target_type->IsType<MidoriType::TextType>() &&
						    !target_type->IsType<MidoriType::TypeVariable>())
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Append assignment type error: expected array or text type", append_assign.m_name, m_file_name, m_source_lines, target_type));
						}

						if (target_type->IsType<MidoriType::ArrayType>())
						{
							std::shared_ptr<MidoriType> element_type = target_type->GetType<MidoriType::ArrayType>().m_element_type;
							return Unify(append_assign.m_name, element_type, value_type)
								.and_then
								(
									[&append_assign, &target_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
									{
										append_assign.m_type_data = target_type;
										return append_assign.m_type_data;
									}
								);
						}
						else if (target_type->IsType<MidoriType::TextType>())
						{
							// Text can only be appended with Text
							std::shared_ptr<MidoriType> text_type = MidoriType::MakeLiteralType<MidoriType::TextType>();
							return Unify(append_assign.m_name, text_type, value_type)
								.and_then
								(
									[&append_assign, &target_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
									{
										append_assign.m_type_data = target_type;
										return append_assign.m_type_data;
									}
								);
						}
						else
						{
							// TypeVariable - create constraints
							append_assign.m_type_data = target_type;
							return append_assign.m_type_data;
						}
					}
				}

				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Append assignment type error: variable not found", append_assign.m_name, m_file_name, m_source_lines));
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::PrependAssign& prepend_assign)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **prepend_assign.m_value)
		.and_then
		(
			[&prepend_assign, this](std::shared_ptr<MidoriType>&& value_type) ->MidoriResult::TypeResult
			{
				for (TypeChecker::TypeEnvironmentStack::reverse_iterator it = m_name_type_table.rbegin(); it != m_name_type_table.rend(); ++it)
				{
					TypeEnvironment::iterator var = it->find(prepend_assign.m_name.m_lexeme);
					if (var != it->end())
					{
						std::shared_ptr<MidoriType> target_type = ApplySubstitution(var->second);
						if (!target_type->IsType<MidoriType::ArrayType>() &&
						    !target_type->IsType<MidoriType::TextType>() &&
						    !target_type->IsType<MidoriType::TypeVariable>())
						{
							return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Prepend assignment type error: expected array or text type", prepend_assign.m_name, m_file_name, m_source_lines, target_type));
						}

						if (target_type->IsType<MidoriType::ArrayType>())
						{
							std::shared_ptr<MidoriType> element_type = target_type->GetType<MidoriType::ArrayType>().m_element_type;
							return Unify(prepend_assign.m_name, element_type, value_type)
								.and_then
								(
									[&prepend_assign, &target_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
									{
										prepend_assign.m_type_data = target_type;
										return prepend_assign.m_type_data;
									}
								);
						}
						else if (target_type->IsType<MidoriType::TextType>())
						{
							// Text can only be prepended with Text
							std::shared_ptr<MidoriType> text_type = MidoriType::MakeLiteralType<MidoriType::TextType>();
							return Unify(prepend_assign.m_name, text_type, value_type)
								.and_then
								(
									[&prepend_assign, &target_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
									{
										prepend_assign.m_type_data = target_type;
										return prepend_assign.m_type_data;
									}
								);
						}
						else
						{
							// TypeVariable - create constraints
							prepend_assign.m_type_data = target_type;
							return prepend_assign.m_type_data;
						}
					}
				}

				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Prepend assignment type error: variable not found", prepend_assign.m_name, m_file_name, m_source_lines));
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::CompoundAssign& compound_assign)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **compound_assign.m_value)
		.and_then
		(
			[&compound_assign, this](std::shared_ptr<MidoriType>&& value_type) ->MidoriResult::TypeResult
			{
				for (TypeChecker::TypeEnvironmentStack::reverse_iterator it = m_name_type_table.rbegin(); it != m_name_type_table.rend(); ++it)
				{
					TypeEnvironment::iterator var = it->find(compound_assign.m_name.m_lexeme);
					if (var != it->end())
					{
						std::shared_ptr<MidoriType> target_type = ApplySubstitution(var->second);

						// Check if operator is arithmetic
						if (compound_assign.m_op.m_token_name == Token::Name::PLUS_EQUAL ||
						    compound_assign.m_op.m_token_name == Token::Name::MINUS_EQUAL ||
						    compound_assign.m_op.m_token_name == Token::Name::STAR_EQUAL ||
						    compound_assign.m_op.m_token_name == Token::Name::SLASH_EQUAL ||
						    compound_assign.m_op.m_token_name == Token::Name::PERCENT_EQUAL)
						{
							// Must be numeric type
							if (!target_type->IsType<MidoriType::IntegerType>() &&
							    !target_type->IsType<MidoriType::FloatType>() &&
							    !target_type->IsType<MidoriType::TypeVariable>())
							{
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Compound assignment type error: expected numeric type", compound_assign.m_op, m_file_name, m_source_lines, target_type));
							}
						}
						else if (compound_assign.m_op.m_token_name == Token::Name::AMPERSAND_EQUAL ||
						         compound_assign.m_op.m_token_name == Token::Name::BAR_EQUAL ||
						         compound_assign.m_op.m_token_name == Token::Name::CARET_EQUAL ||
						         compound_assign.m_op.m_token_name == Token::Name::LEFT_SHIFT_EQUAL ||
						         compound_assign.m_op.m_token_name == Token::Name::RIGHT_SHIFT_EQUAL)
						{
							// Must be integer type
							if (!target_type->IsType<MidoriType::IntegerType>() &&
							    !target_type->IsType<MidoriType::TypeVariable>())
							{
								return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Compound assignment type error: expected integer type for bitwise operator", compound_assign.m_op, m_file_name, m_source_lines, target_type));
							}
						}

						// Unify value type with target type
						return Unify(compound_assign.m_op, target_type, value_type)
							.and_then
							(
								[&compound_assign, &target_type, this](std::shared_ptr<MidoriType>&&)->MidoriResult::TypeResult
								{
									compound_assign.m_type_data = target_type;
									return compound_assign.m_type_data;
								}
							);
					}
				}

				return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Compound assignment type error: variable not found", compound_assign.m_name, m_file_name, m_source_lines));
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

	// Mark as generic instantiation if this was a generic struct/union
	if (construct.IsConstructTypeOf<MidoriExpression::Construct::Struct>())
	{
		if (m_generic_structs.contains(actual_type_name) && construct.m_type_data->IsType<MidoriType::StructType>())
		{
			construct.m_type_data->GetType<MidoriType::StructType>().m_is_generic_instantiation = true;
		}
	}
	else if (m_generic_unions.contains(actual_type_name) && construct.m_type_data->IsType<MidoriType::UnionType>())
	{
		construct.m_type_data->GetType<MidoriType::UnionType>().m_is_generic_instantiation = true;
	}

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

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::RangeBinary& range_binary)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **range_binary.m_start)
		.and_then
		(
			[&range_binary, this](std::shared_ptr<MidoriType>&& start_type) -> MidoriResult::TypeResult
			{
				return std::visit([this](auto&& arg) { return (*this)(arg); }, **range_binary.m_end)
					.and_then
					(
						[&start_type, &range_binary, this](std::shared_ptr<MidoriType>&& end_type) -> MidoriResult::TypeResult
						{
							return Unify(range_binary.m_range_op, start_type, end_type)
								.and_then
								(
									[&range_binary, &start_type, this](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
									{
										std::shared_ptr<MidoriType> resolved_type = ApplySubstitution(start_type);

										if (!resolved_type->IsNumericType() && !resolved_type->IsType<MidoriType::TypeVariable>())
										{
											return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Range expression type error: expected numeric type", range_binary.m_range_op, m_file_name, m_source_lines, resolved_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
										}

										range_binary.m_type_data = MidoriType::MakeRangeType(start_type);
										return range_binary.m_type_data;
									}
								);
						}
					);
			}
		);
}

MidoriResult::TypeResult TypeChecker::operator()(MidoriExpression::RangeTernary& range_ternary)
{
	return std::visit([this](auto&& arg) { return (*this)(arg); }, **range_ternary.m_start)
		.and_then
		(
			[&range_ternary, this](std::shared_ptr<MidoriType>&& start_type) -> MidoriResult::TypeResult
			{
				return std::visit([this](auto&& arg) { return (*this)(arg); }, **range_ternary.m_step)
					.and_then
					(
						[&start_type, &range_ternary, this](std::shared_ptr<MidoriType>&& step_type) -> MidoriResult::TypeResult
						{
							return Unify(range_ternary.m_first_range_op, start_type, step_type)
								.and_then
								(
									[&start_type, &range_ternary, this](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
									{
										return std::visit([this](auto&& arg) { return (*this)(arg); }, **range_ternary.m_end)
											.and_then
											(
												[&start_type, &range_ternary, this](std::shared_ptr<MidoriType>&& end_type) -> MidoriResult::TypeResult
												{
													return Unify(range_ternary.m_second_range_op, start_type, end_type)
														.and_then
														(
															[&range_ternary, &start_type, this](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
															{
																std::shared_ptr<MidoriType> resolved_type = ApplySubstitution(start_type);

																if (!resolved_type->IsNumericType() && !resolved_type->IsType<MidoriType::TypeVariable>())
																{
																	return std::unexpected<std::string>(MidoriError::GenerateTypeCheckerErrorWithContext("Range expression type error: expected numeric type", range_ternary.m_first_range_op, m_file_name, m_source_lines, resolved_type, MidoriType::MakeLiteralType<MidoriType::IntegerType>(), MidoriType::MakeLiteralType<MidoriType::FloatType>()));
																}

																range_ternary.m_type_data = MidoriType::MakeRangeType(start_type);
																return range_ternary.m_type_data;
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

		// Check if the last statement is a break or return (which have NeverType)
		// If so, the block should have NeverType rather than Unit
		if (!block.m_stmts.empty())
		{
			const std::unique_ptr<MidoriStatement>& last_stmt = block.m_stmts.back();
			if (last_stmt->IsStatement<MidoriStatement::Simple>())
			{
				const MidoriStatement::Simple& simple = last_stmt->GetStatement<MidoriStatement::Simple>();
				if (simple.m_expr->IsExpression<MidoriExpression::Break>() ||
				    simple.m_expr->IsExpression<MidoriExpression::Return>())
				{
					// Block ends with break or return statement - use NeverType
					block.m_type_data = simple.m_expr->GetType();
					return block.m_type_data;
				}
			}
		}

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
				// Unify the break value's type with the expected break type for the current loop
				if (m_expected_break_type)
				{
					return Unify(break_expr.m_keyword, type, m_expected_break_type)
						.and_then
						(
							[&break_expr](std::shared_ptr<MidoriType>&&) -> MidoriResult::TypeResult
							{
								// Like return, break never returns normally, so its type is NeverType
								// This allows it to unify with any type in if-else branches
								break_expr.m_type_data = MidoriType::MakeLiteralType<MidoriType::NeverType>();
								return break_expr.m_type_data;
							}
						);
				}
				else
				{
					// Break outside of a loop - this should be caught by an earlier pass
					// For now, just set to NeverType
					break_expr.m_type_data = MidoriType::MakeLiteralType<MidoriType::NeverType>();
					return break_expr.m_type_data;
				}
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
