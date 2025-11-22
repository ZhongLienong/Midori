#include "Type.h"

#include <functional>
#include <numeric>

using namespace std::string_literals;

MidoriType::MidoriType(MidoriType::MidoriTypeUnion&& actual_type) : m_type(std::move(actual_type))
{
}

MidoriType::GenericParam::GenericParam(const std::string& name) : m_name(name)
{
}

bool MidoriType::GenericParam::operator==(const GenericParam& other) const
{
	return m_name == other.m_name;
}

MidoriType::TypeVariable::TypeVariable(int id) : m_id(id)
{
}

bool MidoriType::TypeVariable::operator==(const TypeVariable& other) const
{
	return m_id == other.m_id;
}

MidoriType::UnionType::UnionType(const std::string& name) : m_name(name)
{
}

const std::shared_ptr<MidoriType> MidoriType::MakeUndecidedType()
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(UndecidedType{}));
}

std::shared_ptr<MidoriType> MidoriType::MakeGenericType(const std::string& name)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(GenericParam(name)));
}

std::shared_ptr<MidoriType> MidoriType::MakeTypeVariable(int id)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(TypeVariable(id)));
}

std::shared_ptr<MidoriType> MidoriType::MakeArrayType(const std::shared_ptr<MidoriType>& element_type)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(ArrayType{.m_element_type = element_type}));
}

std::shared_ptr<MidoriType> MidoriType::MakeTupleType(std::vector<std::shared_ptr<MidoriType>>&& element_types)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(TupleType{.m_element_types = std::move(element_types)}));
}

std::shared_ptr<MidoriType> MidoriType::MakeFunctionType(const std::vector<std::shared_ptr<MidoriType>>& param_types, std::shared_ptr<MidoriType>&& return_type, bool is_foreign)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(FunctionType{.m_param_types = param_types, .m_return_type = return_type, .m_is_foreign = is_foreign}));
}

std::shared_ptr<MidoriType> MidoriType::MakeStructType(const std::string& name, std::vector<std::shared_ptr<MidoriType>>&& member_types, std::vector<std::string>&& member_names, std::vector<std::string>&& generic_params)
{
	return std::make_shared<MidoriType>(MidoriTypeUnion(StructType{.m_member_types = std::move(member_types), .m_member_names = std::move(member_names), .m_name = name, .m_generic_params = std::move(generic_params)}));
}

std::shared_ptr<MidoriType> MidoriType::MakeUnionType(const std::string& name, std::vector<std::string>&& generic_params)
{
	UnionType union_type(name);
	union_type.m_generic_params = std::move(generic_params);
	return std::make_shared<MidoriType>(MidoriTypeUnion(std::move(union_type)));
}

std::shared_ptr<MidoriType> MidoriType::SubstituteTypeParams(const std::shared_ptr<MidoriType>& type, const std::unordered_map<std::string, std::shared_ptr<MidoriType>>& substitutions)
{
	using SubstituteFn = std::function<std::shared_ptr<MidoriType>(const std::shared_ptr<MidoriType>&)>;

	SubstituteFn substitute = [&substitutions, &substitute](const std::shared_ptr<MidoriType>& t) -> std::shared_ptr<MidoriType>
		{
			return std::visit
			(
				[&substitutions, &substitute](const auto& type_variant) -> std::shared_ptr<MidoriType>
				{
					using T = std::decay_t<decltype(type_variant)>;

					if constexpr (std::is_same_v<T, GenericParam>)
					{
						std::unordered_map<std::string, std::shared_ptr<MidoriType>>::const_iterator it = substitutions.find(type_variant.m_name);
						return (it != substitutions.end()) ? it->second : MakeGenericType(type_variant.m_name);
					}
					else if constexpr (std::is_same_v<T, ArrayType>)
					{
						return MakeArrayType(substitute(type_variant.m_element_type));
					}
					else if constexpr (std::is_same_v<T, TupleType>)
					{
						std::vector<std::shared_ptr<MidoriType>> new_element_types;
						std::ranges::transform(type_variant.m_element_types, std::back_inserter(new_element_types), substitute);
						return MakeTupleType(std::move(new_element_types));
					}
					else if constexpr (std::is_same_v<T, FunctionType>)
					{
						std::vector<std::shared_ptr<MidoriType>> new_param_types;
						std::ranges::transform(type_variant.m_param_types, std::back_inserter(new_param_types), substitute);
						return MakeFunctionType(new_param_types, substitute(type_variant.m_return_type), type_variant.m_is_foreign);
					}
					else if constexpr (std::is_same_v<T, StructType>)
					{
						std::vector<std::shared_ptr<MidoriType>> new_member_types;
						std::ranges::transform(type_variant.m_member_types, std::back_inserter(new_member_types), substitute);
						return MakeStructType(type_variant.m_name, std::move(new_member_types), std::vector<std::string>(type_variant.m_member_names), {});
					}
					else if constexpr (std::is_same_v<T, UnionType>)
					{
						std::shared_ptr<MidoriType> new_union_type = MakeUnionType(type_variant.m_name, {});
						UnionType& new_union_ref = new_union_type->GetType<UnionType>();

						std::ranges::for_each
						(
							type_variant.m_member_info,
							[&new_union_ref, &substitute](const std::pair<const std::string, UnionType::UnionMemberContext>& member_pair)
							{
								const std::string& member_name = member_pair.first;
								const UnionType::UnionMemberContext& member_ctx = member_pair.second;

								std::vector<std::shared_ptr<MidoriType>> new_member_types;
								std::ranges::transform(member_ctx.m_member_types, std::back_inserter(new_member_types), substitute);

								new_union_ref.m_member_info.emplace(member_name, UnionType::UnionMemberContext{.m_member_types = std::move(new_member_types), .m_tag = member_ctx.m_tag});
							}
						);

						return new_union_type;
					}
					else
					{
						return std::make_shared<MidoriType>(MidoriTypeUnion(type_variant));
					}
				},
				t->m_type
			);
		};

	return substitute(type);
}

std::string MidoriType::ToString() const
{
	using StringJoinFn = std::function<std::string(const std::string&, const std::string&)>;

	StringJoinFn join_with_comma = [](const std::string& acc, const std::string& elem)
		{
			return acc.empty() ? elem : acc + ", "s + elem;
		};

	return std::visit
	(
		[&join_with_comma](const auto& type_variant) -> std::string
		{
			using Type = std::decay_t<decltype(type_variant)>;

			if constexpr (std::is_same_v<Type, UndecidedType>)
			{
				return "Undecided"s;
			}
			else if constexpr (std::is_same_v<Type, GenericParam>)
			{
				return type_variant.m_name;
			}
			else if constexpr (std::is_same_v<Type, TypeVariable>)
			{
				return "T"s + std::to_string(type_variant.m_id);
			}
			else if constexpr (std::is_same_v<Type, FloatType>)
			{
				return "Float"s;
			}
			else if constexpr (std::is_same_v<Type, IntegerType>)
			{
				return "Int"s;
			}
			else if constexpr (std::is_same_v<Type, TextType>)
			{
				return "Text"s;
			}
			else if constexpr (std::is_same_v<Type, BoolType>)
			{
				return "Bool"s;
			}
			else if constexpr (std::is_same_v<Type, UnitType>)
			{
				return "Unit"s;
			}
			else if constexpr (std::is_same_v<Type, NeverType>)
			{
				return "Never"s;
			}
			else if constexpr (std::is_same_v<Type, ArrayType>)
			{
				return "Array<"s + type_variant.m_element_type->ToString() + ">"s;
			}
			else if constexpr (std::is_same_v<Type, TupleType>)
			{
				if (type_variant.m_element_types.empty())
				{
					return "Unit"s;
				}

				std::vector<std::string> type_strings;
				std::ranges::transform(type_variant.m_element_types, std::back_inserter(type_strings), [](const std::shared_ptr<MidoriType>& elem_type) { return elem_type->ToString(); });

				return "("s + std::accumulate(std::next(type_strings.begin()), type_strings.end(), type_strings.front(), join_with_comma) + ")"s;
			}
			else if constexpr (std::is_same_v<Type, FunctionType>)
			{
				if (type_variant.m_param_types.empty())
				{
					return "fn() -> "s + type_variant.m_return_type->ToString();
				}

				std::vector<std::string> param_strings;
				std::ranges::transform(type_variant.m_param_types, std::back_inserter(param_strings), [](const std::shared_ptr<MidoriType>& param_type) { return param_type->ToString(); });

				return "fn("s + std::accumulate(std::next(param_strings.begin()), param_strings.end(), param_strings.front(), join_with_comma) + ") -> "s + type_variant.m_return_type->ToString();
			}
			else if constexpr (std::is_same_v<Type, StructType>)
			{
				if (!type_variant.m_generic_params.empty())
				{
					return type_variant.m_name + "<"s + std::accumulate(std::next(type_variant.m_generic_params.begin()), type_variant.m_generic_params.end(), type_variant.m_generic_params.front(), join_with_comma) + ">"s;
				}
				else if (type_variant.m_is_generic_instantiation)
				{
					std::vector<std::string> member_type_strings;
					std::ranges::transform(type_variant.m_member_types, std::back_inserter(member_type_strings), [](const std::shared_ptr<MidoriType>& member_type) { return member_type->ToString(); });

					return type_variant.m_name + "<"s + std::accumulate(std::next(member_type_strings.begin()), member_type_strings.end(), member_type_strings.front(), join_with_comma) + ">"s;
				}
				else
				{
					return type_variant.m_name;
				}
			}
			else if constexpr (std::is_same_v<Type, UnionType>)
			{
				return type_variant.m_name;
			}
			else
			{
				return ""s;
			}
		},
		m_type
	);
}

bool MidoriType::IsNumericType() const
{
	return IsType<FloatType>() || IsType<IntegerType>();
}

bool MidoriType::CompareGenericStructs(const StructType& a, const StructType& b)
{
	using TypePtrPair = std::pair<std::shared_ptr<MidoriType>, std::shared_ptr<MidoriType>>;
	using TypeEqualityFn = std::function<bool(const TypePtrPair&)>;

	TypeEqualityFn types_equal = [](const TypePtrPair& pair)
		{
			return *pair.first == *pair.second;
		};

	return a.m_member_types.size() == b.m_member_types.size() && std::ranges::all_of(std::views::zip(a.m_member_types, b.m_member_types), types_equal);
}

bool MidoriType::CompareInstantiatedStructs(const StructType& a, const StructType& b)
{
	using StringPairToStringFn = std::function<std::string(const std::pair<std::shared_ptr<MidoriType>, std::shared_ptr<MidoriType>>&)>;

	StringPairToStringFn get_first_string = [](const std::pair<std::shared_ptr<MidoriType>, std::shared_ptr<MidoriType>>& pair)
		{
			return pair.first->ToString();
		};

	StringPairToStringFn get_second_string = [](const std::pair<std::shared_ptr<MidoriType>, std::shared_ptr<MidoriType>>& pair)
		{
			return pair.second->ToString();
		};

	if (a.m_member_types.size() != b.m_member_types.size() || !std::ranges::equal(a.m_member_names, b.m_member_names))
	{
		return false;
	}

	std::ranges::zip_view zipped_members(a.m_member_types, b.m_member_types);

	return std::ranges::all_of
	(
		zipped_members,
		[](const std::pair<std::shared_ptr<MidoriType>, std::shared_ptr<MidoriType>>& pair)
		{
			return pair.first->ToString() == pair.second->ToString();
		}
	);
}

bool MidoriType::CompareStructTypes(const StructType& a, const StructType& b)
{
	if (a.m_name != b.m_name)
	{
		return false;
	}

	bool a_has_generic_params = !a.m_generic_params.empty();
	bool b_has_generic_params = !b.m_generic_params.empty();

	if (a_has_generic_params || b_has_generic_params)
	{
		return CompareGenericStructs(a, b);
	}

	return CompareInstantiatedStructs(a, b);
}

bool MidoriType::CompareGenericUnions(const UnionType& a, const UnionType& b)
{
	using VariantPairCheckFn = std::function<bool(const std::pair<const std::string, UnionType::UnionMemberContext>&)>;

	if (a.m_member_info.size() != b.m_member_info.size())
	{
		return false;
	}

	VariantPairCheckFn check_variant = [&b](const std::pair<const std::string, UnionType::UnionMemberContext>& a_variant)
		{
			std::unordered_map<std::string, UnionType::UnionMemberContext>::const_iterator b_it = b.m_member_info.find(a_variant.first);

			if (b_it == b.m_member_info.end())
			{
				return false;
			}

			const UnionType::UnionMemberContext& a_ctx = a_variant.second;
			const UnionType::UnionMemberContext& b_ctx = b_it->second;

			return a_ctx.m_member_types.size() == b_ctx.m_member_types.size() 
				&& std::ranges::equal
				(
					a_ctx.m_member_types,
					b_ctx.m_member_types,
					[](const std::shared_ptr<MidoriType>& t1, const std::shared_ptr<MidoriType>& t2)
					{
						return *t1 == *t2;
					}
				);
		};

	return std::ranges::all_of(a.m_member_info, check_variant);
}

bool MidoriType::CompareInstantiatedUnions(const UnionType& a, const UnionType& b)
{
	using VariantPairCheckFn = std::function<bool(const std::pair<const std::string, UnionType::UnionMemberContext>&)>;

	if (a.m_member_info.size() != b.m_member_info.size())
	{
		return false;
	}

	VariantPairCheckFn check_variant = [&b](const std::pair<const std::string, UnionType::UnionMemberContext>& a_variant)
		{
			std::unordered_map<std::string, UnionType::UnionMemberContext>::const_iterator b_it = b.m_member_info.find(a_variant.first);

			if (b_it == b.m_member_info.end())
			{
				return false;
			}

			const UnionType::UnionMemberContext& a_ctx = a_variant.second;
			const UnionType::UnionMemberContext& b_ctx = b_it->second;

			if (a_ctx.m_member_types.size() != b_ctx.m_member_types.size())
			{
				return false;
			}

			std::ranges::zip_view zipped_types(a_ctx.m_member_types, b_ctx.m_member_types);

			return std::ranges::all_of
			(
				zipped_types,
				[](const std::pair<std::shared_ptr<MidoriType>, std::shared_ptr<MidoriType>>& type_pair)
				{
					return type_pair.first->ToString() == type_pair.second->ToString();
				}
			);
		};

	return std::ranges::all_of(a.m_member_info, check_variant);
}

bool MidoriType::CompareUnionTypes(const UnionType& a, const UnionType& b)
{
	if (a.m_name != b.m_name)
	{
		return false;
	}

	bool a_has_generic_params = !a.m_generic_params.empty();
	bool b_has_generic_params = !b.m_generic_params.empty();

	if (a_has_generic_params || b_has_generic_params)
	{
		return CompareGenericUnions(a, b);
	}

	return CompareInstantiatedUnions(a, b);
}

bool operator==(const MidoriType& lhs, const MidoriType& rhs)
{
	return std::visit
	(
		[](const auto& a, const auto& b) -> bool
		{
			using TypeA = std::decay_t<decltype(a)>;
			using TypeB = std::decay_t<decltype(b)>;

			if constexpr (!std::is_same_v<TypeA, TypeB>)
			{
				return false;
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::GenericParam>)
			{
				return a.m_name == b.m_name;
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::TypeVariable>)
			{
				return a.m_id == b.m_id;
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::ArrayType>)
			{
				return *a.m_element_type == *b.m_element_type;
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::TupleType>)
			{
				return a.m_element_types.size() == b.m_element_types.size() 
					&& std::ranges::equal
					(
						a.m_element_types,
						b.m_element_types,
						[](const std::shared_ptr<MidoriType>& t1, const std::shared_ptr<MidoriType>& t2)
						{
							return *t1 == *t2;
						}
					);
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::FunctionType>)
			{
				return std::ranges::equal
				(
					a.m_param_types,
					b.m_param_types,
					[](const std::shared_ptr<MidoriType>& t1, const std::shared_ptr<MidoriType>& t2)
					{
						return *t1 == *t2;
					}
				) && *a.m_return_type == *b.m_return_type;
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::StructType>)
			{
				return MidoriType::CompareStructTypes(a, b);
			}
			else if constexpr (std::is_same_v<TypeA, MidoriType::UnionType>)
			{
				return MidoriType::CompareUnionTypes(a, b);
			}
			else
			{
				return true;
			}
		},
		lhs.m_type,
		rhs.m_type
	);
}