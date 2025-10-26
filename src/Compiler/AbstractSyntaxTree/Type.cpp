#include "Type.h"

using namespace std::string_literals;

MidoriType::MidoriType(MidoriType::MidoriTypeUnion&& actual_type) : m_type(std::move(actual_type))
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
    return std::make_shared<MidoriType>(MidoriTypeUnion(ArrayType{ .m_element_type = element_type }));
}

std::shared_ptr<MidoriType> MidoriType::MakeFunctionType(const std::vector<std::shared_ptr<MidoriType>>& param_types, std::shared_ptr<MidoriType>&& return_type, bool is_foreign)
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(FunctionType{ .m_param_types = param_types, .m_return_type = return_type, .m_is_foreign = is_foreign }));
}

std::shared_ptr<MidoriType> MidoriType::MakeStructType(const std::string& name, std::vector<std::shared_ptr<MidoriType>>&& member_types, std::vector<std::string>&& member_names, std::vector<std::string>&& generic_params)
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(StructType{ .m_member_types = std::move(member_types), .m_member_names = std::move(member_names), .m_name = name, .m_generic_params = std::move(generic_params) }));
}

std::shared_ptr<MidoriType> MidoriType::MakeUnionType(const std::string& name, std::vector<std::string>&& generic_params)
{
    auto union_type = UnionType(name);
    union_type.m_generic_params = std::move(generic_params);
    return std::make_shared<MidoriType>(MidoriTypeUnion(std::move(union_type)));
}

std::shared_ptr<MidoriType> MidoriType::SubstituteTypeParams(const std::shared_ptr<MidoriType>& type, const std::unordered_map<std::string, std::shared_ptr<MidoriType>>& substitutions)
{
    return std::visit
    (
        [&substitutions](const auto& type_variant) -> std::shared_ptr<MidoriType>
        {
            using T = std::decay_t<decltype(type_variant)>;

            if constexpr (std::is_same_v<T, GenericParam>)
            {
                // Replace generic parameter with concrete type
                auto it = substitutions.find(type_variant.m_name);
                if (it != substitutions.end())
                {
                    return it->second;
                }
                // If not found in substitutions, return as-is (might be a different generic param)
                return MakeGenericType(type_variant.m_name);
            }
            else if constexpr (std::is_same_v<T, ArrayType>)
            {
                // Recursively substitute in array element type
                auto new_element_type = SubstituteTypeParams(type_variant.m_element_type, substitutions);
                return MakeArrayType(new_element_type);
            }
            else if constexpr (std::is_same_v<T, FunctionType>)
            {
                // Recursively substitute in function parameter and return types
                std::vector<std::shared_ptr<MidoriType>> new_param_types;
                for (const auto& param_type : type_variant.m_param_types)
                {
                    new_param_types.push_back(SubstituteTypeParams(param_type, substitutions));
                }
                auto new_return_type = SubstituteTypeParams(type_variant.m_return_type, substitutions);
                return MakeFunctionType(new_param_types, std::move(new_return_type), type_variant.m_is_foreign);
            }
            else if constexpr (std::is_same_v<T, StructType>)
            {
                // Recursively substitute in struct member types
                std::vector<std::shared_ptr<MidoriType>> new_member_types;
                for (const auto& member_type : type_variant.m_member_types)
                {
                    new_member_types.push_back(SubstituteTypeParams(member_type, substitutions));
                }
                std::vector<std::string> member_names = type_variant.m_member_names;
                // Don't include generic params in the instantiated type
                return MakeStructType(type_variant.m_name, std::move(new_member_types), std::move(member_names), {});
            }
            else if constexpr (std::is_same_v<T, UnionType>)
            {
                // Recursively substitute in union member types
                auto new_union_type = MakeUnionType(type_variant.m_name, {});
                UnionType& new_union_ref = new_union_type->GetType<UnionType>();

                for (const auto& [member_name, member_ctx] : type_variant.m_member_info)
                {
                    std::vector<std::shared_ptr<MidoriType>> new_member_types;
                    for (const auto& member_type : member_ctx.m_member_types)
                    {
                        new_member_types.push_back(SubstituteTypeParams(member_type, substitutions));
                    }
                    new_union_ref.m_member_info.emplace(member_name, UnionType::UnionMemberContext{std::move(new_member_types), member_ctx.m_tag});
                }

                return new_union_type;
            }
            else
            {
                // For primitive types, return as-is
                return std::make_shared<MidoriType>(MidoriTypeUnion(type_variant));
            }
        },
        type->m_type
    );
}

std::string MidoriType::ToString() const
{
    return std::visit
    (
        [](const auto& a) -> std::string
        {
            using Type = std::decay_t<decltype(a)>;
            
            if constexpr (std::is_same_v<Type, MidoriType::UndecidedType>)
            {
                return "Undecided"s;
            }
            else if constexpr (std::is_same_v<Type, MidoriType::GenericParam>)
            {
                return a.m_name;
            }
            else if constexpr (std::is_same_v<Type, MidoriType::TypeVariable>)
            {
                return "T"s + std::to_string(a.m_id);
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
                return "Array<"s + a.m_element_type->ToString() + ">"s;
            }
            else if constexpr (std::is_same_v<Type, FunctionType>)
            {
                const std::vector<std::shared_ptr<MidoriType>>& param_types = a.m_param_types;
                const std::shared_ptr<MidoriType>& return_type = a.m_return_type;
				const std::string function_type_prefix = "fn"s;

                if (param_types.empty())
                {
                    return function_type_prefix + "() -> "s + return_type->ToString();
                }
                else
                {
                    std::string func_type_string = function_type_prefix + "("s;
                    for (const std::shared_ptr<MidoriType>& param_type : param_types)
                    {
                        func_type_string.append(param_type->ToString()).append(", ");
                    }
                    func_type_string.pop_back();
                    func_type_string.pop_back();
                    return func_type_string.append(") -> "s + return_type->ToString());
                }
            }
            else if constexpr (std::is_same_v<Type, StructType>)
            {
                // Only show type arguments for generic structs or instantiated generic structs
                // Check if member types contain any type variables or generic params
                bool has_type_params = false;
                for (const std::shared_ptr<MidoriType>& member_type : a.m_member_types)
                {
                    if (member_type->IsType<GenericParam>() || member_type->IsType<TypeVariable>())
                    {
                        has_type_params = true;
                        break;
                    }
                }

                // Show generic params if defined, or type args if member types contain type variables
                if (!a.m_generic_params.empty())
                {
                    // Show generic parameter names for generic struct declarations
                    std::string struct_type_string = a.m_name + "<"s;
                    for (const std::string& param_name : a.m_generic_params)
                    {
                        struct_type_string.append(param_name).append(", ");
                    }
                    struct_type_string.pop_back();
                    struct_type_string.pop_back();
                    return struct_type_string.append(">"s);
                }
                else if (has_type_params)
                {
                    // Show concrete type arguments for instantiated generic structs
                    std::string struct_type_string = a.m_name + "<"s;
                    for (const std::shared_ptr<MidoriType>& member_type : a.m_member_types)
                    {
                        struct_type_string.append(member_type->ToString()).append(", ");
                    }
                    struct_type_string.pop_back();
                    struct_type_string.pop_back();
                    return struct_type_string.append(">"s);
                }
                else
                {
                    // Non-generic struct - just show the name
                    return a.m_name;
                }
            }
            else
            {
                return a.m_name;
            }
        },
        m_type
    );
}

bool MidoriType::IsNumericType() const
{
    return IsType<FloatType>() || IsType<IntegerType>();
}

MidoriType::UnionType::UnionType(const std::string& name) : m_name(name)
{
}