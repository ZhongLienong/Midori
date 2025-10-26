#pragma once

#include <algorithm>
#include <memory>
#include <ranges>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

class MidoriType
{
public:
    struct UndecidedType {};

    struct GenericParam
    {
        std::string m_name;

        GenericParam(const std::string& name) : m_name(name) {}

        bool operator==(const GenericParam& other) const
        {
            return m_name == other.m_name;
        }
    };

    struct TypeVariable
    {
        int m_id;

        TypeVariable(int id) : m_id(id) {}

        bool operator==(const TypeVariable& other) const
        {
            return m_id == other.m_id;
        }
    };

    struct FloatType {};
    struct IntegerType {};
    struct TextType {};
    struct BoolType {};
    struct UnitType {};
    struct NeverType {};

    struct ArrayType
    {
        std::shared_ptr<MidoriType> m_element_type;
    };

    struct FunctionType
    {
        std::vector<std::shared_ptr<MidoriType>> m_param_types;
        std::shared_ptr<MidoriType> m_return_type;
        bool m_is_foreign = false;
    };

    struct StructType
    {
        std::vector<std::shared_ptr<MidoriType>> m_member_types;
        std::vector<std::string> m_member_names;
        std::string m_name;
        std::vector<std::string> m_generic_params;  // Generic parameter names like ["T", "U"]
    };

    struct UnionType
    {
        struct UnionMemberContext
        {
            std::vector<std::shared_ptr<MidoriType>> m_member_types;
            int m_tag;
        };

        std::unordered_map<std::string, UnionMemberContext> m_member_info;
        std::string m_name;
        std::vector<std::string> m_generic_params;  // Generic parameter names like ["T", "U"]

        UnionType(const std::string& name);
    };

    using MidoriTypeUnion = std::variant<UndecidedType, GenericParam, TypeVariable, FloatType, IntegerType, TextType, BoolType, UnitType, NeverType, ArrayType, FunctionType, StructType, UnionType>;

    MidoriTypeUnion m_type;

    MidoriType(MidoriTypeUnion&& actual_type);

public:
    template<typename T>
    constexpr bool IsType() const
    {
        return std::holds_alternative<T>(m_type);
    }

    template<typename T>
    T& GetType()
    {
        return std::get<T>(m_type);
    }

    template<typename T>
    const T& GetType() const
    {
        return std::get<T>(m_type);
    }

    template<typename T>
    requires std::is_same_v<T, FloatType> || std::is_same_v<T, IntegerType> || std::is_same_v<T, BoolType> || std::is_same_v<T, TextType> || std::is_same_v<T, UnitType> || std::is_same_v<T, NeverType>
    static const std::shared_ptr<MidoriType>& MakeLiteralType()
    {
        static std::shared_ptr<MidoriType> literal_type = std::make_shared<MidoriType>(T{});
        return literal_type;
    }

    static const std::shared_ptr<MidoriType> MakeUndecidedType();

    static std::shared_ptr<MidoriType> MakeGenericType(const std::string& name);

    static std::shared_ptr<MidoriType> MakeTypeVariable(int id);

    static std::shared_ptr<MidoriType> MakeArrayType(const std::shared_ptr<MidoriType>& element_type);

    static std::shared_ptr<MidoriType> MakeFunctionType(const std::vector<std::shared_ptr<MidoriType>>& param_types, std::shared_ptr<MidoriType>&& return_type, bool is_foreign = false); 

    static std::shared_ptr<MidoriType> MakeStructType(const std::string& name, std::vector<std::shared_ptr<MidoriType>>&& member_types, std::vector<std::string>&& member_names, std::vector<std::string>&& generic_params = {});

    static std::shared_ptr<MidoriType> MakeUnionType(const std::string& name, std::vector<std::string>&& generic_params = {});

    static std::shared_ptr<MidoriType> SubstituteTypeParams(const std::shared_ptr<MidoriType>& type, const std::unordered_map<std::string, std::shared_ptr<MidoriType>>& substitutions);

    std::string ToString() const;

    bool IsNumericType() const;

    friend bool operator==(const MidoriType& lhs, const MidoriType& rhs);
};

inline bool operator==(const MidoriType& lhs, const MidoriType& rhs)
{
    return std::visit
    (
        [](const auto& a, const auto& b) -> bool
        {
            using TypeA = std::decay_t<decltype(a)>;
            using TypeB = std::decay_t<decltype(b)>;

            if constexpr (!std::is_same_v<TypeA, TypeB>)
            {
                return false;  // Different underlying types
            }
            else
            {
                if constexpr (std::is_same_v<TypeA, MidoriType::GenericParam>)
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
                else if constexpr (std::is_same_v<TypeA, MidoriType::FunctionType>)
                {
                    return std::ranges::equal
                    (
                        a.m_param_types,
                        b.m_param_types,
                        [](const std::shared_ptr<MidoriType>& t1, const std::shared_ptr<MidoriType>& t2) { return *t1 == *t2; }
                    ) && *a.m_return_type == *b.m_return_type;
                }
                else if constexpr (std::is_same_v<TypeA, MidoriType::StructType>)
                {
                    // For non-generic structs, name equality is sufficient
                    // For generic structs or instantiated generics, we need to compare member types too
                    // (e.g., Box<Integer> vs Box<Text> have same name but different member types)
                    if (a.m_name != b.m_name)
                    {
                        return false;
                    }

                    // If both have no generic params and no type variables in members, name equality is enough
                    bool a_is_generic = !a.m_generic_params.empty();
                    bool b_is_generic = !b.m_generic_params.empty();

                    // Check if members contain type variables (for instantiated generics)
                    if (!a_is_generic)
                    {
                        for (const std::shared_ptr<MidoriType>& member_type : a.m_member_types)
                        {
                            if (member_type->IsType<MidoriType::TypeVariable>() || member_type->IsType<MidoriType::GenericParam>())
                            {
                                a_is_generic = true;
                                break;
                            }
                        }
                    }

                    if (!b_is_generic)
                    {
                        for (const std::shared_ptr<MidoriType>& member_type : b.m_member_types)
                        {
                            if (member_type->IsType<MidoriType::TypeVariable>() || member_type->IsType<MidoriType::GenericParam>())
                            {
                                b_is_generic = true;
                                break;
                            }
                        }
                    }

                    // For non-generic structs, name equality is sufficient
                    if (!a_is_generic && !b_is_generic)
                    {
                        return true;
                    }

                    // For generic structs, compare member types too
                    return a.m_member_types.size() == b.m_member_types.size() &&
                           std::ranges::equal
                           (
                               a.m_member_types,
                               b.m_member_types,
                               [](const std::shared_ptr<MidoriType>& t1, const std::shared_ptr<MidoriType>& t2) { return *t1 == *t2; }
                           );
                }
                else if constexpr (std::is_same_v<TypeA, MidoriType::UnionType>)
                {
                    return a.m_name == b.m_name;
                }
                else
                {
                    // Primitive types (always equal if same variant type)
                    return true;
                }
            }
        },
        lhs.m_type,
        rhs.m_type
    );
}
