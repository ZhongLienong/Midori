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
    struct FractionType {};
    struct IntegerType {};
    struct TextType {};
    struct BoolType {};
    struct UnitType {};

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

        UnionType(const std::string& name);
    };

    using MidoriTypeUnion = std::variant<UndecidedType, FractionType, IntegerType, TextType, BoolType, UnitType, ArrayType, FunctionType, StructType, UnionType>;

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
    requires std::is_same_v<T, FractionType> || std::is_same_v<T, IntegerType> || std::is_same_v<T, BoolType> || std::is_same_v<T, TextType> || std::is_same_v<T, UnitType>
    static const std::shared_ptr<MidoriType>& MakeLiteralType()
    {
        static std::shared_ptr<MidoriType> literal_type = std::make_shared<MidoriType>(T{});
        return literal_type;
    }

    static const std::shared_ptr<MidoriType> MakeUndecidedType();

    static std::shared_ptr<MidoriType> MakeArrayType(const std::shared_ptr<MidoriType>& element_type);

    static std::shared_ptr<MidoriType> MakeFunctionType(const std::vector<std::shared_ptr<MidoriType>>& param_types, std::shared_ptr<MidoriType>&& return_type, bool is_foreign = false); 

    static std::shared_ptr<MidoriType> MakeStructType(const std::string& name, std::vector<std::shared_ptr<MidoriType>>&& member_types, std::vector<std::string>&& member_names);

    static std::shared_ptr<MidoriType> MakeUnionType(const std::string& name);

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
                if constexpr (std::is_same_v<TypeA, MidoriType::ArrayType>)
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
                    return a.m_name == b.m_name;
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
