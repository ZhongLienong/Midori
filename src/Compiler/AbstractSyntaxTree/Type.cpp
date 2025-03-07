#include "Type.h"

using namespace std::string_literals;

MidoriType::MidoriType(MidoriType::MidoriTypeUnion&& actual_type) : m_type(std::move(actual_type))
{
}

const std::shared_ptr<MidoriType> MidoriType::MakeUndecidedType()
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(UndecidedType{}));
}

std::shared_ptr<MidoriType> MidoriType::MakeArrayType(const std::shared_ptr<MidoriType>& element_type)
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(ArrayType{ element_type }));
}

std::shared_ptr<MidoriType> MidoriType::MakeFunctionType(const std::vector<std::shared_ptr<MidoriType>>& param_types, std::shared_ptr<MidoriType>&& return_type, bool is_foreign)
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(FunctionType{ param_types, return_type, is_foreign }));
}

std::shared_ptr<MidoriType> MidoriType::MakeStructType(const std::string& name, std::vector<std::shared_ptr<MidoriType>>&& member_types, std::vector<std::string>&& member_names)
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(StructType{ std::move(member_types), std::move(member_names), name }));
}

std::shared_ptr<MidoriType> MidoriType::MakeUnionType(const std::string& name)
{
    return std::make_shared<MidoriType>(MidoriTypeUnion(UnionType(name)));
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
            else if constexpr (std::is_same_v<Type, FractionType>)
            {
                return "Frac"s;
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
            else if constexpr (std::is_same_v<Type, ArrayType>)
            {
                return "Array<"s + a.m_element_type->ToString() + ">"s;
            }
            else if constexpr (std::is_same_v<Type, FunctionType>)
            {
                const std::vector<std::shared_ptr<MidoriType>>& param_types = a.m_param_types;
                const std::shared_ptr<MidoriType>& return_type = a.m_return_type;

                if (param_types.empty())
                {
                    return "() -> "s + return_type->ToString();
                }
                else
                {
                    std::string func_type_string = "("s;
                    for (const std::shared_ptr<MidoriType>& param_type : param_types)
                    {
                        func_type_string.append(param_type->ToString()).append(", ");
                    }
                    func_type_string.pop_back();
                    func_type_string.pop_back();
                    return func_type_string.append(") -> "s + return_type->ToString());
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
    return IsType<FractionType>() || IsType<IntegerType>();
}

MidoriType::UnionType::UnionType(const std::string& name) : m_name(name)
{
}