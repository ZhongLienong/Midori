#pragma once

#include <memory>
#include <variant>
#include <optional>

#include "Compiler/Token/Token.h"
#include "Type.h"

struct Block;
struct Simple;
struct Define;
struct If;
struct While;
struct For;
struct Break;
struct Continue;
struct Return;
struct Foreign;
struct Struct;
struct Union;
struct Switch;
struct Namespace;

using MidoriStatement = std::variant<Block, Simple, Define, If, While, For, Break, Continue, Return, Foreign, Struct, Union, Switch, Namespace>;

class MidoriExpression
{
public:
	struct NameContext
	{
		struct Local
		{
			int m_index = 0;
		};
		struct Cell
		{
			int m_index = 0;
		};
		struct Global {};

		using Tag = std::variant<Local, Cell, Global>;
	};

	enum class ConditionOperandType
	{
		INTEGER,
		FRACTION,
		OTHER
	};

	struct BaseExpression
	{
		std::shared_ptr<MidoriType> m_type_data = nullptr;
	};

	struct As : BaseExpression
	{
		Token m_as_keyword;
		std::weak_ptr<MidoriType> m_from_type;
		std::shared_ptr<MidoriType> m_to_type;
		std::unique_ptr<MidoriExpression> m_expr;

		As(Token&& as_keyword, std::shared_ptr<MidoriType> to_type, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Binary : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_left;
		std::unique_ptr<MidoriExpression> m_right;

		Binary(Token&& op, std::unique_ptr<MidoriExpression>&& left, std::unique_ptr<MidoriExpression>&& right);
	};

	struct Group : BaseExpression
	{
		std::unique_ptr<MidoriExpression> m_expr_in;

		Group(std::unique_ptr<MidoriExpression>&& expr_in);
	};

	struct TextLiteral : BaseExpression
	{
		Token m_token;

		TextLiteral(Token&& token);
	};

	struct BoolLiteral : BaseExpression
	{
		Token m_token;

		BoolLiteral(Token&& token);
	};

	struct FractionLiteral : BaseExpression
	{
		Token m_token;

		FractionLiteral(Token&& token);
	};

	struct IntegerLiteral : BaseExpression
	{
		Token m_token;

		IntegerLiteral(Token&& token);
	};

	struct UnitLiteral : BaseExpression
	{
		Token m_token;

		UnitLiteral(Token&& token);
	};

	struct UnaryPrefix : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_expr;

		UnaryPrefix(Token&& op, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct UnarySuffix : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_expr;

		UnarySuffix(Token&& op, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Bind : BaseExpression
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		NameContext::Tag m_name_ctx;

		Bind(Token&& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& semantic_tag);
	};

	struct BoundedName : BaseExpression
	{
		Token m_name;
		NameContext::Tag m_name_ctx;

		BoundedName(Token&& name, NameContext::Tag&& name_ctx);
	};

	struct Call : BaseExpression
	{
		Token m_paren;
		std::unique_ptr<MidoriExpression> m_callee;
		std::vector<std::unique_ptr<MidoriExpression>> m_arguments;
		bool m_is_foreign;

		Call(Token&& paren, std::unique_ptr<MidoriExpression>&& callee, std::vector<std::unique_ptr<MidoriExpression>>&& arguments, bool is_foreign = false);
	};

	struct Function : BaseExpression
	{
		Token m_function_keyword;
		std::vector<Token> m_params;
		std::vector<std::shared_ptr<MidoriType>> m_param_types;
		std::shared_ptr<MidoriType> m_return_type;
		std::unique_ptr<MidoriStatement> m_body;
		int m_captured_count;

		Function(Token&& function_keyword, std::vector<Token>&& params, std::unique_ptr<MidoriStatement>&& body, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, int captured_count = 0);
	};

	struct Construct : BaseExpression
	{
		struct Union
		{
			int m_index;
		};
		struct Struct {};
		using ConstructContext = std::variant<Union, Struct>;

		Token m_data_name;
		std::vector<std::unique_ptr<MidoriExpression>> m_params;
		std::shared_ptr<MidoriType> m_return_type;
		ConstructContext m_construct_ctx;

		Construct(Token&& data_name, std::vector<std::unique_ptr<MidoriExpression>>&& params, std::shared_ptr<MidoriType>&& return_type, ConstructContext&& construct_ctx);

		template<typename T>
		constexpr bool IsConstructTypeOf()
		{
			return std::holds_alternative<T>(m_construct_ctx);
		}
	};

	struct Ternary : BaseExpression
	{
		Token m_question;
		Token m_colon;
		std::unique_ptr<MidoriExpression> m_condition;
		std::unique_ptr<MidoriExpression> m_true_branch;
		std::unique_ptr<MidoriExpression> m_else_branch;
		ConditionOperandType m_condition_operand_type;

		Ternary(Token&& question, Token&& colon, std::unique_ptr<MidoriExpression>&& condition, std::unique_ptr<MidoriExpression>&& true_branch, std::unique_ptr<MidoriExpression>&& else_branch, ConditionOperandType condition_operand_type);
	};

	struct Get : BaseExpression
	{
		Token m_member_name;
		std::unique_ptr<MidoriExpression> m_struct;
		int m_index;

		Get(Token&& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, int index = -1);
	};

	struct Set : BaseExpression
	{
		Token m_member_name;
		std::unique_ptr<MidoriExpression> m_struct;
		std::unique_ptr<MidoriExpression> m_value;
		int m_index;

		Set(Token&& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, std::unique_ptr<MidoriExpression>&& value, int index = -1);
	};

	struct Array : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_elems;

		Array(Token&& op, std::vector<std::unique_ptr<MidoriExpression>>&& elems);
	};

	struct ArrayGet : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_indices;
		std::unique_ptr<MidoriExpression> m_arr_var;

		ArrayGet(Token&& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var);
	};

	struct ArraySet : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_indices;
		std::unique_ptr<MidoriExpression> m_arr_var;
		std::unique_ptr<MidoriExpression> m_value;

		ArraySet(Token&& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var, std::unique_ptr<MidoriExpression>&& value);
	};

private:
	using ExpressionUnion = std::variant<As, Binary, Group, TextLiteral, BoolLiteral, FractionLiteral, IntegerLiteral, UnitLiteral, UnaryPrefix, UnarySuffix, Bind, BoundedName, Call, Function, Construct, Ternary, Get, Set, Array, ArrayGet, ArraySet>;
	ExpressionUnion m_expr_data;

public:
	template<typename T>
	MidoriExpression(T&& expr_data) : m_expr_data(std::move(expr_data))
	{
	}

	template<typename T>
	T& GetExpression()
	{
		return std::get<T>(m_expr_data);
	}

	template<typename T>
	constexpr bool IsExpression()
	{
		return std::holds_alternative<T>(m_expr_data);
	}

	ExpressionUnion& operator*();

	std::shared_ptr<MidoriType>& GetType();
};

struct Block
{
	Token m_right_brace;
	std::vector<std::unique_ptr<MidoriStatement>> m_stmts;
	int m_local_count = 0;
};

struct Simple
{
	Token m_semicolon;
	std::unique_ptr<MidoriExpression> m_expr;
};

struct Define
{
	Token m_name;
	std::unique_ptr<MidoriExpression> m_value;
	std::optional<std::shared_ptr<MidoriType>> m_annotated_type;
	std::optional<int> m_local_index;
};

struct If
{
	Token m_if_keyword;
	std::optional<Token> m_else_keyword;
	std::optional<std::unique_ptr<MidoriStatement>> m_else_branch;
	std::unique_ptr<MidoriExpression> m_condition;
	std::unique_ptr<MidoriStatement> m_true_branch;
	MidoriExpression::ConditionOperandType m_condition_operand_type;
};

struct While
{
	Token m_loop_keyword;
	std::unique_ptr<MidoriExpression> m_condition;
	std::unique_ptr<MidoriStatement> m_body;
};

struct For
{
	Token m_loop_keyword;
	std::unique_ptr<MidoriExpression> m_condition;
	std::unique_ptr<MidoriStatement> m_condition_incrementer;
	std::unique_ptr<MidoriStatement> m_condition_intializer;
	std::unique_ptr<MidoriStatement> m_body;
	int m_control_block_local_count = 0;
};

struct Break
{
	Token m_keyword;
	int m_number_to_pop = 0;
};

struct Continue
{
	Token m_keyword;
	int m_number_to_pop = 0;
};

struct Return
{
	Token m_keyword;
	std::unique_ptr<MidoriExpression> m_value;
};

struct Foreign
{
	Token m_function_name;
	std::string m_foreign_name;
	std::shared_ptr<MidoriType> m_type;
	std::optional<int> m_local_index;
};

struct Struct
{
	Token m_name;
	std::shared_ptr<MidoriType> m_self_type;
};

struct Union
{
	Token m_name;
	std::shared_ptr<MidoriType> m_self_type;
};

struct Switch
{
	struct MemberCase
	{
		Token m_keyword;
		std::vector<std::string> m_binding_names;
		std::string m_member_name;
		std::unique_ptr<MidoriStatement> m_stmt;
		int m_tag;
	};

	struct DefaultCase
	{
		Token m_keyword;
		std::unique_ptr<MidoriStatement> m_stmt;
	};

	using Case = std::variant<MemberCase, DefaultCase>;

	Token m_switch_keyword;
	std::unique_ptr<MidoriExpression> m_arg_expr;
	std::vector<Case> m_cases;

	static bool IsMemberCase(const Case& c);

	static bool IsDefaultCase(const Case& c);

	static MemberCase& GetMemberCase(const Case& c);

	static DefaultCase& GetDefaultCase(const Case& c);

	static const Token& GetKeyword(const Case& c);

	static const std::unique_ptr<MidoriStatement>& GetCaseStatement(const Case& c);
};

struct Namespace
{
	Token m_name;
	std::vector<std::unique_ptr<MidoriStatement>> m_stmts;
};

using MidoriProgramTree = std::vector<std::unique_ptr<MidoriStatement>>;