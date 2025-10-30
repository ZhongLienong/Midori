#pragma once

#include <functional>
#include <memory>
#include <variant>
#include <optional>

#include "Compiler/Token/Token.h"
#include "Type.h"

class MidoriExpression;

class MidoriStatement
{
public:

	struct Simple
	{
		Token m_semicolon;
		std::unique_ptr<MidoriExpression> m_expr;

		Simple(const Token& semicolon, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Define
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		std::optional<std::shared_ptr<MidoriType>> m_annotated_type;
		std::optional<int> m_local_index;

		Define(const Token& name, std::unique_ptr<MidoriExpression>&& value, std::optional<std::shared_ptr<MidoriType>>&& annotated_type, std::optional<int>&& local_index);
	};

	struct DefineFunction
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::vector<Token> m_params;
		std::vector<std::shared_ptr<MidoriType>> m_param_types;
		std::shared_ptr<MidoriType> m_return_type;
		std::unique_ptr<MidoriExpression> m_body;
		std::optional<int> m_local_index;
		int m_captured_count;

		DefineFunction(const Token& name, std::vector<Token>&& generic_params, std::vector<Token>&& params, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, std::unique_ptr<MidoriExpression>&& body, std::optional<int>&& local_index, int captured_count = 0);
	};

	struct Continue
	{
		Token m_keyword;
		int m_number_to_pop = 0;

		Continue(const Token& keyword, int number_to_pop);
	};

	struct Foreign
	{
		Token m_function_name;
		std::string m_foreign_name;
		std::shared_ptr<MidoriType> m_type;
		std::optional<int> m_local_index;

		Foreign(const Token& function_name, const std::string& foreign_name, std::shared_ptr<MidoriType>&& type, std::optional<int>&& local_index);
	};

	struct Struct
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::shared_ptr<MidoriType> m_self_type;

		Struct(const Token& name, std::vector<Token>&& generic_params, std::shared_ptr<MidoriType>&& self_type);
	};

	struct Union
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::shared_ptr<MidoriType> m_self_type;

		Union(const Token& name, std::vector<Token>&& generic_params, std::shared_ptr<MidoriType>&& self_type);
	};

	struct Namespace
	{
		Token m_name;
		std::vector<std::unique_ptr<MidoriStatement>> m_stmts;

		Namespace(const Token& name, std::vector<std::unique_ptr<MidoriStatement>>&& stmts);
	};

private:
	using StatementUnion = std::variant<Simple, Define, DefineFunction, Continue, Foreign, Struct, Union, Namespace>;
	StatementUnion m_stmt_data;

public:
	template<typename T>
	MidoriStatement(T&& stmt_data) : m_stmt_data(std::move(stmt_data))
	{
	}

	template<typename T>
	T& GetStatement()
	{
		return std::get<T>(m_stmt_data);
	}

	template<typename T>
	constexpr bool IsStatement() const
	{
		return std::holds_alternative<T>(m_stmt_data);
	}

	StatementUnion& operator*();
};

using MidoriProgramTree = std::vector<std::unique_ptr<MidoriStatement>>;

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
		FLOAT,
		OTHER
	};

	struct BaseExpression
	{
		std::shared_ptr<MidoriType> m_type_data = MidoriType::MakeUndecidedType();
	};

	struct As : BaseExpression
	{
		Token m_as_keyword;
		std::weak_ptr<MidoriType> m_from_type;
		std::shared_ptr<MidoriType> m_to_type;
		std::unique_ptr<MidoriExpression> m_expr;

		As(const Token& as_keyword, std::shared_ptr<MidoriType> to_type, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Binary : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_left;
		std::unique_ptr<MidoriExpression> m_right;

		Binary(const Token& op, std::unique_ptr<MidoriExpression>&& left, std::unique_ptr<MidoriExpression>&& right);
	};

	struct Group : BaseExpression
	{
		std::unique_ptr<MidoriExpression> m_expr_in;

		Group(std::unique_ptr<MidoriExpression>&& expr_in);
	};

	struct TextLiteral : BaseExpression
	{
		Token m_token;

		TextLiteral(const Token& token);
	};

	struct BoolLiteral : BaseExpression
	{
		Token m_token;

		BoolLiteral(const Token& token);
	};

	struct FloatLiteral : BaseExpression
	{
		Token m_token;

		FloatLiteral(const Token& token);
	};

	struct IntegerLiteral : BaseExpression
	{
		Token m_token;

		IntegerLiteral(const Token& token);
	};

	struct UnitLiteral : BaseExpression
	{
		Token m_token;

		UnitLiteral(const Token& token);
	};

	struct UnaryPrefix : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_expr;

		UnaryPrefix(const Token& op, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct UnarySuffix : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_expr;

		UnarySuffix(const Token& op, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Bind : BaseExpression
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		NameContext::Tag m_name_ctx;

		Bind(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& semantic_tag);
	};

	struct BoundedName : BaseExpression
	{
		Token m_name;
		NameContext::Tag m_name_ctx;

		BoundedName(const Token& name, NameContext::Tag&& name_ctx);
	};

	struct Call : BaseExpression
	{
		Token m_paren;
		std::unique_ptr<MidoriExpression> m_callee;
		std::vector<std::unique_ptr<MidoriExpression>> m_arguments;
		bool m_is_foreign;
		bool m_is_tail_call = false;

		Call(const Token& paren, std::unique_ptr<MidoriExpression>&& callee, std::vector<std::unique_ptr<MidoriExpression>>&& arguments, bool is_foreign = false);
	};

	struct Function : BaseExpression
	{
		Token m_function_keyword;
		std::vector<Token> m_generic_params; 
		std::vector<Token> m_params;
		std::vector<std::shared_ptr<MidoriType>> m_param_types;
		std::shared_ptr<MidoriType> m_return_type;
		std::unique_ptr<MidoriExpression> m_body;
		int m_captured_count;

		Function(const Token& function_keyword, std::vector<Token>&& generic_params, std::vector<Token>&& params, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, std::unique_ptr<MidoriExpression>&& body, int captured_count = 0);
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

		Construct(const Token& data_name, std::vector<std::unique_ptr<MidoriExpression>>&& params, std::shared_ptr<MidoriType>&& return_type, ConstructContext&& construct_ctx);

		template<typename T>
		constexpr bool IsConstructTypeOf()
		{
			return std::holds_alternative<T>(m_construct_ctx);
		}
	};

	struct IfElse : BaseExpression
	{
		Token m_if_token;
		Token m_then_token;
		Token m_else_token;
		std::unique_ptr<MidoriExpression> m_condition;
		std::unique_ptr<MidoriExpression> m_true_branch;
		std::unique_ptr<MidoriExpression> m_else_branch;
		ConditionOperandType m_condition_operand_type;

		IfElse(const Token& if_token, const Token& then_token, const Token& else_token, std::unique_ptr<MidoriExpression>&& condition, std::unique_ptr<MidoriExpression>&& true_branch, std::unique_ptr<MidoriExpression>&& else_branch, ConditionOperandType condition_operand_type);
	};

	struct Get : BaseExpression
	{
		Token m_member_name;
		std::unique_ptr<MidoriExpression> m_struct;
		int m_index;

		Get(const Token& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, int index = -1);
	};

	struct Set : BaseExpression
	{
		Token m_member_name;
		std::unique_ptr<MidoriExpression> m_struct;
		std::unique_ptr<MidoriExpression> m_value;
		int m_index;

		Set(const Token& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, std::unique_ptr<MidoriExpression>&& value, int index = -1);
	};

	struct Array : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_elems;

		Array(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& elems);
	};

	struct ArrayGet : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_indices;
		std::unique_ptr<MidoriExpression> m_arr_var;

		ArrayGet(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var);
	};

	struct ArraySet : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_indices;
		std::unique_ptr<MidoriExpression> m_arr_var;
		std::unique_ptr<MidoriExpression> m_value;

		ArraySet(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var, std::unique_ptr<MidoriExpression>&& value);
	};

	struct Block : BaseExpression
	{
		Token m_right_brace;
		std::vector<std::unique_ptr<MidoriStatement>> m_stmts;
		std::optional<std::unique_ptr<MidoriExpression>> m_final_expr = std::nullopt;
		int m_local_count = 0;

		Block(const Token& right_brace, std::vector<std::unique_ptr<MidoriStatement>>&& stmts, int local_count, std::unique_ptr<MidoriExpression>&& final_expr = nullptr);

		bool HasDefine() const;
	};

	struct Match : BaseExpression
	{
		Token m_match_keyword;
		std::unique_ptr<MidoriExpression> m_arg_expr;
		std::vector<std::unique_ptr<MidoriExpression>> m_cases;

		Match(const Token& match_keyword, std::unique_ptr<MidoriExpression>&& arg_expr, std::vector<std::unique_ptr<MidoriExpression>>&& cases);
	};

	struct Case : BaseExpression
	{
		Token m_keyword;
		std::vector<std::string> m_binding_names;
		std::string m_member_name;
		std::unique_ptr<MidoriExpression> m_expr;
		int m_tag;

		Case(const Token& keyword, std::vector<std::string>&& binding_names, const std::string& member_name, std::unique_ptr<MidoriExpression>&& expr, int tag);
	};

	struct Default : BaseExpression
	{
		Token m_keyword;
		std::unique_ptr<MidoriExpression> m_expr;

		Default(const Token& keyword, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Loop : BaseExpression
	{
		Token m_loop_keyword;
		std::unique_ptr<MidoriExpression> m_body;

		Loop(const Token& loop_keyword, std::unique_ptr<MidoriExpression>&& body);
	};

	struct Return : BaseExpression
	{
		Token m_keyword;
		std::unique_ptr<MidoriExpression> m_value;

		Return(const Token& keyword, std::unique_ptr<MidoriExpression>&& value);
	};

	struct Break : BaseExpression
	{
		Token m_keyword;
		int m_number_to_pop = 0;
		std::unique_ptr<MidoriExpression> m_value;

		Break(const Token& keyword, int number_to_pop, std::unique_ptr<MidoriExpression>&& value);
	};

private:
	using ExpressionUnion = std::variant<As, Binary, Group, TextLiteral, BoolLiteral, FloatLiteral, IntegerLiteral, UnitLiteral, UnaryPrefix, UnarySuffix, Bind, BoundedName, Call, Function, Construct, IfElse, Get, Set, Array, ArrayGet, ArraySet, Block, Match, Case, Default, Loop, Return, Break>;
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
	constexpr bool IsExpression() const
	{
		return std::holds_alternative<T>(m_expr_data);
	}

	ExpressionUnion& operator*();

	std::shared_ptr<MidoriType>& GetType();

	template<typename Kind>
	bool Contains() const 
	{
		return std::visit
		(
			[this](auto&& node) -> bool
			{
				using T = std::decay_t<decltype(node)>;

				if constexpr (std::is_same_v<T, Kind>)
				{
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::As>)
				{
					return node.m_expr->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Binary>)
				{
					return node.m_left->Contains<Kind>() || node.m_right->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnaryPrefix>)
				{
					return node.m_expr->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnarySuffix>)
				{
					return node.m_expr->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Group>)
				{
					return node.m_expr_in->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IfElse>)
				{
					return node.m_condition->Contains<Kind>() || node.m_true_branch->Contains<Kind>() || node.m_else_branch->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Block>)
				{
					return std::ranges::any_of
					(
						node.m_stmts, [this](const std::unique_ptr<MidoriStatement>& stmt)
						{
							return stmt->IsStatement<MidoriStatement::Simple>() && stmt->GetStatement<MidoriStatement::Simple>().m_expr->Contains<Kind>();
						}
					)
						|| (node.m_final_expr.has_value() && node.m_final_expr.value()->Contains<Kind>());
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Loop>)
				{
					return node.m_body->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Function>)
				{
					return node.m_body->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Call>)
				{
					if (node.m_callee->Contains<Kind>())
					{
						return true;
					}
					for (const std::unique_ptr<MidoriExpression>& arg : node.m_arguments)
					{
						if (arg->Contains<Kind>())
						{
							return true;
						}
					}
					return false;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Return>)
				{
					return node.m_value->Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Break>)
				{
					return node.m_value->Contains<Kind>();
				}
				else
				{
					return false;
				}
			},
			m_expr_data
		);
	}
};