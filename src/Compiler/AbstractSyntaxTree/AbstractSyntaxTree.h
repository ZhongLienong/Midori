#pragma once

#include <functional>
#include <memory>
#include <variant>
#include <optional>
#include <utility>

#include "Compiler/Token/Token.h"
#include "Type.h"

class MidoriExpression;

class MidoriStatement
{
public:

	struct ExpressionStatement
	{
		Token m_semicolon;
		std::unique_ptr<MidoriExpression> m_expr;

		ExpressionStatement(const Token& semicolon, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct VariableDefinition
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		std::optional<std::shared_ptr<MidoriType>> m_annotated_type;
		std::optional<int> m_local_index;

		VariableDefinition(const Token& name, std::unique_ptr<MidoriExpression>&& value, std::optional<std::shared_ptr<MidoriType>>&& annotated_type, std::optional<int>&& local_index);
	};

	struct TupleDefinition
	{
		std::vector<Token> m_names;
		std::unique_ptr<MidoriExpression> m_value;
		std::vector<std::optional<int>> m_local_indices;

		TupleDefinition(std::vector<Token>&& names, std::unique_ptr<MidoriExpression>&& value, std::vector<std::optional<int>>&& local_indices);
	};

	struct FunctionDefinition
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::vector<Token> m_params;
		std::vector<std::shared_ptr<MidoriType>> m_param_types;
		std::vector<MidoriType::ClassConstraint> m_constraints;
		std::shared_ptr<MidoriType> m_return_type;
		std::unique_ptr<MidoriExpression> m_body;
		std::optional<int> m_local_index;
		int m_captured_count;

		FunctionDefinition(const Token& name, std::vector<Token>&& generic_params, std::vector<Token>&& params, std::vector<std::shared_ptr<MidoriType>>&& param_types, std::shared_ptr<MidoriType>&& return_type, std::unique_ptr<MidoriExpression>&& body, std::optional<int>&& local_index, int captured_count = 0, std::vector<MidoriType::ClassConstraint>&& constraints = {});
	};

	struct Continue
	{
		Token m_keyword;
		int m_number_to_pop = 0;

		Continue(const Token& keyword, int number_to_pop);
	};

	struct ForeignDefinition
	{
		Token m_function_name;
		std::string m_foreign_name;
		std::shared_ptr<MidoriType> m_type;
		std::optional<int> m_local_index;

		ForeignDefinition(const Token& function_name, const std::string& foreign_name, std::shared_ptr<MidoriType>&& type, std::optional<int>&& local_index);
	};

	struct Struct
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::vector<MidoriType::ClassConstraint> m_constraints;
		std::shared_ptr<MidoriType> m_self_type;

		Struct(const Token& name, std::vector<Token>&& generic_params, std::vector<MidoriType::ClassConstraint>&& constraints, std::shared_ptr<MidoriType>&& self_type);
	};

	struct Union
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::vector<MidoriType::ClassConstraint> m_constraints;
		std::shared_ptr<MidoriType> m_self_type;

		Union(const Token& name, std::vector<Token>&& generic_params, std::vector<MidoriType::ClassConstraint>&& constraints, std::shared_ptr<MidoriType>&& self_type);
	};

	struct Class
	{
		Token m_name;
		std::vector<Token> m_type_params;
		std::vector<MidoriType::ClassConstraint> m_superclasses;
		std::vector<std::unique_ptr<MidoriStatement>> m_methods;

		Class(const Token& name, std::vector<Token>&& type_params, std::vector<MidoriType::ClassConstraint>&& superclasses, std::vector<std::unique_ptr<MidoriStatement>>&& methods);
	};

	struct Instance
	{
		Token m_class_name;
		std::vector<std::shared_ptr<MidoriType>> m_type_args;
		std::vector<MidoriType::ClassConstraint> m_constraints;
		std::vector<std::unique_ptr<MidoriStatement>> m_methods;

		Instance(const Token& class_name, std::vector<std::shared_ptr<MidoriType>>&& type_args, std::vector<MidoriType::ClassConstraint>&& constraints, std::vector<std::unique_ptr<MidoriStatement>>&& methods);
	};

	struct TypeAlias
	{
		Token m_name;
		std::vector<Token> m_generic_params;
		std::shared_ptr<MidoriType> m_aliased_type;

		TypeAlias(const Token& name, std::vector<Token>&& generic_params, std::shared_ptr<MidoriType>&& aliased_type);
	};

private:
	using StatementUnion = std::variant<ExpressionStatement, VariableDefinition, TupleDefinition, FunctionDefinition, Continue, ForeignDefinition, Struct, Union, Class, Instance, TypeAlias>;
	StatementUnion m_variant;

public:
	template<typename T>
	MidoriStatement(T&& stmt_data) : m_variant(std::move(stmt_data))
	{
	}

	template<typename T>
	T& GetStatement()
	{
		return std::get<T>(m_variant);
	}

	template<typename T>
	constexpr bool IsStatement() const
	{
		return std::holds_alternative<T>(m_variant);
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
		bool m_uses_convertable = false;

		As(const Token& as_keyword, std::shared_ptr<MidoriType> to_type, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Binary : BaseExpression
	{
		Token m_op;
		std::unique_ptr<MidoriExpression> m_left;
		std::unique_ptr<MidoriExpression> m_right;
		bool m_uses_equatable = false;   // True if using Equatable typeclass for == or !=
		bool m_uses_orderable = false;   // True if using Orderable typeclass for <, >, <=, >=

		Binary(const Token& op, std::unique_ptr<MidoriExpression>&& left, std::unique_ptr<MidoriExpression>&& right);
	};

	struct Group : BaseExpression
	{
		std::unique_ptr<MidoriExpression> m_expr_in;

		Group(std::unique_ptr<MidoriExpression>&& expr_in);
	};

	struct Tuple : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_elements;

		Tuple(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& elements);
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

	struct ByteLiteral : BaseExpression
	{
		Token m_token;

		ByteLiteral(const Token& token);
	};

	struct WordLiteral : BaseExpression
	{
		Token m_token;

		WordLiteral(const Token& token);
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

	struct Assignment : BaseExpression
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		NameContext::Tag m_name_ctx;

		Assignment(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& semantic_tag);
	};

	struct AppendAssign : BaseExpression
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		NameContext::Tag m_name_ctx;

		AppendAssign(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& semantic_tag);
	};

	struct PrependAssign : BaseExpression
	{
		Token m_name;
		std::unique_ptr<MidoriExpression> m_value;
		NameContext::Tag m_name_ctx;

		PrependAssign(const Token& name, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& semantic_tag);
	};

	struct CompoundAssign : BaseExpression
	{
		Token m_name;
		Token m_op;
		std::unique_ptr<MidoriExpression> m_value;
		NameContext::Tag m_name_ctx;

		CompoundAssign(const Token& name, const Token& op, std::unique_ptr<MidoriExpression>&& value, NameContext::Tag&& semantic_tag);
	};

	struct NameAccess : BaseExpression
	{
		Token m_name;
		NameContext::Tag m_name_ctx;

		NameAccess(const Token& name, NameContext::Tag&& name_ctx);
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

	struct MemberAccess : BaseExpression
	{
		Token m_member_name;
		std::unique_ptr<MidoriExpression> m_struct;
		int m_index;

		MemberAccess(const Token& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, int index = -1);
	};

	struct MemberAssignment : BaseExpression
	{
		Token m_member_name;
		std::unique_ptr<MidoriExpression> m_struct;
		std::unique_ptr<MidoriExpression> m_value;
		int m_index;

		MemberAssignment(const Token& member_name, std::unique_ptr<MidoriExpression>&& struct_expr, std::unique_ptr<MidoriExpression>&& value, int index = -1);
	};

	struct Array : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_elems;

		Array(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& elems);
	};

	struct IndexAccess : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_indices;
		std::unique_ptr<MidoriExpression> m_arr_var;

		IndexAccess(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var);
	};

	struct IndexAssignment : BaseExpression
	{
		Token m_op;
		std::vector<std::unique_ptr<MidoriExpression>> m_indices;
		std::unique_ptr<MidoriExpression> m_arr_var;
		std::unique_ptr<MidoriExpression> m_value;

		IndexAssignment(const Token& op, std::vector<std::unique_ptr<MidoriExpression>>&& indices, std::unique_ptr<MidoriExpression>&& arr_var, std::unique_ptr<MidoriExpression>&& value);
	};

	struct ArrayComprehension : BaseExpression
	{
		Token m_bracket;                
		Token m_loop_variable;                         
		Token m_in_keyword;                                 
		std::unique_ptr<MidoriExpression> m_transform_expr;
		std::unique_ptr<MidoriExpression> m_range;    
		int m_loop_variable_index = -1;
		int m_hidden_step_index = -1;    
		int m_hidden_end_index = -1;  
		int m_hidden_array_index = -1;
		int m_result_array_index = -1;  
		bool m_is_array_iteration = false;

		ArrayComprehension(const Token& bracket, const Token& loop_variable, const Token& in_keyword, std::unique_ptr<MidoriExpression>&& transform_expr, std::unique_ptr<MidoriExpression>&& range);
	};

	struct RangeBinary : BaseExpression
	{
		Token m_range_op;
		std::unique_ptr<MidoriExpression> m_start;
		std::unique_ptr<MidoriExpression> m_end;

		RangeBinary(const Token& range_op, std::unique_ptr<MidoriExpression>&& start, std::unique_ptr<MidoriExpression>&& end);
	};

	struct RangeTernary : BaseExpression
	{
		Token m_first_range_op;
		Token m_second_range_op;
		std::unique_ptr<MidoriExpression> m_start;
		std::unique_ptr<MidoriExpression> m_step;
		std::unique_ptr<MidoriExpression> m_end;

		RangeTernary(const Token& first_op, const Token& second_op, std::unique_ptr<MidoriExpression>&& start, std::unique_ptr<MidoriExpression>&& step, std::unique_ptr<MidoriExpression>&& end);
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

	struct For : BaseExpression
	{
		Token m_for_keyword;
		Token m_loop_variable;
		Token m_in_keyword;
		std::unique_ptr<MidoriExpression> m_range;
		std::unique_ptr<MidoriExpression> m_body;
		int m_loop_variable_index = -1;
		int m_hidden_step_index = -1;   // For range: step; For array: current index
		int m_hidden_end_index = -1;    // For range: end; For array: length
		int m_hidden_array_index = -1;  // For array iteration: stores array reference
		bool m_is_array_iteration = false;

		For(const Token& for_keyword, const Token& loop_variable, const Token& in_keyword, std::unique_ptr<MidoriExpression>&& range, std::unique_ptr<MidoriExpression>&& body);
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

	struct Async : BaseExpression
	{
		Token m_keyword;
		std::unique_ptr<MidoriExpression> m_expr;
		int m_captured_count = 0;

		Async(const Token& keyword, std::unique_ptr<MidoriExpression>&& expr);
	};

	struct Await : BaseExpression
	{
		Token m_keyword;
		std::unique_ptr<MidoriExpression> m_expr;

		Await(const Token& keyword, std::unique_ptr<MidoriExpression>&& expr);
	};

private:
	using ExpressionUnion = std::variant<As, Binary, Group, Tuple, TextLiteral, BoolLiteral, FloatLiteral, IntegerLiteral, ByteLiteral, WordLiteral, UnitLiteral, UnaryPrefix, UnarySuffix, Assignment, AppendAssign, PrependAssign, CompoundAssign, NameAccess, Call, Function, Construct, IfElse, MemberAccess, MemberAssignment, Array, IndexAccess, IndexAssignment, ArrayComprehension, RangeBinary, RangeTernary, Block, Match, Case, Default, Loop, For, Return, Break, Async, Await>;
	ExpressionUnion m_variant;

public:
	template<typename T>
	MidoriExpression(T&& expr_data) : m_variant(std::move(expr_data))
	{
	}

	template<typename T>
	T& GetExpression()
	{
		return std::get<T>(m_variant);
	}

	template<typename T>
	constexpr bool IsExpression() const
	{
		return std::holds_alternative<T>(m_variant);
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
					return node.m_expr->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Binary>)
				{
					return node.m_left->template Contains<Kind>() || node.m_right->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnaryPrefix>)
				{
					return node.m_expr->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnarySuffix>)
				{
					return node.m_expr->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Group>)
				{
					return node.m_expr_in->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IfElse>)
				{
					return node.m_condition->template Contains<Kind>() || node.m_true_branch->template Contains<Kind>() || node.m_else_branch->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Block>)
				{
					return std::ranges::any_of
					(
						node.m_stmts, [this](const std::unique_ptr<MidoriStatement>& stmt)
						{
							return stmt->template IsStatement<MidoriStatement::ExpressionStatement>() && stmt->template GetStatement<MidoriStatement::ExpressionStatement>().m_expr->template Contains<Kind>();
						}
					)
						|| (node.m_final_expr.has_value() && node.m_final_expr.value()->template Contains<Kind>());
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Loop>)
				{
					return node.m_body->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Function>)
				{
					return node.m_body->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Call>)
				{
					if (node.m_callee->template Contains<Kind>())
					{
						return true;
					}
					for (const std::unique_ptr<MidoriExpression>& arg : node.m_arguments)
					{
						if (arg->template Contains<Kind>())
						{
							return true;
						}
					}
					return false;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Return>)
				{
					return node.m_value->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Break>)
				{
					return node.m_value->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::ArrayComprehension>)
				{
					return node.m_transform_expr->template Contains<Kind>() || node.m_range->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Async>)
				{
					return node.m_expr->template Contains<Kind>();
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Await>)
				{
					return node.m_expr->template Contains<Kind>();
				}
				else
				{
					return false;
				}
			},
			m_variant
		);
	}
};

template<typename Visitor>
decltype(auto) VisitNode(Visitor&& visitor, MidoriStatement& statement)
{
	return std::visit(std::forward<Visitor>(visitor), *statement);
}

template<typename Visitor>
decltype(auto) VisitNode(Visitor&& visitor, std::unique_ptr<MidoriStatement>& statement)
{
	return std::visit(std::forward<Visitor>(visitor), **statement);
}

template<typename Visitor>
decltype(auto) VisitNode(Visitor&& visitor, const std::unique_ptr<MidoriStatement>& statement)
{
	return std::visit(std::forward<Visitor>(visitor), **statement);
}

template<typename Visitor>
decltype(auto) VisitNode(Visitor&& visitor, MidoriExpression& expression)
{
	return std::visit(std::forward<Visitor>(visitor), *expression);
}

template<typename Visitor>
decltype(auto) VisitNode(Visitor&& visitor, std::unique_ptr<MidoriExpression>& expression)
{
	return std::visit(std::forward<Visitor>(visitor), **expression);
}

template<typename Visitor>
decltype(auto) VisitNode(Visitor&& visitor, const std::unique_ptr<MidoriExpression>& expression)
{
	return std::visit(std::forward<Visitor>(visitor), **expression);
}
