#pragma once

#include <array>
#include <unordered_map>
#include <unordered_set>

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"

class ExpectedTypeGuard;

struct TypePairHash
{
	std::size_t operator()(const std::pair<MidoriType*, MidoriType*>& pair) const noexcept
	{
		std::size_t h1 = std::hash<MidoriType*>{}(pair.first);
		std::size_t h2 = std::hash<MidoriType*>{}(pair.second);
		return h1 ^ (h2 << 1);
	}
};

class TypeChecker
{
	friend class ExpectedTypeGuard;

public:
	using TypeEnvironment = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;

	struct ClassInfo
	{
		std::string m_name;
		std::vector<std::string> m_type_param_names;
		std::vector<MidoriType::ClassConstraint> m_superclasses;
		std::unordered_map<std::string, std::shared_ptr<MidoriType>> m_method_types;
		std::unordered_set<std::string> m_methods_with_defaults;

		ClassInfo() = default;
		ClassInfo(const std::string& name, std::vector<std::string>&& params, std::vector<MidoriType::ClassConstraint>&& supers, std::unordered_map<std::string, std::shared_ptr<MidoriType>>&& methods, std::unordered_set<std::string>&& defaults);
	};

private:
	using TypeEnvironmentStack = std::vector<TypeEnvironment>;
	using TypeSubstitution = std::unordered_map<int, std::shared_ptr<MidoriType>>;
	using GenericFunctionNames = std::unordered_set<std::string>;
	using GenericStructNames = std::unordered_set<std::string>;
	using GenericUnionNames = std::unordered_set<std::string>;
	struct FresheningContext
	{
		std::unordered_map<std::string, std::shared_ptr<MidoriType>> m_generic_params;
		std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>> m_type_cache;  // Maps type pointers to their fresh versions (for cycle detection)
	};

	struct InstanceKey
	{
		std::string m_class_name;
		std::vector<std::string> m_concrete_types;

		bool operator==(const InstanceKey& other) const;
	};

	struct InstanceKeyHash
	{
		std::size_t operator()(const InstanceKey& key) const;
	};

	struct InstanceInfo
	{
		std::string m_class_name;
		std::vector<std::shared_ptr<MidoriType>> m_type_args;
		std::vector<MidoriType::ClassConstraint> m_constraints;
		std::unordered_map<std::string, std::unique_ptr<MidoriStatement>> m_method_impls;

		InstanceInfo() = default;
		InstanceInfo(const std::string& tc_name, std::vector<std::shared_ptr<MidoriType>>&& args, std::vector<MidoriType::ClassConstraint>&& constraints, std::unordered_map<std::string, std::unique_ptr<MidoriStatement>>&& methods);
	};

	MidoriProgramTree m_program_tree;
	std::string m_file_name;
	TypeEnvironmentStack m_name_type_table;
	TypeSubstitution m_type_substitution;
	GenericFunctionNames m_generic_functions;
	GenericStructNames m_generic_structs;
	GenericUnionNames m_generic_unions;
	std::unordered_map<std::string, ClassInfo> m_classes;
	std::unordered_map<InstanceKey, InstanceInfo, InstanceKeyHash> m_instances;
	std::vector<MidoriType::ClassConstraint> m_active_constraints;
	const std::vector<std::string>& m_source_lines;
	int m_next_type_var_id;
	std::shared_ptr<MidoriType> m_expected_return_type;
	std::shared_ptr<MidoriType> m_expected_break_type;
	std::shared_ptr<MidoriType> m_expected_expr_type; 
	const std::array<Token::Name, 5u> m_binary_arithmetic_operators{ Token::Name::SINGLE_PLUS, Token::Name::SINGLE_MINUS, Token::Name::STAR, Token::Name::SLASH, Token::Name::PERCENT };
	const std::array<Token::Name, 1u> m_binary_concatenation_operators{ Token::Name::DOUBLE_PLUS };
	const std::array<Token::Name, 4u> m_binary_partial_order_comparison_operators{ Token::Name::LEFT_ANGLE, Token::Name::LESS_EQUAL, Token::Name::RIGHT_ANGLE, Token::Name::GREATER_EQUAL };
	const std::array<Token::Name, 2u> m_binary_equality_operators{ Token::Name::DOUBLE_EQUAL, Token::Name::BANG_EQUAL };
	const std::array<Token::Name, 2u> m_binary_logical_operators{ Token::Name::DOUBLE_AMPERSAND, Token::Name::DOUBLE_BAR };
	const std::array<Token::Name, 5u> m_binary_bitwise_operators{ Token::Name::CARET, Token::Name::SINGLE_AMPERSAND, Token::Name::SINGLE_BAR, Token::Name::RIGHT_SHIFT, Token::Name::LEFT_SHIFT };

	std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash> m_unify_visited;

public:

	TypeChecker(
		MidoriProgramTree&& parser_result,
		std::string_view file_name,
		const std::vector<std::string>& source_lines,
		TypeEnvironment imported_types = {},
		const std::unordered_map<std::string, ClassInfo>& imported_typeclasses = {}
	);

	MidoriResult::TypeCheckerResult TypeCheck();

	// Extract type signatures from parsed AST (for parallel type checking)
	static TypeEnvironment ExtractTypeSignatures(const MidoriProgramTree& ast, const std::unordered_set<std::string>* exported_symbols = nullptr);

private:

	void BeginScope();

	void EndScope();

	void UpdateConditionOperandType(MidoriExpression::ConditionOperandType& op_type, const std::unique_ptr<MidoriExpression>& expr);

	std::shared_ptr<MidoriType> FreshTypeVar();

	std::shared_ptr<MidoriType> Freshen(const std::shared_ptr<MidoriType>& type);

	std::shared_ptr<MidoriType> Freshen(const std::shared_ptr<MidoriType>& type, FresheningContext& context);

	std::shared_ptr<MidoriType> ApplySubstitution(const std::shared_ptr<MidoriType>& type);

	std::shared_ptr<MidoriType> ApplySubstitution(const std::shared_ptr<MidoriType>& type, std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>& cache);

	bool OccursCheck(int var_id, const std::shared_ptr<MidoriType>& type);

	MidoriResult::TypeResult Unify(const Token& token, std::shared_ptr<MidoriType>& left, std::shared_ptr<MidoriType>& right);

	MidoriResult::TypeResult operator()(MidoriStatement::ExpressionStatement& simple);

	MidoriResult::TypeResult operator()(MidoriStatement::VariableDefinition& def);

	MidoriResult::TypeResult operator()(MidoriStatement::TupleDefinition& def_tuple);

	MidoriResult::TypeResult operator()(MidoriStatement::FunctionDefinition& defun);

	MidoriResult::TypeResult operator()(MidoriStatement::Continue& continue_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::ForeignDefinition& foreign_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Struct& struct_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Union& union_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Class& typeclass_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::Instance& instance_stmt);

	MidoriResult::TypeResult operator()(MidoriStatement::TypeAlias& type_alias);

	MidoriResult::TypeResult operator()(MidoriExpression::As& as);

	MidoriResult::TypeResult operator()(MidoriExpression::Binary& binary);

	MidoriResult::TypeResult operator()(MidoriExpression::Group& group);

	MidoriResult::TypeResult operator()(MidoriExpression::Tuple& tuple);

	MidoriResult::TypeResult operator()(MidoriExpression::UnaryPrefix& unary);

	MidoriResult::TypeResult operator()(MidoriExpression::UnarySuffix& unary);

	MidoriResult::TypeResult operator()(MidoriExpression::Call& call);

	MidoriResult::TypeResult operator()(MidoriExpression::MemberAccess& get);

	MidoriResult::TypeResult operator()(MidoriExpression::MemberAssignment& set);

	MidoriResult::TypeResult operator()(MidoriExpression::NameAccess& variable);

	MidoriResult::TypeResult operator()(MidoriExpression::Assignment& bind);

	MidoriResult::TypeResult operator()(MidoriExpression::AppendAssign& append_assign);

	MidoriResult::TypeResult operator()(MidoriExpression::PrependAssign& prepend_assign);

	MidoriResult::TypeResult operator()(MidoriExpression::CompoundAssign& compound_assign);

	MidoriResult::TypeResult operator()(MidoriExpression::TextLiteral& text);

	MidoriResult::TypeResult operator()(MidoriExpression::BoolLiteral& bool_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::FloatLiteral& float_literal);

	MidoriResult::TypeResult operator()(MidoriExpression::IntegerLiteral& integer);

	MidoriResult::TypeResult operator()(MidoriExpression::ByteLiteral& byte_literal);

	MidoriResult::TypeResult operator()(MidoriExpression::WordLiteral& word_literal);

	MidoriResult::TypeResult operator()(MidoriExpression::UnitLiteral& unit);

	MidoriResult::TypeResult operator()(MidoriExpression::Function& function);

	MidoriResult::TypeResult operator()(MidoriExpression::Construct& construct);

	MidoriResult::TypeResult operator()(MidoriExpression::Array& array);

	MidoriResult::TypeResult operator()(MidoriExpression::IndexAccess& array_get);

	MidoriResult::TypeResult operator()(MidoriExpression::IndexAssignment& array_set);

	MidoriResult::TypeResult operator()(MidoriExpression::RangeBinary& range_binary);

	MidoriResult::TypeResult operator()(MidoriExpression::RangeTernary& range_ternary);

	MidoriResult::TypeResult operator()(MidoriExpression::IfElse& if_else);

	MidoriResult::TypeResult operator()(MidoriExpression::Block& block);

	MidoriResult::TypeResult operator()(MidoriExpression::Match& match);

	MidoriResult::TypeResult operator()(MidoriExpression::Case& case_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Default& default_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Loop& loop);

	MidoriResult::TypeResult operator()(MidoriExpression::For& for_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::ArrayComprehension& comp);

	MidoriResult::TypeResult operator()(MidoriExpression::Return& return_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Break& break_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Async& async_expr);

	MidoriResult::TypeResult operator()(MidoriExpression::Await& await_expr);
};
