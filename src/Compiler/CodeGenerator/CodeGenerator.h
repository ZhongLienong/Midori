#pragma once

#include <algorithm>
#include <stack>
#include <unordered_set>
#include <optional>
#include <set>

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"
#include "Compiler/BytecodeModule/BytecodeModule.h"
#include "Library/MidoriBuiltinFFIRegistry/MidoriFFIRegistry.h"
#include "GenericFunctionInfo.h"

class CodeGenerator
{
private:

	struct ResolvedMethodCandidate
	{
		std::string m_first_type_name;
		std::string m_second_type_name;
		std::string m_resolved_name;
		bool m_has_instance = false;
	};

	struct LoopContext
	{
		std::vector<int> m_break_positions;
		int m_loop_start = 0;
		int m_continue_target = 0;
	};



	struct FunctionSignature
	{
		std::string m_base_name;
		std::vector<std::string> m_concrete_types;

		bool operator==(const FunctionSignature& other) const;
	};

	struct FunctionSignatureHash
	{
		std::size_t operator()(const FunctionSignature& sig) const;
	};

	struct TypePairHash
	{
		std::size_t operator()(const std::pair<MidoriType*, MidoriType*>& pair) const;
	};

	struct BytecodeBuilder
	{
		MidoriExecutable::Procedures m_procedures{ BytecodeStream() };
		std::vector<MidoriText> m_procedure_names;
		MidoriExecutable::StringPool m_string_pool;
		size_t m_current_procedure_index = 0u;
		int m_string_pool_index = 0;
		OpCode m_last_opcode = OpCode::HALT;

		BytecodeBuilder EmitByte(OpCode byte, int line) &&;
		BytecodeBuilder PopByte(int line) &&;
	};

	using TypeEnvironment = std::unordered_map<std::string, std::shared_ptr<MidoriType>>;
	using TypeclassMethodMap = std::unordered_map<std::string, std::unordered_set<std::string>>;
	using TypeclassInstanceMap = std::unordered_map<std::string, std::vector<std::string>>;
	using TypeclassInstanceTypeMap = std::unordered_map<std::string, std::vector<std::vector<std::shared_ptr<MidoriType>>>>;

	MidoriProgramTree m_program_tree;
	std::string m_file_name;
	const std::vector<std::string>& m_source_lines;
	std::optional<std::string> m_module_name;
	std::unordered_set<std::string> m_export_symbols;

	BytecodeBuilder m_builder;
	std::vector<BytecodeModule::ExportedSymbol> m_tracked_exports;
	std::vector<BytecodeModule::ImportedSymbol> m_tracked_imports;

	std::unordered_map<std::string, int> m_global_variables;
	std::unordered_map<std::string, int> m_local_variables;
	std::unordered_map<std::string, GenericFunctionInfo> m_generic_functions;
	std::unordered_map<FunctionSignature, int, FunctionSignatureHash> m_specialized_functions;
	TypeEnvironment m_param_type_map;
	TypeEnvironment m_generic_type_substitution;
	TypeclassMethodMap m_class_methods;
	TypeclassInstanceMap m_class_instances;
	TypeclassInstanceTypeMap m_class_instance_type_args;
	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>> m_method_resolution_map;
	std::unordered_map<std::string, size_t> m_ffi_indices;

	MidoriExecutable m_executable;
	std::stack<LoopContext> m_loop_contexts;
	std::string m_errors;
	int m_local_count = 0;

public:

	CodeGenerator(MidoriProgramTree&& program_tree, std::string_view file_name, const std::vector<std::string>& source_lines, std::string module_name, std::unordered_set<std::string> export_symbols, const TypeclassMethodMap& imported_class_methods = {}, const TypeclassInstanceMap& imported_class_instances = {}, const TypeclassInstanceTypeMap& imported_class_instance_type_args = {}, const std::unordered_map<std::string, GenericFunctionInfo>& imported_generic_functions = {});

	MidoriResult::CodeGeneratorResult GenerateModuleBytecode() &;
	MidoriResult::CodeGeneratorResult GenerateModuleBytecode() &&;

private:

	void AddError(std::string&& error);

	void PopByte(int line);

	void EmitTextConstant(std::string_view data, int line);

	void EmitByte(OpCode byte, int line);

	void EmitTwoBytes(int byte1, int byte2, int line);

	void EmitThreeBytes(int byte1, int byte2, int byte3, int line);

	void EmitNumericConstant(MidoriInteger val, int line, bool is_integer);

	void EmitFloatConstant(MidoriFloat value, int line);

	void EmitIntegerConstant(MidoriInteger value, int line);

	void EmitByteConstant(MidoriByte value, int line);

	void EmitWordConstant(MidoriWord value, int line);

	void EmitVariable(int variable_index, OpCode op, int line);

	void EmitCall(int arity, int line);

	void EmitCallProc(int proc_index, int arity, int line);

	bool MatchInstanceTypeArg(const std::shared_ptr<MidoriType>& pattern, const std::shared_ptr<MidoriType>& concrete, TypeEnvironment& substitutions, std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash>& visited) const;

	bool EmitIterableNextCall(const std::shared_ptr<MidoriType>& iter_type, const std::shared_ptr<MidoriType>& item_type, int line);

	int GetImportPlaceholder(const std::string& module_name, const std::string& symbol_name, int line);

	int EmitJump(OpCode op, int line);

	void PatchJump(int offset, int line);

	void EmitLoop(int loop_start, int line);

	void EmitEquatableEquals(const std::shared_ptr<MidoriType>& operand_type, int line);

	void EmitOrderableCompare(const std::shared_ptr<MidoriType>& operand_type, int line);

	void EmitPopCount(int count, int line);

	void EmitInstanceMethodDefinitions();

	int CountPatternBindings(const MidoriPattern& pattern) const;

	void EmitPatternCheck(const MidoriPattern& pattern, std::vector<int>& failure_jumps, int extra_pops);

	void EmitPatternBind(const MidoriPattern& pattern);

	void EmitPatternLiteralConstant(const MidoriPattern::Literal& literal);

	void EmitPatternLiteralEquals(const MidoriPattern::Literal& literal, int line);

	void BeginLoop(int loop_start);

	void EndLoop(int line);

	void Visit(const std::unique_ptr<MidoriStatement>& statement);

	void Visit(const std::unique_ptr<MidoriExpression>& expression);

	void Visit(const std::shared_ptr<MidoriExpression>& expression);

	void DispatchStatement(MidoriStatement& statement);

	void DispatchExpression(MidoriExpression& expression);

	void operator()(MidoriStatement::ExpressionStatement& simple);

	void operator()(MidoriStatement::VariableDefinition& def);

	void operator()(MidoriStatement::TupleDefinition& def_tuple);

	void operator()(MidoriStatement::FunctionDefinition& defun);

	void operator()(MidoriStatement::Continue& continue_stmt);

	void operator()(MidoriStatement::ForeignDefinition& foreign);

	void operator()(MidoriStatement::Struct& struct_stmt);

	void operator()(MidoriStatement::Union& union_stmt);

	void operator()(MidoriStatement::Class& typeclass_stmt);

	void operator()(MidoriStatement::Instance& instance_stmt);

	void operator()(MidoriStatement::TypeAlias& type_alias);

	void operator()(MidoriExpression::As& as);

	void operator()(MidoriExpression::Binary& binary);

	void operator()(MidoriExpression::Group& group);

	void operator()(MidoriExpression::Tuple& tuple);

	void operator()(MidoriExpression::UnaryPrefix& unary);

	void operator()(MidoriExpression::UnarySuffix& unary);

	void operator()(MidoriExpression::Call& call);

	void operator()(MidoriExpression::MemberAccess& get);

	void operator()(MidoriExpression::MemberAssignment& set);

	void operator()(MidoriExpression::NameAccess& variable);

	void operator()(MidoriExpression::Assignment& bind);

	void operator()(MidoriExpression::AppendAssign& append_assign);

	void operator()(MidoriExpression::ExtendAssign& extend_assign);

	void operator()(MidoriExpression::PrependAssign& prepend_assign);

	void operator()(MidoriExpression::CompoundAssign& compound_assign);

	void operator()(MidoriExpression::TextLiteral& text);

	void operator()(MidoriExpression::BoolLiteral& bool_expr);

	void operator()(MidoriExpression::FloatLiteral& float_literal);

	void operator()(MidoriExpression::IntegerLiteral& integer);

	void operator()(MidoriExpression::ByteLiteral& byte_literal);

	void operator()(MidoriExpression::WordLiteral& word_literal);

	void operator()(MidoriExpression::UnitLiteral& unit);

	void operator()(MidoriExpression::Function& function);

	void operator()(MidoriExpression::Construct& construct);

	void operator()(MidoriExpression::Array& array);

	void operator()(MidoriExpression::IndexAccess& array_get);

	void operator()(MidoriExpression::IndexAssignment& array_set);

	void operator()(MidoriExpression::RangeBinary& range_binary);

	void operator()(MidoriExpression::RangeTernary& range_ternary);

	void operator()(MidoriExpression::IfElse& if_else);

	void operator()(MidoriExpression::Block& block);

	void operator()(MidoriExpression::Match& match);

	void operator()(MidoriExpression::Case& case_expr);

	void operator()(MidoriExpression::Default& default_expr);

	void operator()(MidoriExpression::Loop& loop);

	void operator()(MidoriExpression::For& for_expr);

	void operator()(MidoriExpression::ArrayComprehension& comp);

	void operator()(MidoriExpression::Break& break_expr);

	void operator()(MidoriExpression::Return& return_expr);

	void operator()(MidoriExpression::Async& async_expr);

	void operator()(MidoriExpression::Await& await_expr);

	void EmitNumericConditionalJump(MidoriExpression::ConditionOperandType operand_type, std::unique_ptr<MidoriExpression>& true_branch, std::unique_ptr<MidoriExpression>& else_branch, int line);

	void EmitFunction(const std::vector<Token>& params, std::unique_ptr<MidoriExpression>& body, const std::string& debug_name, int line, int captured_count = 0);

	bool IsGenericType(const std::shared_ptr<MidoriType>& type);

	void DeduceGenericTypesRecursive(const std::shared_ptr<MidoriType>& param_type, const std::shared_ptr<MidoriType>& concrete_type, std::unordered_map<std::string, std::shared_ptr<MidoriType>>& map, std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash>& visited);

	int SpecializeGenericFunction(const std::string& base_name, const std::vector<std::shared_ptr<MidoriType>>& concrete_arg_types, int line);

	std::shared_ptr<MidoriType> GetConcreteTypeForExpression(const std::unique_ptr<MidoriExpression>& expr);

	std::shared_ptr<MidoriType> SubstituteGenericTypes(const std::shared_ptr<MidoriType>& type, const TypeEnvironment& generic_type_map);

	std::optional<std::string> ResolveMethodNameForCall(const std::string& callee_name, const MidoriExpression::Call& call, int line);

	bool EmitResolvedNameGetGlobal(const std::string& resolved_name, int line);

	std::optional<std::string> ResolveInstanceName(const std::string& class_name, const std::string& base_name) const;

	bool AreTypeArgsEqual(const std::vector<std::shared_ptr<MidoriType>>& left, const std::vector<std::shared_ptr<MidoriType>>& right) const;

	void AddInstanceTypeArgs(const std::string& class_name, const std::vector<std::shared_ptr<MidoriType>>& type_args);

	bool EmitGenericLengthCall(const std::string& function_name, const std::shared_ptr<MidoriType>& operand_type, int line);

	bool EmitCountableCall(const MidoriExpression::UnaryPrefix& unary, const std::shared_ptr<MidoriType>& count_type, int line);
};
