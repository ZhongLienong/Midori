#pragma once

#include <algorithm>
#include <stack>
#include <unordered_set>
#include <optional>

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"
#include "Compiler/BytecodeModule/BytecodeModule.h"

class CodeGenerator
{
private:

	struct LoopContext
	{
		std::vector<int> m_break_positions;
		int m_loop_start = 0;
		int m_continue_target = 0;  // Where continue should jump to
	};

	struct GenericFunctionInfo
	{
		std::string m_name;
		std::vector<Token> m_params;
		std::unique_ptr<MidoriExpression>* m_body;
		int m_captured_count;
		std::vector<std::shared_ptr<MidoriType>> m_generic_param_types;
		std::shared_ptr<MidoriType> m_generic_return_type;
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

	MidoriExecutable::Procedures m_procedures{ BytecodeStream() };
	std::vector<MidoriText> m_procedure_names;
	MidoriProgramTree m_program_tree;
	std::string m_errors;
	std::string m_file_name;
	MidoriExecutable::StringPool m_string_pool;
	std::stack<LoopContext> m_loop_contexts;
	std::unordered_map<std::string, int> m_global_variables;
	std::unordered_map<std::string, int> m_local_variables;
	std::unordered_map<std::string, GenericFunctionInfo> m_generic_functions;
	std::unordered_map<FunctionSignature, int, FunctionSignatureHash> m_specialized_functions;
	std::unordered_map<std::string, std::shared_ptr<MidoriType>> m_param_type_map;

	MidoriExecutable m_executable;
	const std::vector<std::string>& m_source_lines;
	size_t m_current_procedure_index = 0u;
	int m_string_pool_index = 0;
	int m_local_count = 0;
	OpCode m_last_opcode = OpCode::HALT;

	// Module-specific fields for per-module compilation
	std::optional<std::string> m_module_name;
	std::unordered_set<std::string> m_export_symbols;
	std::vector<BytecodeModule::ExportedSymbol> m_tracked_exports;
	std::vector<BytecodeModule::ImportedSymbol> m_tracked_imports;

public:

	CodeGenerator(MidoriProgramTree&& program_tree, std::string_view file_name, const std::vector<std::string>& source_lines, std::string module_name, std::unordered_set<std::string> export_symbols);

	MidoriResult::CodeGeneratorResult GenerateModuleBytecode();

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

	void EmitVariable(int variable_index, OpCode op, int line);

	int EmitJump(OpCode op, int line);

	void PatchJump(int offset, int line);

	void EmitLoop(int loop_start, int line);

	void BeginLoop(int loop_start);

	void EndLoop(int line);

	void operator()(MidoriStatement::Simple& simple);

	void operator()(MidoriStatement::Define& def);

	void operator()(MidoriStatement::DefineTuple& def_tuple);

	void operator()(MidoriStatement::DefineFunction& defun);

	void operator()(MidoriStatement::Continue& continue_stmt);

	void operator()(MidoriStatement::Foreign& foreign);

	void operator()(MidoriStatement::Struct& struct_stmt);

	void operator()(MidoriStatement::Union& union_stmt);

	void operator()(MidoriExpression::As& as);

	void operator()(MidoriExpression::Binary& binary);

	void operator()(MidoriExpression::Group& group);

	void operator()(MidoriExpression::Tuple& tuple);

	void operator()(MidoriExpression::UnaryPrefix& unary);

	void operator()(MidoriExpression::UnarySuffix& unary);

	void operator()(MidoriExpression::Call& call);

	void operator()(MidoriExpression::Get& get);

	void operator()(MidoriExpression::Set& set);

	void operator()(MidoriExpression::BoundedName& variable);

	void operator()(MidoriExpression::Bind& bind);

	void operator()(MidoriExpression::TextLiteral& text);

	void operator()(MidoriExpression::BoolLiteral& bool_expr);

	void operator()(MidoriExpression::FloatLiteral& float_literal);

	void operator()(MidoriExpression::IntegerLiteral& integer);

	void operator()(MidoriExpression::UnitLiteral& unit);

	void operator()(MidoriExpression::Function& function);

	void operator()(MidoriExpression::Construct& construct);

	void operator()(MidoriExpression::Array& array);

	void operator()(MidoriExpression::ArrayGet& array_get);

	void operator()(MidoriExpression::ArraySet& array_set);

	void operator()(MidoriExpression::RangeBinary& range_binary);

	void operator()(MidoriExpression::RangeTernary& range_ternary);

	void operator()(MidoriExpression::IfElse& if_else);

	void operator()(MidoriExpression::Block& block);

	void operator()(MidoriExpression::Match& match);

	void operator()(MidoriExpression::Case& case_expr);

	void operator()(MidoriExpression::Default& default_expr);

	void operator()(MidoriExpression::Loop& loop);

	void operator()(MidoriExpression::For& for_expr);

	void operator()(MidoriExpression::Break& break_expr);

	void operator()(MidoriExpression::Return& return_expr);

	void EmitNumericConditionalJump(MidoriExpression::ConditionOperandType operand_type, std::unique_ptr<MidoriExpression>& true_branch, std::unique_ptr<MidoriExpression>& else_branch, int line);

	void EmitFunction(const std::vector<Token>& params, std::unique_ptr<MidoriExpression>& body, const std::string& debug_name, int line, int captured_count = 0);

	bool IsGenericType(const std::shared_ptr<MidoriType>& type);

	int SpecializeGenericFunction(const std::string& base_name, const std::vector<std::shared_ptr<MidoriType>>& concrete_arg_types, int line);

	std::shared_ptr<MidoriType> GetConcreteTypeForExpression(const std::unique_ptr<MidoriExpression>& expr);
};
