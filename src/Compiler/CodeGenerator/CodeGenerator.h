#pragma once

#include <algorithm>
#include <stack>

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"

class CodeGenerator
{
private:
	struct LoopContext
	{
		std::vector<int> m_break_positions;
		int m_loop_start = 0;
	};

	MidoriExecutable::Procedures m_procedures{ BytecodeStream() };
#ifdef DEBUG
	std::vector<MidoriText> m_procedure_names{ MidoriText("runtime startup") };
#endif
	std::string m_errors;
	MidoriExecutable::StringPool m_string_pool;
	std::stack<LoopContext> m_loop_contexts;
	std::unordered_map<std::string, int> m_global_variables;
	std::unordered_map<std::string, int> m_local_variables;

	MidoriExecutable m_executable;
	size_t m_current_procedure_index = 0;
	int m_string_pool_index = 0;
	int m_local_count = 0;
	OpCode m_last_opcode = OpCode::HALT;

public:

	MidoriResult::CodeGeneratorResult GenerateCode(MidoriProgramTree&& program_tree);

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

	void operator()(MidoriStatement::Continue& continue_stmt);

	void operator()(MidoriStatement::Foreign& foreign);

	void operator()(MidoriStatement::Struct& struct_stmt);

	void operator()(MidoriStatement::Union& union_stmt);

	void operator()(MidoriStatement::Namespace& namespace_stmt);

	void operator()(MidoriExpression::As& as);

	void operator()(MidoriExpression::Binary& binary);

	void operator()(MidoriExpression::Group& group);

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

	void operator()(MidoriExpression::IfElse& if_else);

	void operator()(MidoriExpression::Block& block);

	void operator()(MidoriExpression::Match& match);

	void operator()(MidoriExpression::Case& case_expr);

	void operator()(MidoriExpression::Default& default_expr);

	void operator()(MidoriExpression::Loop& loop);

	void operator()(MidoriExpression::Break& break_expr);

	void operator()(MidoriExpression::Return& return_expr);

	void EmitNumericConditionalJump(MidoriExpression::ConditionOperandType operand_type, std::unique_ptr<MidoriExpression>& true_branch, std::unique_ptr<MidoriExpression>& else_branch, int line);
};
