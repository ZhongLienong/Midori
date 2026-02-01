#if MIDORI_ENABLE_AST_DUMP
#pragma once

#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"
#include "Common/BuildConfig/BuildConfig.h"

struct PrintAbstractSyntaxTree
{
	void PrintWithIndentation(int depth, std::string_view text) const;

	void PrintVariableSemantic(int depth, const MidoriExpression::NameContext::Tag& tag) const;

	void operator()(const MidoriStatement::ExpressionStatement& simple, int depth = 0) const;

	void operator()(const MidoriStatement::VariableDefinition& def, int depth = 0) const;

	void operator()(const MidoriStatement::TupleDefinition& def_tuple, int depth = 0) const;

	void operator()(const MidoriStatement::FunctionDefinition& defun, int depth = 0) const;

	void operator()(const MidoriStatement::Continue&, int depth = 0) const;

	void operator()(const MidoriStatement::ForeignDefinition& foreign, int depth = 0) const;

	void operator()(const MidoriStatement::Struct& struct_stmt, int depth = 0) const;

	void operator()(const MidoriStatement::Union& union_stmt, int depth = 0) const;

	void operator()(const MidoriExpression::As& as, int depth = 0) const;

	void operator()(const MidoriExpression::Binary& binary, int depth = 0) const;

	void operator()(const MidoriExpression::Group& group, int depth = 0) const;

	void operator()(const MidoriExpression::Tuple& tuple, int depth = 0) const;

	void operator()(const MidoriExpression::UnaryPrefix& unary, int depth = 0) const;

	void operator()(const MidoriExpression::UnarySuffix& unary, int depth = 0) const;

	void operator()(const MidoriExpression::Call& call, int depth = 0) const;

	void operator()(const MidoriExpression::MemberAccess& get, int depth = 0) const;

	void operator()(const MidoriExpression::MemberAssignment& set, int depth = 0) const;

	void operator()(const MidoriExpression::NameAccess& variable, int depth = 0) const;

	void operator()(const MidoriExpression::Assignment& bind, int depth = 0) const;

	void operator()(const MidoriExpression::TextLiteral& text, int depth = 0) const;

	void operator()(const MidoriExpression::BoolLiteral& bool_val, int depth = 0) const;

	void operator()(const MidoriExpression::FloatLiteral& float_literal, int depth = 0) const;

	void operator()(const MidoriExpression::IntegerLiteral& integer, int depth = 0) const;

	void operator()(const MidoriExpression::ByteLiteral& byte_literal, int depth = 0) const;

	void operator()(const MidoriExpression::WordLiteral& word_literal, int depth = 0) const;

	void operator()(const MidoriExpression::UnitLiteral&, int depth = 0) const;

	void operator()(const MidoriExpression::Function& closure, int depth = 0) const;

	void operator()(const MidoriExpression::Construct& construct, int depth = 0) const;

	void operator()(const MidoriExpression::Array& array, int depth = 0) const;

	void operator()(const MidoriExpression::IndexAccess& array_get, int depth = 0) const;

	void operator()(const MidoriExpression::IndexAssignment& array_set, int depth = 0) const;

	void operator()(const MidoriExpression::RangeBinary& range_binary, int depth = 0) const;

	void operator()(const MidoriExpression::RangeTernary& range_ternary, int depth = 0) const;

	void operator()(const MidoriExpression::IfElse& if_else, int depth = 0) const;

	void operator()(const MidoriExpression::Block& block, int depth = 0) const;

	void operator()(const MidoriExpression::Match& match, int depth = 0) const;

	void operator()(const MidoriExpression::Case& case_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Default& default_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Loop& loop, int depth = 0) const;

	void operator()(const MidoriExpression::For& for_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Return& return_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Break& break_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Async& async_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Await& await_expr, int depth = 0) const;

	void operator()(const MidoriPattern::Binding& binding, int depth = 0) const;

	void operator()(const MidoriPattern::Literal& literal, int depth = 0) const;

	void operator()(const MidoriPattern::Tuple& tuple, int depth = 0) const;

	void operator()(const MidoriPattern::Array& array, int depth = 0) const;

	void operator()(const MidoriPattern::Constructor& constructor, int depth = 0) const;

private:

	void Visit(const std::unique_ptr<MidoriStatement>& statement, int depth) const;

	void Visit(const std::unique_ptr<MidoriExpression>& expression, int depth) const;

	void Visit(const std::unique_ptr<MidoriPattern>& pattern, int depth) const;
};
#endif
