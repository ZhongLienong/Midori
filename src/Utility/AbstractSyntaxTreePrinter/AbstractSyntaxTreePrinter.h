#ifdef DEBUG
#pragma once

#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"

struct PrintAbstractSyntaxTree
{
	void PrintWithIndentation(int depth, std::string_view text) const;

	void PrintVariableSemantic(int depth, const MidoriExpression::NameContext::Tag& tag) const;

	void operator()(const MidoriStatement::Simple& simple, int depth = 0) const;

	void operator()(const MidoriStatement::Define& def, int depth = 0) const;

	void operator()(const MidoriStatement::DefineFunction& defun, int depth = 0) const;

	void operator()(const MidoriStatement::Continue&, int depth = 0) const;

	void operator()(const MidoriStatement::Foreign& foreign, int depth = 0) const;

	void operator()(const MidoriStatement::Struct& struct_stmt, int depth = 0) const;

	void operator()(const MidoriStatement::Union& union_stmt, int depth = 0) const;

	void operator()(const MidoriStatement::Namespace& namespace_stmt, int depth = 0) const;

	void operator()(const MidoriExpression::As& as, int depth = 0) const;

	void operator()(const MidoriExpression::Binary& binary, int depth = 0) const;

	void operator()(const MidoriExpression::Group& group, int depth = 0) const;

	void operator()(const MidoriExpression::UnaryPrefix& unary, int depth = 0) const;

	void operator()(const MidoriExpression::UnarySuffix& unary, int depth = 0) const;

	void operator()(const MidoriExpression::Call& call, int depth = 0) const;

	void operator()(const MidoriExpression::Get& get, int depth = 0) const;

	void operator()(const MidoriExpression::Set& set, int depth = 0) const;

	void operator()(const MidoriExpression::BoundedName& variable, int depth = 0) const;

	void operator()(const MidoriExpression::Bind& bind, int depth = 0) const;

	void operator()(const MidoriExpression::TextLiteral& text, int depth = 0) const;

	void operator()(const MidoriExpression::BoolLiteral& bool_val, int depth = 0) const;

	void operator()(const MidoriExpression::FloatLiteral& float_literal, int depth = 0) const;

	void operator()(const MidoriExpression::IntegerLiteral& integer, int depth = 0) const;

	void operator()(const MidoriExpression::UnitLiteral&, int depth = 0) const;

	void operator()(const MidoriExpression::Function& closure, int depth = 0) const;

	void operator()(const MidoriExpression::Construct& construct, int depth = 0) const;

	void operator()(const MidoriExpression::Array& array, int depth = 0) const;

	void operator()(const MidoriExpression::ArrayGet& array_get, int depth = 0) const;

	void operator()(const MidoriExpression::ArraySet& array_set, int depth = 0) const;

	void operator()(const MidoriExpression::IfElse& if_else, int depth = 0) const;

	void operator()(const MidoriExpression::Block& block, int depth = 0) const;

	void operator()(const MidoriExpression::Match& match, int depth = 0) const;

	void operator()(const MidoriExpression::Case& case_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Default& default_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Loop& loop, int depth = 0) const;

	void operator()(const MidoriExpression::Return& return_expr, int depth = 0) const;

	void operator()(const MidoriExpression::Break& break_expr, int depth = 0) const;
};
#endif