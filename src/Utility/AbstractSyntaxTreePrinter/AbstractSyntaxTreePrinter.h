#ifdef DEBUG
#pragma once

#include "Compiler/AbstractSyntaxTree/AbstractSyntaxTree.h"

struct PrintAbstractSyntaxTree
{
	void PrintWithIndentation(int depth, std::string_view text) const;

	void PrintVariableSemantic(int depth, const MidoriExpression::NameContext::Tag& tag) const;

	void operator()(const Block& block, int depth = 0) const;

	void operator()(const Simple& simple, int depth = 0) const;

	void operator()(const Define& def, int depth = 0) const;

	void operator()(const If& if_stmt, int depth = 0) const;

	void operator()(const While& while_stmt, int depth = 0) const;

	void operator()(const For& for_stmt, int depth = 0) const;

	void operator()(const Break&, int depth = 0) const;

	void operator()(const Continue&, int depth = 0) const;

	void operator()(const Return& return_stmt, int depth = 0) const;

	void operator()(const Foreign& foreign, int depth = 0) const;

	void operator()(const Struct& struct_stmt, int depth = 0) const;

	void operator()(const Union& union_stmt, int depth = 0) const;

	void operator()(const Switch& switch_stmt, int depth = 0) const;

	void operator()(const Namespace& namespace_stmt, int depth = 0) const;

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

	void operator()(const MidoriExpression::Ternary& ternary, int depth = 0) const;
};
#endif