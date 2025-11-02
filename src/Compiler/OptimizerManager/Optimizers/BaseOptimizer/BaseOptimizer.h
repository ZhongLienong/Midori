#pragma once

#include "Common/Error/Error.h"
#include "Compiler/Result/Result.h"

class MidoriOptimizer
{
protected:
#ifdef DEBUG
	int m_optimizations_performed = 0;
#endif

	std::unique_ptr<MidoriExpression> m_pending_replacement;

public:
	virtual ~MidoriOptimizer() = default;

	virtual int Optimize(MidoriProgramTree& program_tree) = 0;

	virtual std::string_view GetName() const = 0;

#ifdef DEBUG

	void ResetCounter();

	int GetOptimizationsPerformed() const;
#endif

protected:
#ifdef DEBUG
	// Mark that an optimization was performed (only in debug builds)
	void MarkOptimization();
#else
	 // Mark optimization (no-op in other builds)
	void MarkOptimization() { }
#endif

protected:
	void VisitAndReplace(std::unique_ptr<MidoriExpression>& expr);

	virtual void Replace(std::unique_ptr<MidoriExpression>&& new_node, std::unique_ptr<MidoriExpression>& old_node);

	virtual void operator()(MidoriStatement::Simple& simple);

	virtual void operator()(MidoriStatement::Define& def);

	virtual void operator()(MidoriStatement::DefineTuple& def_tuple);

	virtual void operator()(MidoriStatement::DefineFunction& defun);

	virtual void operator()(MidoriStatement::Continue& continue_stmt);

	virtual void operator()(MidoriStatement::Foreign& foreign);

	virtual void operator()(MidoriStatement::Struct& struct_stmt);

	virtual void operator()(MidoriStatement::Union& union_stmt);

	virtual void operator()(MidoriStatement::Namespace& namespace_stmt);

	virtual void operator()(MidoriExpression::As& as);

	virtual void operator()(MidoriExpression::Binary& binary);

	virtual void operator()(MidoriExpression::Group& group);

	virtual void operator()(MidoriExpression::Tuple& tuple);

	virtual void operator()(MidoriExpression::UnaryPrefix& unary);

	virtual void operator()(MidoriExpression::UnarySuffix& unary);

	virtual void operator()(MidoriExpression::Call& call);

	virtual void operator()(MidoriExpression::Get& get);

	virtual void operator()(MidoriExpression::Set& set);

	virtual void operator()(MidoriExpression::BoundedName& variable);

	virtual void operator()(MidoriExpression::Bind& bind);

	virtual void operator()(MidoriExpression::TextLiteral& text);

	virtual void operator()(MidoriExpression::BoolLiteral& bool_expr);

	virtual void operator()(MidoriExpression::FloatLiteral& float_literal);

	virtual void operator()(MidoriExpression::IntegerLiteral& integer);

	virtual void operator()(MidoriExpression::UnitLiteral& unit);

	virtual void operator()(MidoriExpression::Function& function);

	virtual void operator()(MidoriExpression::Construct& construct);

	virtual void operator()(MidoriExpression::Array& array);

	virtual void operator()(MidoriExpression::ArrayGet& array_get);

	virtual void operator()(MidoriExpression::ArraySet& array_set);

	virtual void operator()(MidoriExpression::IfElse& if_else);

	virtual void operator()(MidoriExpression::Block& block);

	virtual void operator()(MidoriExpression::Match& match);

	virtual void operator()(MidoriExpression::Case& case_expr);

	virtual void operator()(MidoriExpression::Default& default_expr);

	virtual void operator()(MidoriExpression::Loop& loop);

	virtual void operator()(MidoriExpression::Break& break_expr);

	virtual void operator()(MidoriExpression::Return& return_expr);
};