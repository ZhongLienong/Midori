#pragma once

#include "Common/Error/Error.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Compiler/Result/Result.h"

class MidoriOptimizer
{
protected:
#if MIDORI_ENABLE_OPTIMIZER_STATS
	int m_optimizations_performed = 0;
#endif
	bool m_did_change = false;

	std::unique_ptr<MidoriExpression> m_pending_replacement;

public:
	virtual ~MidoriOptimizer() = default;

	virtual MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) = 0;

	virtual std::string_view GetName() const = 0;

	void ResetPassState();

	bool DidChange() const;

#if MIDORI_ENABLE_OPTIMIZER_STATS

	int GetOptimizationsPerformed() const;
#endif

protected:
#if MIDORI_ENABLE_OPTIMIZER_STATS
	void MarkOptimization();
#else
	void MarkOptimization()
	{
		m_did_change = true;
	}
#endif

protected:
	void VisitStatement(std::unique_ptr<MidoriStatement>& statement);

	void VisitAndReplace(std::unique_ptr<MidoriExpression>& expr);

	virtual void Replace(std::unique_ptr<MidoriExpression>&& new_node, std::unique_ptr<MidoriExpression>& old_node);

	virtual void operator()(MidoriStatement::ExpressionStatement& simple);

	virtual void operator()(MidoriStatement::VariableDefinition& def);

	virtual void operator()(MidoriStatement::TupleDefinition& def_tuple);

	virtual void operator()(MidoriStatement::FunctionDefinition& defun);

	virtual void operator()(MidoriStatement::Continue& continue_stmt);

	virtual void operator()(MidoriStatement::ForeignDefinition& foreign);

	virtual void operator()(MidoriStatement::Struct& struct_stmt);

	virtual void operator()(MidoriStatement::Union& union_stmt);

	virtual void operator()(MidoriStatement::Class& typeclass_stmt);

	virtual void operator()(MidoriStatement::Instance& instance_stmt);

	virtual void operator()(MidoriStatement::TypeAlias& type_alias);

	virtual void operator()(MidoriExpression::As& as);

	virtual void operator()(MidoriExpression::Binary& binary);

	virtual void operator()(MidoriExpression::Group& group);

	virtual void operator()(MidoriExpression::Tuple& tuple);

	virtual void operator()(MidoriExpression::UnaryPrefix& unary);

	virtual void operator()(MidoriExpression::UnarySuffix& unary);

	virtual void operator()(MidoriExpression::Call& call);

	virtual void operator()(MidoriExpression::MemberAccess& get);

	virtual void operator()(MidoriExpression::MemberAssignment& set);

	virtual void operator()(MidoriExpression::NameAccess& variable);

	virtual void operator()(MidoriExpression::Assignment& bind);

	virtual void operator()(MidoriExpression::AppendAssign& append_assign);

	virtual void operator()(MidoriExpression::ExtendAssign& extend_assign);

	virtual void operator()(MidoriExpression::PrependAssign& prepend_assign);

	virtual void operator()(MidoriExpression::CompoundAssign& compound_assign);

	virtual void operator()(MidoriExpression::TextLiteral& text);

	virtual void operator()(MidoriExpression::BoolLiteral& bool_expr);

	virtual void operator()(MidoriExpression::FloatLiteral& float_literal);

	virtual void operator()(MidoriExpression::IntegerLiteral& integer);

	virtual void operator()(MidoriExpression::ByteLiteral& byte_literal);

	virtual void operator()(MidoriExpression::WordLiteral& word_literal);

	virtual void operator()(MidoriExpression::UnitLiteral& unit);

	virtual void operator()(MidoriExpression::Function& function);

	virtual void operator()(MidoriExpression::Construct& construct);

	virtual void operator()(MidoriExpression::Array& array);

	virtual void operator()(MidoriExpression::IndexAccess& array_get);

	virtual void operator()(MidoriExpression::IndexAssignment& array_set);

	virtual void operator()(MidoriExpression::ArrayComprehension& comp);

	virtual void operator()(MidoriExpression::RangeBinary& range_binary);

	virtual void operator()(MidoriExpression::RangeTernary& range_ternary);

	virtual void operator()(MidoriExpression::IfElse& if_else);

	virtual void operator()(MidoriExpression::Block& block);

	virtual void operator()(MidoriExpression::Match& match);

	virtual void operator()(MidoriExpression::Case& case_expr);

	virtual void operator()(MidoriExpression::Default& default_expr);

	virtual void operator()(MidoriExpression::Loop& loop);

	virtual void operator()(MidoriExpression::For& for_expr);

	virtual void operator()(MidoriExpression::Break& break_expr);

	virtual void operator()(MidoriExpression::Return& return_expr);

	virtual void operator()(MidoriExpression::Async& async_expr);

	virtual void operator()(MidoriExpression::Await& await_expr);
};
