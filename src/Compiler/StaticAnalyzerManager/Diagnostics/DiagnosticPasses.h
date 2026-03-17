#pragma once

#include "Compiler/StaticAnalyzerManager/StaticAnalyzerManager.h"

#include <string>
#include <unordered_map>
#include <unordered_set>

class UnusedLocalDiagnostic final : public AstDiagnosticPass
{
public:
	std::string_view GetName() const override;

protected:
	void Reset() override;

	void Finish() override;

	void operator()(MidoriStatement::VariableDefinition& def) override;
	void operator()(MidoriStatement::TupleDefinition& def_tuple) override;
	void operator()(MidoriStatement::FunctionDefinition& defun) override;
	void operator()(MidoriStatement::ForeignDefinition& foreign) override;
	void operator()(MidoriPattern::Binding& binding) override;
	void operator()(MidoriExpression::NameAccess& access) override;
	void operator()(MidoriExpression::AppendAssign& append_assign) override;
	void operator()(MidoriExpression::ExtendAssign& extend_assign) override;
	void operator()(MidoriExpression::PrependAssign& prepend_assign) override;
	void operator()(MidoriExpression::CompoundAssign& compound_assign) override;
	void operator()(MidoriExpression::Function& function) override;
	void operator()(MidoriExpression::Block& block) override;
	void operator()(MidoriExpression::Case& case_expr) override;

private:
	struct BindingInfo
	{
		Token m_token;
		std::string m_display_name;
		bool m_is_read = false;
		bool m_is_suppressed = false;
	};

	struct Scope
	{
		std::vector<int> m_binding_ids;
	};

	struct FunctionContext
	{
		std::vector<Scope> m_scopes;
		std::unordered_map<int, int> m_active_locals;
		std::vector<BindingInfo> m_bindings;
	};

	void PushFunctionContext();
	void PopFunctionContext();
	void PushScope();
	void PopScope();
	void RegisterBinding(const Token& token, std::optional<int> local_index, bool suppressed);
	void RegisterParameterBindings(const std::vector<Token>& params);
	void MarkRead(const MidoriExpression::NameContext::Tag& name_ctx);

	std::vector<FunctionContext> m_functions;
};

class UnreachableCodeDiagnostic final : public AstDiagnosticPass
{
public:
	std::string_view GetName() const override;

protected:
	void operator()(MidoriExpression::Block& block) override;
};

class ShadowingPolicyDiagnostic final : public AstDiagnosticPass
{
public:
	std::string_view GetName() const override;

protected:
	void Reset() override;

	void Finish() override;

	void operator()(MidoriStatement::VariableDefinition& def) override;
	void operator()(MidoriStatement::TupleDefinition& def_tuple) override;
	void operator()(MidoriStatement::FunctionDefinition& defun) override;
	void operator()(MidoriStatement::ForeignDefinition& foreign) override;
	void operator()(MidoriStatement::Struct& struct_stmt) override;
	void operator()(MidoriStatement::Union& union_stmt) override;
	void operator()(MidoriStatement::TypeAlias& type_alias) override;
	void operator()(MidoriPattern::Binding& binding) override;
	void operator()(MidoriExpression::Function& function) override;
	void operator()(MidoriExpression::Block& block) override;
	void operator()(MidoriExpression::Case& case_expr) override;

private:
	struct Scope
	{
		std::unordered_set<std::string> m_variables;
		std::unordered_set<std::string> m_structs;
		std::unordered_set<std::string> m_union_constructors;
		std::unordered_set<std::string> m_types;
	};

	void PushScope();
	void PopScope();
	void DefineVariable(const Token& token);
	void DefineStruct(const Token& token);
	void DefineUnionConstructor(const Token& token);
	void DefineType(const Token& token);
	void DefineGenericParams(const std::vector<Token>& generic_params);
	std::vector<std::string_view> CollectShadowKinds(const std::string& mangled_name) const;
	void EmitShadowWarning(const Token& token, const std::vector<std::string_view>& kinds);

	std::vector<Scope> m_scopes;
};

class CaptureEscapeDiagnostic final : public AstDiagnosticPass
{
public:
	std::string_view GetName() const override;

protected:
	void Reset() override;

	void Finish() override;

	void operator()(MidoriStatement::VariableDefinition& def) override;
	void operator()(MidoriStatement::FunctionDefinition& defun) override;
	void operator()(MidoriExpression::Function& function) override;
	void operator()(MidoriExpression::Block& block) override;
	void operator()(MidoriExpression::Return& return_expr) override;

private:
	struct ClosureBinding
	{
		Token m_token;
		std::string m_display_name;
	};

	struct Scope
	{
		std::unordered_map<int, ClosureBinding> m_closures;
	};

	struct FunctionContext
	{
		std::vector<Scope> m_scopes;
	};

	struct EscapingClosure
	{
		Token m_token;
		std::string m_display_name;
	};

	void PushFunctionContext();
	void PopFunctionContext();
	void PushScope();
	void PopScope();
	void RegisterClosureBinding(std::optional<int> local_index, const Token& token, bool has_captures);
	const ClosureBinding* ResolveClosure(const MidoriExpression::NameContext::Tag& name_ctx) const;
	std::optional<EscapingClosure> TryGetEscapingClosure(const MidoriExpression& expression) const;
	void WarnOnEscapingClosure(const MidoriExpression& expression);

	std::vector<FunctionContext> m_functions;
};
