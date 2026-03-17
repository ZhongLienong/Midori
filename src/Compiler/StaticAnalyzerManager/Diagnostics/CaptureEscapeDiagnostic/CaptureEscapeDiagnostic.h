#pragma once

#include "Compiler/StaticAnalyzerManager/Diagnostics/BaseDiagnosticPass/BaseDiagnosticPass.h"

#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

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
