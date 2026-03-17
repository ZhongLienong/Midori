#include "UnusedLocalDiagnostic.h"

#include "Compiler/Analysis/SharedAnalysis.h"

#include <format>

std::string_view UnusedLocalDiagnostic::GetName() const
{
	return "UnusedLocalDiagnostic";
}

void UnusedLocalDiagnostic::Reset()
{
	m_functions.clear();
	PushFunctionContext();
}

void UnusedLocalDiagnostic::Finish()
{
	while (!m_functions.empty())
	{
		PopFunctionContext();
	}
}

void UnusedLocalDiagnostic::PushFunctionContext()
{
	m_functions.emplace_back();
	m_functions.back().m_scopes.emplace_back();
}

void UnusedLocalDiagnostic::PopFunctionContext()
{
	while (!m_functions.back().m_scopes.empty())
	{
		PopScope();
	}
	m_functions.pop_back();
}

void UnusedLocalDiagnostic::PushScope()
{
	m_functions.back().m_scopes.emplace_back();
}

void UnusedLocalDiagnostic::PopScope()
{
	FunctionContext& function = m_functions.back();
	Scope scope = std::move(function.m_scopes.back());
	function.m_scopes.pop_back();

	for (int binding_id : scope.m_binding_ids)
	{
		const BindingInfo& binding = function.m_bindings[static_cast<std::size_t>(binding_id)];
		if (!binding.m_is_suppressed && !binding.m_is_read)
		{
			EmitWarning
			(
				CompilerWarningCode::UnusedLocal,
				binding.m_token,
				std::format("Binding '{}' is never read.", binding.m_display_name)
			);
		}
	}

	for (int binding_id : scope.m_binding_ids)
	{
		for
		(
			std::unordered_map<int, int>::iterator it = function.m_active_locals.begin();
			it != function.m_active_locals.end();
		)
		{
			if (it->second == binding_id)
			{
				it = function.m_active_locals.erase(it);
			}
			else
			{
				++it;
			}
		}
	}
}

void UnusedLocalDiagnostic::RegisterBinding(const Token& token, std::optional<int> local_index, bool suppressed)
{
	if (!local_index.has_value())
	{
		return;
	}

	FunctionContext& function = m_functions.back();
	const int binding_id = static_cast<int>(function.m_bindings.size());
	function.m_bindings.emplace_back
	(
		BindingInfo
		{
			token,
			MidoriAnalysis::DemangleDisplayName(token.m_lexeme),
			false,
			suppressed
		}
	);

	function.m_scopes.back().m_binding_ids.push_back(binding_id);
	function.m_active_locals[local_index.value()] = binding_id;
}

void UnusedLocalDiagnostic::RegisterParameterBindings(const std::vector<Token>& params)
{
	for (std::size_t index = 0u; index < params.size(); index += 1u)
	{
		const Token& token = params[index];
		RegisterBinding(token, static_cast<int>(index), MidoriAnalysis::IsIgnoredBindingName(MidoriAnalysis::DemangleDisplayName(token.m_lexeme)));
	}
}

void UnusedLocalDiagnostic::MarkRead(const MidoriExpression::NameContext::Tag& name_ctx)
{
	if (std::holds_alternative<MidoriExpression::NameContext::Local>(name_ctx))
	{
		const int local_index = std::get<MidoriExpression::NameContext::Local>(name_ctx).m_index;
		const std::unordered_map<int, int>::const_iterator it = m_functions.back().m_active_locals.find(local_index);
		if (it != m_functions.back().m_active_locals.end())
		{
			m_functions.back().m_bindings[static_cast<std::size_t>(it->second)].m_is_read = true;
		}
		return;
	}

	if (!std::holds_alternative<MidoriExpression::NameContext::Cell>(name_ctx))
	{
		return;
	}

	const int local_index = std::get<MidoriExpression::NameContext::Cell>(name_ctx).m_index;
	for (std::size_t index = m_functions.size(); index > 1u; index -= 1u)
	{
		FunctionContext& outer_function = m_functions[index - 2u];
		const std::unordered_map<int, int>::const_iterator it = outer_function.m_active_locals.find(local_index);
		if (it != outer_function.m_active_locals.end())
		{
			outer_function.m_bindings[static_cast<std::size_t>(it->second)].m_is_read = true;
			return;
		}
	}
}

void UnusedLocalDiagnostic::operator()(MidoriStatement::VariableDefinition& def)
{
	RegisterBinding(def.m_name, def.m_local_index, def.m_is_elided || MidoriAnalysis::IsIgnoredBindingName(MidoriAnalysis::DemangleDisplayName(def.m_name.m_lexeme)));
	VisitExpression(def.m_value);
}

void UnusedLocalDiagnostic::operator()(MidoriStatement::TupleDefinition& def_tuple)
{
	for (std::size_t index = 0u; index < def_tuple.m_names.size(); index += 1u)
	{
		const Token& token = def_tuple.m_names[index];
		const std::optional<int>& local_index = def_tuple.m_local_indices[index];
		RegisterBinding(token, local_index, MidoriAnalysis::IsIgnoredBindingName(MidoriAnalysis::DemangleDisplayName(token.m_lexeme)));
	}

	VisitExpression(def_tuple.m_value);
}

void UnusedLocalDiagnostic::operator()(MidoriStatement::FunctionDefinition& defun)
{
	RegisterBinding(defun.m_name, defun.m_local_index, MidoriAnalysis::IsIgnoredBindingName(MidoriAnalysis::DemangleDisplayName(defun.m_name.m_lexeme)));

	PushFunctionContext();
	RegisterParameterBindings(defun.m_params);
	VisitExpression(defun.m_body);
	PopFunctionContext();
}

void UnusedLocalDiagnostic::operator()(MidoriStatement::ForeignDefinition& foreign)
{
	RegisterBinding(foreign.m_function_name, foreign.m_local_index, MidoriAnalysis::IsIgnoredBindingName(MidoriAnalysis::DemangleDisplayName(foreign.m_function_name.m_lexeme)));
}

void UnusedLocalDiagnostic::operator()(MidoriPattern::Binding& binding)
{
	RegisterBinding(binding.m_name, binding.m_local_index, MidoriAnalysis::IsIgnoredBindingName(MidoriAnalysis::DemangleDisplayName(binding.m_name.m_lexeme)));
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::NameAccess& access)
{
	MarkRead(access.m_name_ctx);
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::AppendAssign& append_assign)
{
	MarkRead(append_assign.m_name_ctx);
	VisitExpression(append_assign.m_value);
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::ExtendAssign& extend_assign)
{
	MarkRead(extend_assign.m_name_ctx);
	VisitExpression(extend_assign.m_value);
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::PrependAssign& prepend_assign)
{
	MarkRead(prepend_assign.m_name_ctx);
	VisitExpression(prepend_assign.m_value);
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::CompoundAssign& compound_assign)
{
	MarkRead(compound_assign.m_name_ctx);
	VisitExpression(compound_assign.m_value);
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::Function& function)
{
	PushFunctionContext();
	RegisterParameterBindings(function.m_params);
	VisitExpression(function.m_body);
	PopFunctionContext();
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::Block& block)
{
	PushScope();

	for (std::unique_ptr<MidoriStatement>& statement : block.m_stmts)
	{
		VisitStatement(statement);
	}

	if (block.m_final_expr.has_value())
	{
		VisitExpression(block.m_final_expr.value());
	}

	PopScope();
}

void UnusedLocalDiagnostic::operator()(MidoriExpression::Case& case_expr)
{
	PushScope();
	VisitPattern(case_expr.m_pattern);
	VisitExpression(case_expr.m_expr);
	PopScope();
}
