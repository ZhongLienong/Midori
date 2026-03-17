#include "DiagnosticPasses.h"

#include "Compiler/Analysis/SharedAnalysis.h"

#include <format>

namespace
{
	std::string JoinKinds(const std::vector<std::string_view>& kinds)
	{
		if (kinds.empty())
		{
			return {};
		}

		if (kinds.size() == 1u)
		{
			return std::string(kinds.front());
		}

		if (kinds.size() == 2u)
		{
			return std::format("{} and {}", kinds[0], kinds[1]);
		}

		std::string result;
		for (size_t i = 0u; i < kinds.size(); i += 1u)
		{
			if (i > 0u)
			{
				result.append(i + 1u == kinds.size() ? ", and " : ", ");
			}
			result.append(kinds[i]);
		}
		return result;
	}
}

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
		const BindingInfo& binding = function.m_bindings[static_cast<size_t>(binding_id)];
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
		for (auto it = function.m_active_locals.begin(); it != function.m_active_locals.end();)
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
	for (size_t index = 0u; index < params.size(); index += 1u)
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
		const auto it = m_functions.back().m_active_locals.find(local_index);
		if (it != m_functions.back().m_active_locals.end())
		{
			m_functions.back().m_bindings[static_cast<size_t>(it->second)].m_is_read = true;
		}
		return;
	}

	if (!std::holds_alternative<MidoriExpression::NameContext::Cell>(name_ctx))
	{
		return;
	}

	const int local_index = std::get<MidoriExpression::NameContext::Cell>(name_ctx).m_index;
	for (size_t index = m_functions.size(); index > 1u; index -= 1u)
	{
		FunctionContext& outer_function = m_functions[index - 2u];
		const auto it = outer_function.m_active_locals.find(local_index);
		if (it != outer_function.m_active_locals.end())
		{
			outer_function.m_bindings[static_cast<size_t>(it->second)].m_is_read = true;
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
	for (size_t index = 0u; index < def_tuple.m_names.size(); index += 1u)
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

std::string_view UnreachableCodeDiagnostic::GetName() const
{
	return "UnreachableCodeDiagnostic";
}

void UnreachableCodeDiagnostic::operator()(MidoriExpression::Block& block)
{
	bool reachable = true;

	for (std::unique_ptr<MidoriStatement>& statement : block.m_stmts)
	{
		if (!reachable)
		{
			if (const Token* token = MidoriAnalysis::GetPrimaryToken(*statement))
			{
				EmitWarning
				(
					CompilerWarningCode::UnreachableCode,
					*token,
					"This code is unreachable because control flow already exits earlier in the block."
				);
			}
			continue;
		}

		VisitStatement(statement);
		reachable = !MidoriAnalysis::IsTerminatingStatement(*statement);
	}

	if (!block.m_final_expr.has_value())
	{
		return;
	}

	if (!reachable)
	{
		if (const Token* token = MidoriAnalysis::GetPrimaryToken(*block.m_final_expr.value()))
		{
			EmitWarning
			(
				CompilerWarningCode::UnreachableCode,
				*token,
				"This code is unreachable because control flow already exits earlier in the block."
			);
		}
		return;
	}

	VisitExpression(block.m_final_expr.value());
}

std::string_view ShadowingPolicyDiagnostic::GetName() const
{
	return "ShadowingPolicyDiagnostic";
}

void ShadowingPolicyDiagnostic::Reset()
{
	m_scopes.clear();
	PushScope();
}

void ShadowingPolicyDiagnostic::Finish()
{
	while (!m_scopes.empty())
	{
		PopScope();
	}
}

void ShadowingPolicyDiagnostic::PushScope()
{
	m_scopes.emplace_back();
}

void ShadowingPolicyDiagnostic::PopScope()
{
	m_scopes.pop_back();
}

void ShadowingPolicyDiagnostic::DefineVariable(const Token& token)
{
	std::vector<std::string_view> kinds = CollectShadowKinds(token.m_lexeme);
	if (!kinds.empty())
	{
		EmitShadowWarning(token, kinds);
	}

	m_scopes.back().m_variables.insert(token.m_lexeme);
}

void ShadowingPolicyDiagnostic::DefineStruct(const Token& token)
{
	std::vector<std::string_view> kinds = CollectShadowKinds(token.m_lexeme);
	if (!kinds.empty())
	{
		EmitShadowWarning(token, kinds);
	}

	m_scopes.back().m_structs.insert(token.m_lexeme);
	m_scopes.back().m_types.insert(token.m_lexeme);
}

void ShadowingPolicyDiagnostic::DefineUnionConstructor(const Token& token)
{
	std::vector<std::string_view> kinds = CollectShadowKinds(token.m_lexeme);
	if (!kinds.empty())
	{
		EmitShadowWarning(token, kinds);
	}

	m_scopes.back().m_union_constructors.insert(token.m_lexeme);
}

void ShadowingPolicyDiagnostic::DefineType(const Token& token)
{
	std::vector<std::string_view> kinds = CollectShadowKinds(token.m_lexeme);
	if (!kinds.empty())
	{
		EmitShadowWarning(token, kinds);
	}

	m_scopes.back().m_types.insert(token.m_lexeme);
}

void ShadowingPolicyDiagnostic::DefineGenericParams(const std::vector<Token>& generic_params)
{
	for (const Token& token : generic_params)
	{
		DefineType(token);
	}
}

std::vector<std::string_view> ShadowingPolicyDiagnostic::CollectShadowKinds(const std::string& mangled_name) const
{
	bool shadows_struct = false;
	bool shadows_variable = false;
	bool shadows_union = false;
	bool shadows_type = false;

	for (size_t index = m_scopes.size(); index > 1u; index -= 1u)
	{
		const Scope& scope = m_scopes[index - 2u];
		shadows_struct = shadows_struct || scope.m_structs.contains(mangled_name);
		shadows_variable = shadows_variable || scope.m_variables.contains(mangled_name);
		shadows_union = shadows_union || scope.m_union_constructors.contains(mangled_name);
		shadows_type = shadows_type || scope.m_types.contains(mangled_name);
	}

	std::vector<std::string_view> kinds;
	if (shadows_struct)
	{
		kinds.push_back("a struct");
	}
	if (shadows_variable)
	{
		kinds.push_back("a variable");
	}
	if (shadows_union)
	{
		kinds.push_back("a union constructor");
	}
	if (shadows_type)
	{
		kinds.push_back("a type");
	}
	return kinds;
}

void ShadowingPolicyDiagnostic::EmitShadowWarning(const Token& token, const std::vector<std::string_view>& kinds)
{
	const std::string display_name = MidoriAnalysis::DemangleDisplayName(token.m_lexeme);
	EmitWarning
	(
		CompilerWarningCode::NameShadowing,
		token,
		std::format("Name '{}' shadows {} from an outer scope.", display_name, JoinKinds(kinds))
	);
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::VariableDefinition& def)
{
	DefineVariable(def.m_name);
	VisitExpression(def.m_value);
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::TupleDefinition& def_tuple)
{
	for (const Token& token : def_tuple.m_names)
	{
		DefineVariable(token);
	}

	VisitExpression(def_tuple.m_value);
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::FunctionDefinition& defun)
{
	DefineVariable(defun.m_name);

	if (!defun.m_generic_params.empty())
	{
		PushScope();
		DefineGenericParams(defun.m_generic_params);
	}

	PushScope();
	for (const Token& token : defun.m_params)
	{
		DefineVariable(token);
	}
	VisitExpression(defun.m_body);
	PopScope();

	if (!defun.m_generic_params.empty())
	{
		PopScope();
	}
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::ForeignDefinition& foreign)
{
	DefineVariable(foreign.m_function_name);
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::Struct& struct_stmt)
{
	DefineStruct(struct_stmt.m_name);

	if (!struct_stmt.m_generic_params.empty())
	{
		PushScope();
		DefineGenericParams(struct_stmt.m_generic_params);
		PopScope();
	}
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::Union& union_stmt)
{
	DefineType(union_stmt.m_name);

	if (!union_stmt.m_generic_params.empty())
	{
		PushScope();
		DefineGenericParams(union_stmt.m_generic_params);
	}

	for (const Token& constructor_name : union_stmt.m_constructor_names)
	{
		DefineUnionConstructor(constructor_name);
	}

	if (!union_stmt.m_generic_params.empty())
	{
		PopScope();
	}
}

void ShadowingPolicyDiagnostic::operator()(MidoriStatement::TypeAlias& type_alias)
{
	DefineType(type_alias.m_name);

	if (!type_alias.m_generic_params.empty())
	{
		PushScope();
		DefineGenericParams(type_alias.m_generic_params);
		PopScope();
	}
}

void ShadowingPolicyDiagnostic::operator()(MidoriPattern::Binding& binding)
{
	DefineVariable(binding.m_name);
}

void ShadowingPolicyDiagnostic::operator()(MidoriExpression::Function& function)
{
	if (!function.m_generic_params.empty())
	{
		PushScope();
		DefineGenericParams(function.m_generic_params);
	}

	PushScope();
	for (const Token& token : function.m_params)
	{
		DefineVariable(token);
	}
	VisitExpression(function.m_body);
	PopScope();

	if (!function.m_generic_params.empty())
	{
		PopScope();
	}
}

void ShadowingPolicyDiagnostic::operator()(MidoriExpression::Block& block)
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

void ShadowingPolicyDiagnostic::operator()(MidoriExpression::Case& case_expr)
{
	PushScope();
	VisitPattern(case_expr.m_pattern);
	VisitExpression(case_expr.m_expr);
	PopScope();
}

std::string_view CaptureEscapeDiagnostic::GetName() const
{
	return "CaptureEscapeDiagnostic";
}

void CaptureEscapeDiagnostic::Reset()
{
	m_functions.clear();
	PushFunctionContext();
}

void CaptureEscapeDiagnostic::Finish()
{
	while (!m_functions.empty())
	{
		PopFunctionContext();
	}
}

void CaptureEscapeDiagnostic::PushFunctionContext()
{
	m_functions.emplace_back();
	m_functions.back().m_scopes.emplace_back();
}

void CaptureEscapeDiagnostic::PopFunctionContext()
{
	m_functions.pop_back();
}

void CaptureEscapeDiagnostic::PushScope()
{
	m_functions.back().m_scopes.emplace_back();
}

void CaptureEscapeDiagnostic::PopScope()
{
	m_functions.back().m_scopes.pop_back();
}

void CaptureEscapeDiagnostic::RegisterClosureBinding(std::optional<int> local_index, const Token& token, bool has_captures)
{
	if (!has_captures || !local_index.has_value())
	{
		return;
	}

	m_functions.back().m_scopes.back().m_closures.emplace
	(
		local_index.value(),
		ClosureBinding
		{
			token,
			MidoriAnalysis::DemangleDisplayName(token.m_lexeme)
		}
	);
}

const CaptureEscapeDiagnostic::ClosureBinding* CaptureEscapeDiagnostic::ResolveClosure(const MidoriExpression::NameContext::Tag& name_ctx) const
{
	if (std::holds_alternative<MidoriExpression::NameContext::Local>(name_ctx))
	{
		const int local_index = std::get<MidoriExpression::NameContext::Local>(name_ctx).m_index;
		for (size_t scope_index = m_functions.back().m_scopes.size(); scope_index > 0u; scope_index -= 1u)
		{
			const Scope& scope = m_functions.back().m_scopes[scope_index - 1u];
			const auto it = scope.m_closures.find(local_index);
			if (it != scope.m_closures.end())
			{
				return &it->second;
			}
		}
		return nullptr;
	}

	if (!std::holds_alternative<MidoriExpression::NameContext::Cell>(name_ctx))
	{
		return nullptr;
	}

	const int local_index = std::get<MidoriExpression::NameContext::Cell>(name_ctx).m_index;
	for (size_t function_index = m_functions.size(); function_index > 1u; function_index -= 1u)
	{
		const FunctionContext& function = m_functions[function_index - 2u];
		for (size_t scope_index = function.m_scopes.size(); scope_index > 0u; scope_index -= 1u)
		{
			const Scope& scope = function.m_scopes[scope_index - 1u];
			const auto it = scope.m_closures.find(local_index);
			if (it != scope.m_closures.end())
			{
				return &it->second;
			}
		}
	}

	return nullptr;
}

std::optional<CaptureEscapeDiagnostic::EscapingClosure> CaptureEscapeDiagnostic::TryGetEscapingClosure(const MidoriExpression& expression) const
{
	const MidoriExpression* stripped = MidoriAnalysis::StripRedundantGroups(&expression);
	if (stripped == nullptr)
	{
		return std::nullopt;
	}

	if (stripped->IsExpression<MidoriExpression::Function>())
	{
		const MidoriExpression::Function& function = stripped->GetExpression<MidoriExpression::Function>();
		if (function.m_captured_count > 0)
		{
			return EscapingClosure
			{
				function.m_function_keyword,
				"anonymous closure"
			};
		}
		return std::nullopt;
	}

	if (stripped->IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& access = stripped->GetExpression<MidoriExpression::NameAccess>();
		if (const ClosureBinding* binding = ResolveClosure(access.m_name_ctx))
		{
			return EscapingClosure
			{
				access.m_name,
				binding->m_display_name
			};
		}
		return std::nullopt;
	}

	if (stripped->IsExpression<MidoriExpression::Block>())
	{
		const MidoriExpression::Block& block = stripped->GetExpression<MidoriExpression::Block>();
		if (block.m_final_expr.has_value())
		{
			return TryGetEscapingClosure(*block.m_final_expr.value());
		}
		return std::nullopt;
	}

	return std::nullopt;
}

void CaptureEscapeDiagnostic::WarnOnEscapingClosure(const MidoriExpression& expression)
{
	const std::optional<EscapingClosure> escaping = TryGetEscapingClosure(expression);
	if (!escaping.has_value())
	{
		return;
	}

	if (escaping->m_display_name == "anonymous closure")
	{
		EmitWarning
		(
			CompilerWarningCode::CaptureEscape,
			escaping->m_token,
			"Captured anonymous closure escapes its defining scope as a return value."
		);
		return;
	}

	EmitWarning
	(
		CompilerWarningCode::CaptureEscape,
		escaping->m_token,
		std::format("Captured closure '{}' escapes its defining scope as a return value.", escaping->m_display_name)
	);
}

void CaptureEscapeDiagnostic::operator()(MidoriStatement::VariableDefinition& def)
{
	bool has_capturing_closure = false;
	const MidoriExpression* stripped = MidoriAnalysis::StripRedundantGroups(def.m_value.get());
	if (stripped != nullptr && stripped->IsExpression<MidoriExpression::Function>())
	{
		has_capturing_closure = stripped->GetExpression<MidoriExpression::Function>().m_captured_count > 0;
	}

	RegisterClosureBinding(def.m_local_index, def.m_name, has_capturing_closure);
	VisitExpression(def.m_value);
}

void CaptureEscapeDiagnostic::operator()(MidoriStatement::FunctionDefinition& defun)
{
	RegisterClosureBinding(defun.m_local_index, defun.m_name, defun.m_captured_count > 0);

	PushFunctionContext();
	VisitExpression(defun.m_body);
	WarnOnEscapingClosure(*defun.m_body);
	PopFunctionContext();
}

void CaptureEscapeDiagnostic::operator()(MidoriExpression::Function& function)
{
	PushFunctionContext();
	VisitExpression(function.m_body);
	WarnOnEscapingClosure(*function.m_body);
	PopFunctionContext();
}

void CaptureEscapeDiagnostic::operator()(MidoriExpression::Block& block)
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

void CaptureEscapeDiagnostic::operator()(MidoriExpression::Return& return_expr)
{
	VisitExpression(return_expr.m_value);
	WarnOnEscapingClosure(*return_expr.m_value);
}
