#include "LambdaLifting.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Compiler/Token/Token.h"
#include <format>

namespace
{
	class SelfReferencePatcher : public MidoriOptimizer
	{
	public:
		std::string_view m_self_name;
		std::string m_new_global_name;

		SelfReferencePatcher(std::string_view self_name, std::string new_global_name)
			: m_self_name(self_name), 
			m_new_global_name(std::move(new_global_name)) 
		{
		}

		int Optimize(MidoriProgramTree&) override 
		{ 
			return 0; 
		}

		std::string_view GetName() const override 
		{ 
			return "SelfReferencePatcher"; 
		}

		void Patch(std::unique_ptr<MidoriExpression>& expr)
		{
			VisitAndReplace(expr);
		}

	protected:
		void operator()(MidoriExpression::NameAccess& access) override
		{
			if (std::holds_alternative<MidoriExpression::NameContext::Local>(access.m_name_ctx))
			{
				if (access.m_name.m_lexeme == m_self_name)
				{
					access.m_name.m_lexeme = m_new_global_name;
					access.m_name_ctx = MidoriExpression::NameContext::Global{};
				}
			}
		}
	};
}

int LambdaLifting::Optimize(MidoriProgramTree& program_tree)
{
#if MIDORI_ENABLE_OPTIMIZER_STATS
	ResetCounter();
#endif
	m_new_globals.clear();
	m_lifted_count = 0;

	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **stmt);
		}
	);

	if (!m_new_globals.empty())
	{
		program_tree.reserve(program_tree.size() + m_new_globals.size());
		for (std::unique_ptr<MidoriStatement>& global_stmt : m_new_globals)
		{
			program_tree.emplace_back(std::move(global_stmt));
		}
	}

#if MIDORI_ENABLE_OPTIMIZER_STATS
	return GetOptimizationsPerformed();
#else
	return m_lifted_count;
#endif
}

std::string_view LambdaLifting::GetName() const
{
	return "LambdaLifting";
}

void LambdaLifting::operator()(MidoriExpression::Function& function)
{
	VisitAndReplace(function.m_body);

	if (function.m_captured_count == 0)
	{
		m_lifted_count++;
#if MIDORI_ENABLE_OPTIMIZER_STATS
		MarkOptimization();
#endif

		static int s_global_lift_id = 0;
		std::string name = std::format("__lifted_lambda_{}", s_global_lift_id++);

		Token name_token(std::move(name), Token::Name::IDENTIFIER_LITERAL, function.m_function_keyword.m_line);

		std::unique_ptr<MidoriExpression> new_func_expr = std::make_unique<MidoriExpression>
		(
			MidoriExpression::Function
			(
				function.m_function_keyword,
				std::move(function.m_generic_params),
				std::move(function.m_params),
				std::move(function.m_param_types),
				std::move(function.m_return_type),
				std::move(function.m_body),
				0
			)
		);
		new_func_expr->GetType() = function.m_type_data;

		std::unique_ptr<MidoriStatement> global_def = std::make_unique<MidoriStatement>
		(
			MidoriStatement::VariableDefinition
			(
				name_token,
				std::move(new_func_expr),
				std::nullopt,
				std::nullopt
			)
		);

		m_new_globals.push_back(std::move(global_def));

		MidoriExpression::NameContext::Tag context = MidoriExpression::NameContext::Global{};
		std::unique_ptr<MidoriExpression> ref_expr = std::make_unique<MidoriExpression>
		(
			MidoriExpression::NameAccess(name_token, std::move(context))
		);
		ref_expr->GetType() = function.m_type_data;

		m_pending_replacement = std::move(ref_expr);
	}
}

void LambdaLifting::operator()(MidoriExpression::Block& block)
{
	for (std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
	{
		if (stmt->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			MidoriStatement::FunctionDefinition& defun = stmt->GetStatement<MidoriStatement::FunctionDefinition>();

			VisitAndReplace(defun.m_body);

			bool is_local_nested = defun.m_local_index.has_value();
			if (is_local_nested && defun.m_captured_count == 0)
			{
				m_lifted_count++;
#if MIDORI_ENABLE_OPTIMIZER_STATS
				MarkOptimization();
#endif
				static int s_global_lift_id = 0;
				std::string lifted_name = std::format("__lifted_{}_{}", defun.m_name.m_lexeme, s_global_lift_id++);
				Token lifted_token(std::move(lifted_name), Token::Name::IDENTIFIER_LITERAL, defun.m_name.m_line);

				SelfReferencePatcher patcher(defun.m_name.m_lexeme, lifted_token.m_lexeme);
				patcher.Patch(defun.m_body);

				std::unique_ptr<MidoriStatement> global_def = std::make_unique<MidoriStatement>
				(
					MidoriStatement::FunctionDefinition
					(
						lifted_token,
						std::move(defun.m_generic_params),
						std::move(defun.m_params),
						std::move(defun.m_param_types),
						std::move(defun.m_return_type),
						std::move(defun.m_body),
						std::nullopt,
						0,
						std::move(defun.m_constraints)
					)
				);
				m_new_globals.emplace_back(std::move(global_def));

				MidoriExpression::NameContext::Tag context = MidoriExpression::NameContext::Global{};
				std::unique_ptr<MidoriExpression> ref_expr = std::make_unique<MidoriExpression>
				(
					MidoriExpression::NameAccess(lifted_token, std::move(context))
				);

				std::unique_ptr<MidoriStatement> local_var_def = std::make_unique<MidoriStatement>
				(
					MidoriStatement::VariableDefinition
					(
						defun.m_name,
						std::move(ref_expr),
						std::nullopt,
						std::move(defun.m_local_index)
					)
				);

				stmt = std::move(local_var_def);
			}
		}
		else
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **stmt);
		}
	}

	if (block.m_final_expr.has_value())
	{
		VisitAndReplace(block.m_final_expr.value());
	}
}
