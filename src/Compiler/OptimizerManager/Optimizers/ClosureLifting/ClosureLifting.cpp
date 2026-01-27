#include "ClosureLifting.h"
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
		int m_patch_count{ 0 };

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
		// Helper to rewrite name access
		void RewriteAccess(Token& name, MidoriExpression::NameContext::Tag& context)
		{
			if (name.m_lexeme == m_self_name)
			{
				name.m_lexeme = m_new_global_name;
				context = MidoriExpression::NameContext::Global{};
				m_patch_count++;
			}
		}

		void operator()(MidoriExpression::NameAccess& access) override
		{
			RewriteAccess(access.m_name, access.m_name_ctx);
		}

		void operator()(MidoriExpression::Assignment& assign) override
		{
			RewriteAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}

		void operator()(MidoriExpression::AppendAssign& assign) override
		{
			RewriteAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}

		void operator()(MidoriExpression::PrependAssign& assign) override
		{
			RewriteAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}

		void operator()(MidoriExpression::CompoundAssign& assign) override
		{
			RewriteAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}
	};

	class CaptureAnalyzer : public MidoriOptimizer
	{
	public:
		std::string_view m_self_name;
		bool m_is_safe{ true };

		CaptureAnalyzer(std::string_view self_name)
			: m_self_name(self_name)
		{
		}

		int Optimize(MidoriProgramTree&) override 
		{ 
			return 0; 
		}
		
		std::string_view GetName() const override
		{
			return "CaptureAnalyzer"; 
		}

		void Analyze(std::unique_ptr<MidoriExpression>& expr)
		{
			VisitAndReplace(expr);
		}

	protected:
		void CheckAccess(const Token& name, const MidoriExpression::NameContext::Tag& context)
		{
			if (!m_is_safe)
			{
				return;
			}

			if (std::holds_alternative<MidoriExpression::NameContext::Cell>(context))
			{
				// Any cell access that isn't self-ref is an external capture -> UNSAFE.
				if (name.m_lexeme != m_self_name)
				{
					m_is_safe = false;
				}
			}
			// Local accesses are always safe (refer to current stack frame)
		}

		void operator()(MidoriExpression::NameAccess& access) override
		{
			CheckAccess(access.m_name, access.m_name_ctx);
		}

		void operator()(MidoriExpression::Assignment& assign) override
		{
			CheckAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}

		void operator()(MidoriExpression::AppendAssign& assign) override
		{
			CheckAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}

		void operator()(MidoriExpression::PrependAssign& assign) override
		{
			CheckAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}

		void operator()(MidoriExpression::CompoundAssign& assign) override
		{
			CheckAccess(assign.m_name, assign.m_name_ctx);
			VisitAndReplace(assign.m_value);
		}
	};
}

int ClosureLifting::Optimize(MidoriProgramTree& program_tree)
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
			VisitStatement(stmt);
		}
	);

	// Prepend new globals to handle forward references
	if (!m_new_globals.empty())
	{
		program_tree.insert(program_tree.begin(), std::make_move_iterator(m_new_globals.begin()), std::make_move_iterator(m_new_globals.end()));
	}

#if MIDORI_ENABLE_OPTIMIZER_STATS
	return GetOptimizationsPerformed();
#else
	return m_lifted_count;
#endif
}

std::string_view ClosureLifting::GetName() const
{
	return "ClosureLifting";
}

void ClosureLifting::operator()(MidoriExpression::Function& function)
{
	// Only visit the body to analyze nested functions within lambdas.
	// Lambda lifting is disabled because it causes forward reference issues
	// when lifted lambda bodies reference global functions defined later.
	VisitAndReplace(function.m_body);
}

void ClosureLifting::operator()(MidoriExpression::Block& block)
{
	for (std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
	{
		if (stmt->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			MidoriStatement::FunctionDefinition& defun = stmt->GetStatement<MidoriStatement::FunctionDefinition>();

			VisitAndReplace(defun.m_body);

			bool is_local_nested = defun.m_local_index.has_value();

			if (is_local_nested)
			{
				// Analyze captured variables
				// We need to verify if the function *actually* uses any environmental captures.
				// Local accesses are safe. Cell accesses are unsafe unless they are self-references.
				CaptureAnalyzer analyzer(defun.m_name.m_lexeme);
				analyzer.Analyze(defun.m_body);

				if (analyzer.m_is_safe)
				{
					static int s_global_lift_id = 0;
					std::string lifted_name = std::format("__lifted_{}_{}", defun.m_name.m_lexeme, s_global_lift_id++);
					Token lifted_token(std::move(lifted_name), Token::Name::IDENTIFIER_LITERAL, defun.m_name.m_line, defun.m_name.m_file_name);

					SelfReferencePatcher patcher(defun.m_name.m_lexeme, lifted_token.m_lexeme);
					patcher.Patch(defun.m_body);

					m_lifted_count++;
#if MIDORI_ENABLE_OPTIMIZER_STATS
					MarkOptimization();
#endif

					// Reset captured count to 0. 
					// This is crucial: it tells CodeGenerator to emit MAKE_FUNCTION instead of MAKE_CLOSURE,
					// and to NOT emit BIND_CAPTURES. Since we verified no captures are used, this is safe.
					defun.m_captured_count = 0;

					// Save parameters for the wrapper function BEFORE moving them
					std::vector<Token> wrapper_params = defun.m_params;
					std::vector<Token> wrapper_generic = defun.m_generic_params;
					std::vector<std::shared_ptr<MidoriType>> wrapper_param_types = defun.m_param_types;
					std::shared_ptr<MidoriType> wrapper_return_type = defun.m_return_type;

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

					// Create arguments for the wrapper call
					std::vector<std::unique_ptr<MidoriExpression>> call_args;
					call_args.reserve(wrapper_params.size());
					for (size_t i = 0uz; i < wrapper_params.size(); i += 1uz) 
					{
						call_args.emplace_back(std::make_unique<MidoriExpression>(MidoriExpression::NameAccess(wrapper_params[i], MidoriExpression::NameContext::Local{static_cast<int>(i)})));
					}

					MidoriExpression::NameContext::Tag context = MidoriExpression::NameContext::Global{};
					Token callee_token = lifted_token;
					std::unique_ptr<MidoriExpression> callee_expr = std::make_unique<MidoriExpression>(MidoriExpression::NameAccess(callee_token, std::move(context)));

					Token paren_token(std::string("("), Token::Name::LEFT_PAREN, defun.m_name.m_line, defun.m_name.m_file_name);
					std::unique_ptr<MidoriExpression> call_expr = std::make_unique<MidoriExpression>
						(
							MidoriExpression::Call
							(
								paren_token,
								std::move(callee_expr),
								std::move(call_args)
							)
						);

					std::unique_ptr<MidoriStatement> wrapper_def = std::make_unique<MidoriStatement>
					(
						MidoriStatement::FunctionDefinition
						(
							defun.m_name,
							std::move(wrapper_generic),
							std::move(wrapper_params),
							std::move(wrapper_param_types),
							std::move(wrapper_return_type),
							std::move(call_expr),
							std::move(defun.m_local_index)
						)
					);

					stmt = std::move(wrapper_def);
				}
			}
		}
		else
		{
			VisitStatement(stmt);
		}
	}

	if (block.m_final_expr.has_value())
	{
		VisitAndReplace(block.m_final_expr.value());
	}
}
