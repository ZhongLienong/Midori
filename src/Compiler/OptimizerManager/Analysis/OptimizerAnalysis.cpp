#include "OptimizerAnalysis.h"

#include <optional>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace
{
	using OptimizerAnalysis::BlockLocalAccessSummary;
	using OptimizerAnalysis::ConstantValue;
	using OptimizerAnalysis::LiteralForm;
	using OptimizerAnalysis::StatementLocalAccessSummary;
	using OptimizerAnalysis::UnitConstant;

	std::optional<MidoriInteger> SafeParseInteger(const std::string& lexeme)
	{
		try
		{
			return std::stoll(lexeme);
		}
		catch (const std::invalid_argument&)
		{
			return std::nullopt;
		}
		catch (const std::out_of_range&)
		{
			return std::nullopt;
		}
	}

	std::optional<MidoriFloat> SafeParseFloat(const std::string& lexeme)
	{
		try
		{
			return std::stod(lexeme);
		}
		catch (const std::invalid_argument&)
		{
			return std::nullopt;
		}
		catch (const std::out_of_range&)
		{
			return std::nullopt;
		}
	}

	std::optional<MidoriByte> SafeParseByte(const std::string& lexeme)
	{
		try
		{
			return static_cast<MidoriByte>(std::stoul(lexeme, nullptr, 0));
		}
		catch (const std::invalid_argument&)
		{
			return std::nullopt;
		}
		catch (const std::out_of_range&)
		{
			return std::nullopt;
		}
	}

	std::optional<MidoriWord> SafeParseWord(const std::string& lexeme)
	{
		try
		{
			return std::stoull(lexeme, nullptr, 0);
		}
		catch (const std::invalid_argument&)
		{
			return std::nullopt;
		}
		catch (const std::out_of_range&)
		{
			return std::nullopt;
		}
	}

	std::optional<bool> TryGetBoolValue(const ConstantValue& value)
	{
		if (!value.Is<MidoriBool>())
		{
			return std::nullopt;
		}
		return value.Get<MidoriBool>();
	}

	std::optional<int> TryGetLocalIndex(const MidoriExpression::NameContext::Tag& name_ctx)
	{
		if (!std::holds_alternative<MidoriExpression::NameContext::Local>(name_ctx))
		{
			return std::nullopt;
		}

		return std::get<MidoriExpression::NameContext::Local>(name_ctx).m_index;
	}

	void RecordRead(StatementLocalAccessSummary& summary, int local_index)
	{
		summary.m_locals[local_index].m_reads += 1;
	}

	void RecordAssignment(StatementLocalAccessSummary& summary, int local_index)
	{
		summary.m_locals[local_index].m_assignments += 1;
	}

	void RecordDefinition(StatementLocalAccessSummary& summary, const std::optional<int>& local_index)
	{
		if (local_index.has_value())
		{
			RecordAssignment(summary, local_index.value());
		}
	}

	std::optional<ConstantValue> EvalConstant(const MidoriExpression& expr);

	std::optional<ConstantValue> EvalBinary(const MidoriExpression::Binary& binary)
	{
		switch (binary.m_op.m_token_name)
		{
		case Token::Name::DOUBLE_AMPERSAND:
		{
			std::optional<bool> left_truth = OptimizerAnalysis::TryEvalTruthValue(*binary.m_left);
			if (left_truth.has_value() && !left_truth.value())
			{
				return ConstantValue{ false };
			}

			std::optional<bool> right_truth = OptimizerAnalysis::TryEvalTruthValue(*binary.m_right);
			if (left_truth.has_value() && right_truth.has_value())
			{
				return ConstantValue{ left_truth.value() && right_truth.value() };
			}
			return std::nullopt;
		}
		case Token::Name::DOUBLE_BAR:
		{
			std::optional<bool> left_truth = OptimizerAnalysis::TryEvalTruthValue(*binary.m_left);
			if (left_truth.has_value() && left_truth.value())
			{
				return ConstantValue{ true };
			}

			std::optional<bool> right_truth = OptimizerAnalysis::TryEvalTruthValue(*binary.m_right);
			if (left_truth.has_value() && right_truth.has_value())
			{
				return ConstantValue{ left_truth.value() || right_truth.value() };
			}
			return std::nullopt;
		}
		default:
			break;
		}

		std::optional<ConstantValue> left_value = EvalConstant(*binary.m_left);
		std::optional<ConstantValue> right_value = EvalConstant(*binary.m_right);
		if (!left_value.has_value() || !right_value.has_value())
		{
			return std::nullopt;
		}

		const Token::Name op = binary.m_op.m_token_name;

		if (left_value->Is<MidoriInteger>() && right_value->Is<MidoriInteger>())
		{
			const MidoriInteger left = left_value->Get<MidoriInteger>();
			const MidoriInteger right = right_value->Get<MidoriInteger>();

			switch (op)
			{
			case Token::Name::SINGLE_PLUS:
				return ConstantValue{ left + right };
			case Token::Name::SINGLE_MINUS:
				return ConstantValue{ left - right };
			case Token::Name::STAR:
				return ConstantValue{ left * right };
			case Token::Name::SLASH:
				if (right == 0ll)
				{
					return std::nullopt;
				}
				return ConstantValue{ left / right };
			case Token::Name::PERCENT:
				if (right == 0ll)
				{
					return std::nullopt;
				}
				return ConstantValue{ left % right };
			case Token::Name::LEFT_SHIFT:
				return ConstantValue{ left << right };
			case Token::Name::RIGHT_SHIFT:
				return ConstantValue{ left >> right };
			case Token::Name::SINGLE_AMPERSAND:
				return ConstantValue{ left & right };
			case Token::Name::SINGLE_BAR:
				return ConstantValue{ left | right };
			case Token::Name::CARET:
				return ConstantValue{ left ^ right };
			case Token::Name::LEFT_ANGLE:
				return ConstantValue{ left < right };
			case Token::Name::RIGHT_ANGLE:
				return ConstantValue{ left > right };
			case Token::Name::LESS_EQUAL:
				return ConstantValue{ left <= right };
			case Token::Name::GREATER_EQUAL:
				return ConstantValue{ left >= right };
			case Token::Name::DOUBLE_EQUAL:
				return ConstantValue{ left == right };
			case Token::Name::BANG_EQUAL:
				return ConstantValue{ left != right };
			default:
				return std::nullopt;
			}
		}

		if (left_value->Is<MidoriFloat>() && right_value->Is<MidoriFloat>())
		{
			const MidoriFloat left = left_value->Get<MidoriFloat>();
			const MidoriFloat right = right_value->Get<MidoriFloat>();

			switch (op)
			{
			case Token::Name::SINGLE_PLUS:
				return ConstantValue{ left + right };
			case Token::Name::SINGLE_MINUS:
				return ConstantValue{ left - right };
			case Token::Name::STAR:
				return ConstantValue{ left * right };
			case Token::Name::SLASH:
				if (right == 0.0)
				{
					return std::nullopt;
				}
				return ConstantValue{ left / right };
			default:
				return std::nullopt;
			}
		}

		if (left_value->Is<MidoriByte>() && right_value->Is<MidoriByte>())
		{
			const MidoriByte left = left_value->Get<MidoriByte>();
			const MidoriByte right = right_value->Get<MidoriByte>();

			switch (op)
			{
			case Token::Name::SINGLE_PLUS:
				return ConstantValue{ static_cast<MidoriByte>(left + right) };
			case Token::Name::SINGLE_MINUS:
				return ConstantValue{ static_cast<MidoriByte>(left - right) };
			case Token::Name::STAR:
				return ConstantValue{ static_cast<MidoriByte>(left * right) };
			case Token::Name::SLASH:
				if (right == 0u)
				{
					return std::nullopt;
				}
				return ConstantValue{ static_cast<MidoriByte>(left / right) };
			case Token::Name::PERCENT:
				if (right == 0u)
				{
					return std::nullopt;
				}
				return ConstantValue{ static_cast<MidoriByte>(left % right) };
			case Token::Name::LEFT_SHIFT:
				return ConstantValue{ static_cast<MidoriByte>(left << right) };
			case Token::Name::RIGHT_SHIFT:
				return ConstantValue{ static_cast<MidoriByte>(left >> right) };
			case Token::Name::SINGLE_AMPERSAND:
				return ConstantValue{ static_cast<MidoriByte>(left & right) };
			case Token::Name::SINGLE_BAR:
				return ConstantValue{ static_cast<MidoriByte>(left | right) };
			case Token::Name::CARET:
				return ConstantValue{ static_cast<MidoriByte>(left ^ right) };
			case Token::Name::LEFT_ANGLE:
				return ConstantValue{ left < right };
			case Token::Name::RIGHT_ANGLE:
				return ConstantValue{ left > right };
			case Token::Name::LESS_EQUAL:
				return ConstantValue{ left <= right };
			case Token::Name::GREATER_EQUAL:
				return ConstantValue{ left >= right };
			case Token::Name::DOUBLE_EQUAL:
				return ConstantValue{ left == right };
			case Token::Name::BANG_EQUAL:
				return ConstantValue{ left != right };
			default:
				return std::nullopt;
			}
		}

		if (left_value->Is<MidoriWord>() && right_value->Is<MidoriWord>())
		{
			const MidoriWord left = left_value->Get<MidoriWord>();
			const MidoriWord right = right_value->Get<MidoriWord>();

			switch (op)
			{
			case Token::Name::SINGLE_PLUS:
				return ConstantValue{ left + right };
			case Token::Name::SINGLE_MINUS:
				return ConstantValue{ left - right };
			case Token::Name::STAR:
				return ConstantValue{ left * right };
			case Token::Name::SLASH:
				if (right == 0ull)
				{
					return std::nullopt;
				}
				return ConstantValue{ left / right };
			case Token::Name::PERCENT:
				if (right == 0ull)
				{
					return std::nullopt;
				}
				return ConstantValue{ left % right };
			case Token::Name::LEFT_SHIFT:
				return ConstantValue{ left << right };
			case Token::Name::RIGHT_SHIFT:
				return ConstantValue{ left >> right };
			case Token::Name::SINGLE_AMPERSAND:
				return ConstantValue{ left & right };
			case Token::Name::SINGLE_BAR:
				return ConstantValue{ left | right };
			case Token::Name::CARET:
				return ConstantValue{ left ^ right };
			case Token::Name::LEFT_ANGLE:
				return ConstantValue{ left < right };
			case Token::Name::RIGHT_ANGLE:
				return ConstantValue{ left > right };
			case Token::Name::LESS_EQUAL:
				return ConstantValue{ left <= right };
			case Token::Name::GREATER_EQUAL:
				return ConstantValue{ left >= right };
			case Token::Name::DOUBLE_EQUAL:
				return ConstantValue{ left == right };
			case Token::Name::BANG_EQUAL:
				return ConstantValue{ left != right };
			default:
				return std::nullopt;
			}
		}

		if (left_value->Is<MidoriBool>() && right_value->Is<MidoriBool>())
		{
			const MidoriBool left = left_value->Get<MidoriBool>();
			const MidoriBool right = right_value->Get<MidoriBool>();

			switch (op)
			{
			case Token::Name::DOUBLE_AMPERSAND:
				return ConstantValue{ left && right };
			case Token::Name::DOUBLE_BAR:
				return ConstantValue{ left || right };
			case Token::Name::DOUBLE_EQUAL:
				return ConstantValue{ left == right };
			case Token::Name::BANG_EQUAL:
				return ConstantValue{ left != right };
			default:
				return std::nullopt;
			}
		}

		if (left_value->Is<std::string>() && right_value->Is<std::string>())
		{
			if (op != Token::Name::DOUBLE_PLUS)
			{
				return std::nullopt;
			}

			return ConstantValue{ left_value->Get<std::string>() + right_value->Get<std::string>() };
		}

		return std::nullopt;
	}

	std::optional<ConstantValue> EvalUnary(const MidoriExpression::UnaryPrefix& unary)
	{
		std::optional<ConstantValue> inner_value = EvalConstant(*unary.m_expr);
		if (!inner_value.has_value())
		{
			return std::nullopt;
		}

		switch (unary.m_op.m_token_name)
		{
		case Token::Name::SINGLE_MINUS:
			if (inner_value->Is<MidoriInteger>())
			{
				return ConstantValue{ -inner_value->Get<MidoriInteger>() };
			}
			if (inner_value->Is<MidoriFloat>())
			{
				return ConstantValue{ -inner_value->Get<MidoriFloat>() };
			}
			return std::nullopt;
		case Token::Name::BANG:
			if (!inner_value->Is<MidoriBool>())
			{
				return std::nullopt;
			}
			return ConstantValue{ !inner_value->Get<MidoriBool>() };
		case Token::Name::TILDE:
			if (inner_value->Is<MidoriInteger>())
			{
				return ConstantValue{ ~inner_value->Get<MidoriInteger>() };
			}
			if (inner_value->Is<MidoriByte>())
			{
				return ConstantValue{ static_cast<MidoriByte>(~inner_value->Get<MidoriByte>()) };
			}
			if (inner_value->Is<MidoriWord>())
			{
				return ConstantValue{ static_cast<MidoriWord>(~inner_value->Get<MidoriWord>()) };
			}
			return std::nullopt;
		default:
			return std::nullopt;
		}
	}

	std::optional<ConstantValue> EvalConstant(const MidoriExpression& expr)
	{
		const MidoriExpression* stripped_expr = OptimizerAnalysis::StripRedundantGroups(&expr);
		if (stripped_expr == nullptr)
		{
			return std::nullopt;
		}

		return std::visit
		(
			[](const auto& node) -> std::optional<ConstantValue>
			{
				using T = std::decay_t<decltype(node)>;

				if constexpr (std::is_same_v<T, MidoriExpression::BoolLiteral>)
				{
					return ConstantValue{ node.m_token.m_token_name == Token::Name::TRUE };
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IntegerLiteral>)
				{
					std::optional<MidoriInteger> value = SafeParseInteger(node.m_token.m_lexeme);
					return value.has_value() ? std::optional<ConstantValue>{ ConstantValue{ value.value() } } : std::nullopt;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::FloatLiteral>)
				{
					std::optional<MidoriFloat> value = SafeParseFloat(node.m_token.m_lexeme);
					return value.has_value() ? std::optional<ConstantValue>{ ConstantValue{ value.value() } } : std::nullopt;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::ByteLiteral>)
				{
					std::optional<MidoriByte> value = SafeParseByte(node.m_token.m_lexeme);
					return value.has_value() ? std::optional<ConstantValue>{ ConstantValue{ value.value() } } : std::nullopt;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::WordLiteral>)
				{
					std::optional<MidoriWord> value = SafeParseWord(node.m_token.m_lexeme);
					return value.has_value() ? std::optional<ConstantValue>{ ConstantValue{ value.value() } } : std::nullopt;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::TextLiteral>)
				{
					return ConstantValue{ node.m_token.m_lexeme };
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnitLiteral>)
				{
					return ConstantValue{ UnitConstant{} };
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnaryPrefix>)
				{
					return EvalUnary(node);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Binary>)
				{
					return EvalBinary(node);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IfElse>)
				{
					std::optional<bool> condition_value = OptimizerAnalysis::TryEvalTruthValue(*node.m_condition);
					if (!condition_value.has_value())
					{
						return std::nullopt;
					}

					return condition_value.value()
						? EvalConstant(*node.m_true_branch)
						: EvalConstant(*node.m_else_branch);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::As>)
				{
					if (node.m_uses_convertable)
					{
						return std::nullopt;
					}

					std::shared_ptr<MidoriType> from_type = node.m_from_type.lock();
					if (!from_type || *from_type != *node.m_to_type)
					{
						return std::nullopt;
					}

					return EvalConstant(*node.m_expr);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Block>)
				{
					if (!node.m_stmts.empty() || !node.m_final_expr.has_value())
					{
						return std::nullopt;
					}

					return EvalConstant(*node.m_final_expr.value());
				}
				else
				{
					return std::nullopt;
				}
			},
			**stripped_expr
		);
	}

	bool IsPureStatement(const MidoriStatement& stmt);

	bool IsPureExpression(const MidoriExpression& expr)
	{
		const MidoriExpression* stripped_expr = OptimizerAnalysis::StripRedundantGroups(&expr);
		if (stripped_expr == nullptr)
		{
			return false;
		}

		return std::visit
		(
			[](const auto& node) -> bool
			{
				using T = std::decay_t<decltype(node)>;

				if constexpr (std::is_same_v<T, MidoriExpression::TextLiteral>
					|| std::is_same_v<T, MidoriExpression::BoolLiteral>
					|| std::is_same_v<T, MidoriExpression::FloatLiteral>
					|| std::is_same_v<T, MidoriExpression::IntegerLiteral>
					|| std::is_same_v<T, MidoriExpression::ByteLiteral>
					|| std::is_same_v<T, MidoriExpression::WordLiteral>
					|| std::is_same_v<T, MidoriExpression::UnitLiteral>
					|| std::is_same_v<T, MidoriExpression::NameAccess>)
				{
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::As>)
				{
					if (node.m_uses_convertable)
					{
						return false;
					}

					std::shared_ptr<MidoriType> from_type = node.m_from_type.lock();
					if (!from_type || *from_type != *node.m_to_type)
					{
						return false;
					}

					return IsPureExpression(*node.m_expr);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Binary>)
				{
					if (node.m_uses_equatable || node.m_uses_orderable)
					{
						return false;
					}

					if (node.m_op.m_token_name == Token::Name::SLASH || node.m_op.m_token_name == Token::Name::PERCENT)
					{
						return false;
					}

					if (node.m_op.m_token_name == Token::Name::DOUBLE_AMPERSAND)
					{
						std::optional<bool> left_truth = OptimizerAnalysis::TryEvalTruthValue(*node.m_left);
						if (left_truth.has_value() && !left_truth.value())
						{
							return IsPureExpression(*node.m_left);
						}
					}
					else if (node.m_op.m_token_name == Token::Name::DOUBLE_BAR)
					{
						std::optional<bool> left_truth = OptimizerAnalysis::TryEvalTruthValue(*node.m_left);
						if (left_truth.has_value() && left_truth.value())
						{
							return IsPureExpression(*node.m_left);
						}
					}

					return IsPureExpression(*node.m_left) && IsPureExpression(*node.m_right);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Tuple>)
				{
					for (const std::unique_ptr<MidoriExpression>& element : node.m_elements)
					{
						if (!IsPureExpression(*element))
						{
							return false;
						}
					}
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnaryPrefix>)
				{
					if (node.m_uses_countable)
					{
						return false;
					}
					return IsPureExpression(*node.m_expr);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::UnarySuffix>)
				{
					return false;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Assignment>
					|| std::is_same_v<T, MidoriExpression::AppendAssign>
					|| std::is_same_v<T, MidoriExpression::ExtendAssign>
					|| std::is_same_v<T, MidoriExpression::PrependAssign>
					|| std::is_same_v<T, MidoriExpression::CompoundAssign>
					|| std::is_same_v<T, MidoriExpression::Call>
					|| std::is_same_v<T, MidoriExpression::MemberAssignment>
					|| std::is_same_v<T, MidoriExpression::IndexAssignment>
					|| std::is_same_v<T, MidoriExpression::Loop>
					|| std::is_same_v<T, MidoriExpression::For>
					|| std::is_same_v<T, MidoriExpression::Return>
					|| std::is_same_v<T, MidoriExpression::Break>
					|| std::is_same_v<T, MidoriExpression::Async>
					|| std::is_same_v<T, MidoriExpression::Await>
					|| std::is_same_v<T, MidoriExpression::ArrayComprehension>)
				{
					return false;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Function>)
				{
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Construct>)
				{
					for (const std::unique_ptr<MidoriExpression>& arg : node.m_params)
					{
						if (!IsPureExpression(*arg))
						{
							return false;
						}
					}
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IfElse>)
				{
					std::optional<bool> condition_value = OptimizerAnalysis::TryEvalTruthValue(*node.m_condition);
					if (condition_value.has_value())
					{
						return IsPureExpression(*node.m_condition)
							&& (condition_value.value()
								? IsPureExpression(*node.m_true_branch)
								: IsPureExpression(*node.m_else_branch));
					}

					return IsPureExpression(*node.m_condition)
						&& IsPureExpression(*node.m_true_branch)
						&& IsPureExpression(*node.m_else_branch);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::MemberAccess>)
				{
					return IsPureExpression(*node.m_struct);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Array>)
				{
					for (const std::unique_ptr<MidoriExpression>& elem : node.m_elems)
					{
						if (!IsPureExpression(*elem))
						{
							return false;
						}
					}
					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::IndexAccess>)
				{
					return false;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::RangeBinary>)
				{
					return IsPureExpression(*node.m_start) && IsPureExpression(*node.m_end);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::RangeTernary>)
				{
					return IsPureExpression(*node.m_start)
						&& IsPureExpression(*node.m_step)
						&& IsPureExpression(*node.m_end);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Block>)
				{
					for (const std::unique_ptr<MidoriStatement>& stmt : node.m_stmts)
					{
						if (!IsPureStatement(*stmt))
						{
							return false;
						}
					}

					return !node.m_final_expr.has_value() || IsPureExpression(*node.m_final_expr.value());
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Match>)
				{
					if (!IsPureExpression(*node.m_arg_expr))
					{
						return false;
					}

					for (const std::unique_ptr<MidoriExpression>& case_expr : node.m_cases)
					{
						if (!IsPureExpression(*case_expr))
						{
							return false;
						}
					}

					return true;
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Case>)
				{
					return IsPureExpression(*node.m_expr);
				}
				else if constexpr (std::is_same_v<T, MidoriExpression::Default>)
				{
					return IsPureExpression(*node.m_expr);
				}
				else
				{
					return false;
				}
			},
			**stripped_expr
		);
	}

	bool IsPureStatement(const MidoriStatement& stmt)
	{
		return std::visit
		(
			[](const auto& node) -> bool
			{
				using T = std::decay_t<decltype(node)>;

				if constexpr (std::is_same_v<T, MidoriStatement::ExpressionStatement>)
				{
					return IsPureExpression(*node.m_expr);
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::VariableDefinition>)
				{
					return IsPureExpression(*node.m_value);
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::TupleDefinition>)
				{
					return IsPureExpression(*node.m_value);
				}
				else if constexpr (std::is_same_v<T, MidoriStatement::FunctionDefinition>)
				{
					return true;
				}
				else
				{
					return false;
				}
			},
			*stmt
		);
	}

	class LocalAccessCollector
	{
	public:
		explicit LocalAccessCollector(StatementLocalAccessSummary& summary)
			: m_summary(summary)
		{
		}

		void VisitStatement(const MidoriStatement& stmt)
		{
			std::visit([this](const auto& node) { Visit(node); }, *stmt);
		}

		void VisitExpression(const MidoriExpression& expr)
		{
			std::visit([this](const auto& node) { Visit(node); }, *expr);
		}

	private:
		StatementLocalAccessSummary& m_summary;

		void NoteNestedCallableBoundary()
		{
			m_summary.m_has_nested_callable_boundary = true;
		}

		void RecordLocalRead(const MidoriExpression::NameContext::Tag& name_ctx)
		{
			std::optional<int> local_index = TryGetLocalIndex(name_ctx);
			if (local_index.has_value())
			{
				RecordRead(m_summary, local_index.value());
			}
		}

		void RecordLocalAssignment(const MidoriExpression::NameContext::Tag& name_ctx, bool also_reads)
		{
			std::optional<int> local_index = TryGetLocalIndex(name_ctx);
			if (!local_index.has_value())
			{
				return;
			}

			if (also_reads)
			{
				RecordRead(m_summary, local_index.value());
			}
			RecordAssignment(m_summary, local_index.value());
		}

		void RecordSyntheticLocal(int local_index)
		{
			if (local_index >= 0)
			{
				RecordAssignment(m_summary, local_index);
			}
		}

		void VisitPattern(const MidoriPattern& pattern)
		{
			std::visit
			(
				[this](const auto& node)
				{
					using T = std::decay_t<decltype(node)>;

					if constexpr (std::is_same_v<T, MidoriPattern::Binding>)
					{
						RecordDefinition(m_summary, node.m_local_index);
					}
					else if constexpr (std::is_same_v<T, MidoriPattern::Tuple> || std::is_same_v<T, MidoriPattern::Array>)
					{
						for (const std::unique_ptr<MidoriPattern>& element : node.m_elements)
						{
							VisitPattern(*element);
						}
					}
					else if constexpr (std::is_same_v<T, MidoriPattern::Constructor>)
					{
						for (const std::unique_ptr<MidoriPattern>& arg : node.m_args)
						{
							VisitPattern(*arg);
						}
					}
				},
				*pattern
			);
		}

		void Visit(const MidoriStatement::ExpressionStatement& node)
		{
			VisitExpression(*node.m_expr);
		}

		void Visit(const MidoriStatement::VariableDefinition& node)
		{
			RecordDefinition(m_summary, node.m_local_index);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriStatement::TupleDefinition& node)
		{
			for (const std::optional<int>& local_index : node.m_local_indices)
			{
				RecordDefinition(m_summary, local_index);
			}
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriStatement::FunctionDefinition& node)
		{
			RecordDefinition(m_summary, node.m_local_index);
			NoteNestedCallableBoundary();
		}

		void Visit(const MidoriStatement::Continue&)
		{
		}

		void Visit(const MidoriStatement::ForeignDefinition& node)
		{
			RecordDefinition(m_summary, node.m_local_index);
		}

		void Visit(const MidoriStatement::Struct&)
		{
		}

		void Visit(const MidoriStatement::Union&)
		{
		}

		void Visit(const MidoriStatement::Class&)
		{
			NoteNestedCallableBoundary();
		}

		void Visit(const MidoriStatement::Instance&)
		{
			NoteNestedCallableBoundary();
		}

		void Visit(const MidoriStatement::TypeAlias&)
		{
		}

		void Visit(const MidoriExpression::As& node)
		{
			VisitExpression(*node.m_expr);
		}

		void Visit(const MidoriExpression::Binary& node)
		{
			VisitExpression(*node.m_left);
			VisitExpression(*node.m_right);
		}

		void Visit(const MidoriExpression::Group& node)
		{
			VisitExpression(*node.m_expr_in);
		}

		void Visit(const MidoriExpression::Tuple& node)
		{
			for (const std::unique_ptr<MidoriExpression>& element : node.m_elements)
			{
				VisitExpression(*element);
			}
		}

		void Visit(const MidoriExpression::TextLiteral&)
		{
		}

		void Visit(const MidoriExpression::BoolLiteral&)
		{
		}

		void Visit(const MidoriExpression::FloatLiteral&)
		{
		}

		void Visit(const MidoriExpression::IntegerLiteral&)
		{
		}

		void Visit(const MidoriExpression::ByteLiteral&)
		{
		}

		void Visit(const MidoriExpression::WordLiteral&)
		{
		}

		void Visit(const MidoriExpression::UnitLiteral&)
		{
		}

		void Visit(const MidoriExpression::UnaryPrefix& node)
		{
			VisitExpression(*node.m_expr);
		}

		void Visit(const MidoriExpression::UnarySuffix& node)
		{
			VisitExpression(*node.m_expr);
		}

		void Visit(const MidoriExpression::Assignment& node)
		{
			RecordLocalAssignment(node.m_name_ctx, false);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::AppendAssign& node)
		{
			RecordLocalAssignment(node.m_name_ctx, true);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::ExtendAssign& node)
		{
			RecordLocalAssignment(node.m_name_ctx, true);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::PrependAssign& node)
		{
			RecordLocalAssignment(node.m_name_ctx, true);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::CompoundAssign& node)
		{
			RecordLocalAssignment(node.m_name_ctx, true);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::NameAccess& node)
		{
			RecordLocalRead(node.m_name_ctx);
		}

		void Visit(const MidoriExpression::Call& node)
		{
			VisitExpression(*node.m_callee);
			for (const std::unique_ptr<MidoriExpression>& arg : node.m_arguments)
			{
				VisitExpression(*arg);
			}
		}

		void Visit(const MidoriExpression::Function&)
		{
			NoteNestedCallableBoundary();
		}

		void Visit(const MidoriExpression::Construct& node)
		{
			for (const std::unique_ptr<MidoriExpression>& param : node.m_params)
			{
				VisitExpression(*param);
			}
		}

		void Visit(const MidoriExpression::IfElse& node)
		{
			VisitExpression(*node.m_condition);
			VisitExpression(*node.m_true_branch);
			VisitExpression(*node.m_else_branch);
		}

		void Visit(const MidoriExpression::MemberAccess& node)
		{
			VisitExpression(*node.m_struct);
		}

		void Visit(const MidoriExpression::MemberAssignment& node)
		{
			VisitExpression(*node.m_struct);
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::Array& node)
		{
			for (const std::unique_ptr<MidoriExpression>& elem : node.m_elems)
			{
				VisitExpression(*elem);
			}
		}

		void Visit(const MidoriExpression::IndexAccess& node)
		{
			VisitExpression(*node.m_arr_var);
			for (const std::unique_ptr<MidoriExpression>& index : node.m_indices)
			{
				VisitExpression(*index);
			}
		}

		void Visit(const MidoriExpression::IndexAssignment& node)
		{
			VisitExpression(*node.m_arr_var);
			for (const std::unique_ptr<MidoriExpression>& index : node.m_indices)
			{
				VisitExpression(*index);
			}
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::ArrayComprehension& node)
		{
			RecordSyntheticLocal(node.m_loop_variable_index);
			RecordSyntheticLocal(node.m_hidden_step_index);
			RecordSyntheticLocal(node.m_hidden_end_index);
			RecordSyntheticLocal(node.m_hidden_array_index);
			RecordSyntheticLocal(node.m_result_array_index);
			VisitExpression(*node.m_transform_expr);
			VisitExpression(*node.m_range);
		}

		void Visit(const MidoriExpression::RangeBinary& node)
		{
			VisitExpression(*node.m_start);
			VisitExpression(*node.m_end);
		}

		void Visit(const MidoriExpression::RangeTernary& node)
		{
			VisitExpression(*node.m_start);
			VisitExpression(*node.m_step);
			VisitExpression(*node.m_end);
		}

		void Visit(const MidoriExpression::Block& node)
		{
			for (const std::unique_ptr<MidoriStatement>& stmt : node.m_stmts)
			{
				VisitStatement(*stmt);
			}

			if (node.m_final_expr.has_value())
			{
				VisitExpression(*node.m_final_expr.value());
			}
		}

		void Visit(const MidoriExpression::Match& node)
		{
			RecordSyntheticLocal(node.m_match_value_index);
			VisitExpression(*node.m_arg_expr);
			for (const std::unique_ptr<MidoriExpression>& case_expr : node.m_cases)
			{
				VisitExpression(*case_expr);
			}
		}

		void Visit(const MidoriExpression::Case& node)
		{
			VisitPattern(*node.m_pattern);
			VisitExpression(*node.m_expr);
		}

		void Visit(const MidoriExpression::Default& node)
		{
			VisitExpression(*node.m_expr);
		}

		void Visit(const MidoriExpression::Loop& node)
		{
			VisitExpression(*node.m_body);
		}

		void Visit(const MidoriExpression::For& node)
		{
			RecordSyntheticLocal(node.m_loop_variable_index);
			RecordSyntheticLocal(node.m_hidden_step_index);
			RecordSyntheticLocal(node.m_hidden_end_index);
			RecordSyntheticLocal(node.m_hidden_array_index);
			VisitExpression(*node.m_range);
			VisitExpression(*node.m_body);
		}

		void Visit(const MidoriExpression::Break& node)
		{
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::Return& node)
		{
			VisitExpression(*node.m_value);
		}

		void Visit(const MidoriExpression::Async&)
		{
			NoteNestedCallableBoundary();
		}

		void Visit(const MidoriExpression::Await& node)
		{
			VisitExpression(*node.m_expr);
		}
	};
}

namespace OptimizerAnalysis
{
	const MidoriExpression* StripRedundantGroups(const MidoriExpression* expr)
	{
		const MidoriExpression* current = expr;
		while (current != nullptr && current->IsExpression<MidoriExpression::Group>())
		{
			current = current->GetExpression<MidoriExpression::Group>().m_expr_in.get();
		}
		return current;
	}

	std::unique_ptr<MidoriExpression> StripRedundantGroups(std::unique_ptr<MidoriExpression> expr)
	{
		while (expr && expr->IsExpression<MidoriExpression::Group>())
		{
			expr = std::move(expr->GetExpression<MidoriExpression::Group>().m_expr_in);
		}
		return expr;
	}

	LiteralForm GetLiteralForm(const MidoriExpression& expr)
	{
		const MidoriExpression* stripped_expr = StripRedundantGroups(&expr);
		if (stripped_expr == nullptr)
		{
			return LiteralForm::None;
		}

		if (stripped_expr->IsExpression<MidoriExpression::BoolLiteral>())
		{
			return LiteralForm::Bool;
		}
		if (stripped_expr->IsExpression<MidoriExpression::IntegerLiteral>())
		{
			return LiteralForm::Integer;
		}
		if (stripped_expr->IsExpression<MidoriExpression::FloatLiteral>())
		{
			return LiteralForm::Float;
		}
		if (stripped_expr->IsExpression<MidoriExpression::ByteLiteral>())
		{
			return LiteralForm::Byte;
		}
		if (stripped_expr->IsExpression<MidoriExpression::WordLiteral>())
		{
			return LiteralForm::Word;
		}
		if (stripped_expr->IsExpression<MidoriExpression::TextLiteral>())
		{
			return LiteralForm::Text;
		}
		if (stripped_expr->IsExpression<MidoriExpression::UnitLiteral>())
		{
			return LiteralForm::Unit;
		}

		return LiteralForm::None;
	}

	bool IsLiteralExpression(const MidoriExpression& expr)
	{
		return GetLiteralForm(expr) != LiteralForm::None;
	}

	bool IsPure(const MidoriExpression& expr)
	{
		return IsPureExpression(expr);
	}

	std::optional<ConstantValue> TryEvalConstant(const MidoriExpression& expr)
	{
		return EvalConstant(expr);
	}

	std::optional<ConstantValue> TryEvalConstant(const MidoriExpression::Binary& expr)
	{
		return EvalBinary(expr);
	}

	std::optional<ConstantValue> TryEvalConstant(const MidoriExpression::UnaryPrefix& expr)
	{
		return EvalUnary(expr);
	}

	std::optional<bool> TryEvalTruthValue(const MidoriExpression& expr)
	{
		std::optional<ConstantValue> value = TryEvalConstant(expr);
		if (!value.has_value())
		{
			return std::nullopt;
		}

		return TryGetBoolValue(value.value());
	}

	std::unique_ptr<MidoriExpression> MakeLiteralExpression(const ConstantValue& value, const Token& source_token)
	{
		return std::visit
		(
			[&source_token](const auto& literal_value) -> std::unique_ptr<MidoriExpression>
			{
				using T = std::decay_t<decltype(literal_value)>;

				if constexpr (std::is_same_v<T, MidoriBool>)
				{
					if (literal_value)
					{
						return std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(Token("true", Token::Name::TRUE, source_token.m_line, source_token.m_file_name)));
					}

					return std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(Token("false", Token::Name::FALSE, source_token.m_line, source_token.m_file_name)));
				}
				else if constexpr (std::is_same_v<T, MidoriInteger>)
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(Token(std::to_string(literal_value), Token::Name::INTEGER_LITERAL, source_token.m_line, source_token.m_file_name)));
				}
				else if constexpr (std::is_same_v<T, MidoriFloat>)
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(Token(std::to_string(literal_value), Token::Name::FLOAT_LITERAL, source_token.m_line, source_token.m_file_name)));
				}
				else if constexpr (std::is_same_v<T, MidoriByte>)
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::ByteLiteral(Token(std::to_string(literal_value), Token::Name::INTEGER_LITERAL, source_token.m_line, source_token.m_file_name)));
				}
				else if constexpr (std::is_same_v<T, MidoriWord>)
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::WordLiteral(Token(std::to_string(literal_value), Token::Name::INTEGER_LITERAL, source_token.m_line, source_token.m_file_name)));
				}
				else if constexpr (std::is_same_v<T, std::string>)
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::TextLiteral(Token(std::string(literal_value), Token::Name::TEXT_LITERAL, source_token.m_line, source_token.m_file_name)));
				}
				else
				{
					return std::make_unique<MidoriExpression>(MidoriExpression::UnitLiteral(Token("()", Token::Name::UNIT, source_token.m_line, source_token.m_file_name)));
				}
			},
			value.m_value
		);
	}

	bool StatementLocalAccessSummary::UsesLocal(int local_index) const
	{
		return GetUseCount(local_index) > 0;
	}

	bool StatementLocalAccessSummary::AssignsLocal(int local_index) const
	{
		return GetAssignmentCount(local_index) > 0;
	}

	int StatementLocalAccessSummary::GetUseCount(int local_index) const
	{
		const auto it = m_locals.find(local_index);
		return it == m_locals.end() ? 0 : it->second.m_reads;
	}

	int StatementLocalAccessSummary::GetAssignmentCount(int local_index) const
	{
		const auto it = m_locals.find(local_index);
		return it == m_locals.end() ? 0 : it->second.m_assignments;
	}

	bool BlockLocalAccessSummary::IsLocalUsedAfter(int local_index, std::size_t statement_index) const
	{
		const std::size_t start_index = statement_index + 1u;
		for (std::size_t idx = start_index; idx < m_statement_summaries.size(); idx += 1u)
		{
			if (m_statement_summaries[idx].UsesLocal(local_index))
			{
				return true;
			}
		}

		return m_final_expression_summary.has_value() && m_final_expression_summary->UsesLocal(local_index);
	}

	bool BlockLocalAccessSummary::IsLocalAssignedAfter(int local_index, std::size_t statement_index) const
	{
		const std::size_t start_index = statement_index + 1u;
		for (std::size_t idx = start_index; idx < m_statement_summaries.size(); idx += 1u)
		{
			if (m_statement_summaries[idx].AssignsLocal(local_index))
			{
				return true;
			}
		}

		return m_final_expression_summary.has_value() && m_final_expression_summary->AssignsLocal(local_index);
	}

	StatementLocalAccessSummary AnalyzeStatementLocalAccess(const MidoriStatement& stmt)
	{
		StatementLocalAccessSummary summary;
		LocalAccessCollector collector(summary);
		collector.VisitStatement(stmt);
		return summary;
	}

	StatementLocalAccessSummary AnalyzeExpressionLocalAccess(const MidoriExpression& expr)
	{
		StatementLocalAccessSummary summary;
		LocalAccessCollector collector(summary);
		collector.VisitExpression(expr);
		return summary;
	}

	BlockLocalAccessSummary AnalyzeBlockLocalAccess(const MidoriExpression::Block& block)
	{
		BlockLocalAccessSummary summary;
		summary.m_statement_summaries.reserve(block.m_stmts.size());

		for (const std::unique_ptr<MidoriStatement>& stmt : block.m_stmts)
		{
			summary.m_statement_summaries.emplace_back(AnalyzeStatementLocalAccess(*stmt));
		}

		if (block.m_final_expr.has_value())
		{
			summary.m_final_expression_summary = AnalyzeExpressionLocalAccess(*block.m_final_expr.value());
		}

		return summary;
	}
}
