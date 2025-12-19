#include "ConstantFolding.h"
#include "Common/BuildConfig/BuildConfig.h"
#include "Compiler/Token/Token.h"
#include <stdexcept>
#include <optional>

namespace
{
	std::optional<MidoriInteger> SafeParseInteger(const std::string& str)
	{
		try
		{
			return std::stoll(str);
		}
		catch (const std::out_of_range&)
		{
			return std::nullopt;
		}
		catch (const std::invalid_argument&)
		{
			return std::nullopt;
		}
	}
}

int ConstantFolding::Optimize(MidoriProgramTree& program_tree)
{
#if MIDORI_ENABLE_OPTIMIZER_STATS
	ResetCounter();
#endif

	std::ranges::for_each
	(
		program_tree,
		[this](std::unique_ptr<MidoriStatement>& stmt)
		{
			std::visit([this](auto&& arg) { (*this)(arg); }, **stmt);
		}
	);

#if MIDORI_ENABLE_OPTIMIZER_STATS
	return GetOptimizationsPerformed();
#else
	return 0;
#endif
}

std::string_view ConstantFolding::GetName() const
{
	return "ConstantFolding";
}

void ConstantFolding::operator()(MidoriExpression::Binary& binary)
{
	VisitAndReplace(binary.m_left);
	VisitAndReplace(binary.m_right);

	std::shared_ptr<MidoriType> original_type = binary.m_type_data;

	Token& op = binary.m_op;

	if (binary.m_left->IsExpression<MidoriExpression::IntegerLiteral>() && binary.m_right->IsExpression<MidoriExpression::IntegerLiteral>())
	{
		std::optional<MidoriInteger> left_opt = SafeParseInteger(binary.m_left->GetExpression<MidoriExpression::IntegerLiteral>().m_token.m_lexeme);
		std::optional<MidoriInteger> right_opt = SafeParseInteger(binary.m_right->GetExpression<MidoriExpression::IntegerLiteral>().m_token.m_lexeme);

		if (!left_opt.has_value() || !right_opt.has_value())
		{
			return;
		}

		MidoriInteger left_val = left_opt.value();
		MidoriInteger right_val = right_opt.value();
		MidoriInteger result = 0ll;

		switch (op.m_token_name)
		{
		case Token::Name::SINGLE_PLUS:
			result = left_val + right_val;
			break;
		case Token::Name::SINGLE_MINUS:
			result = left_val - right_val;
			break;
		case Token::Name::STAR:
			result = left_val * right_val;
			break;
		case Token::Name::SLASH:
			if (right_val == 0ll)
			{
				return;
			}
			result = left_val / right_val;
			break;
		case Token::Name::PERCENT:
			if (right_val == 0ll)
			{
				return;
			}
			result = left_val % right_val;
			break;
		case Token::Name::LEFT_SHIFT:
			result = left_val << right_val;
			break;
		case Token::Name::RIGHT_SHIFT:
			result = left_val >> right_val;
			break;
		case Token::Name::SINGLE_AMPERSAND:
			result = left_val & right_val;
			break;
		case Token::Name::SINGLE_BAR:
			result = left_val | right_val;
			break;
		case Token::Name::CARET:
			result = left_val ^ right_val;
			break;
		default:
			if (op.m_token_name == Token::Name::LEFT_ANGLE || op.m_token_name == Token::Name::RIGHT_ANGLE || op.m_token_name == Token::Name::LESS_EQUAL || op.m_token_name == Token::Name::GREATER_EQUAL || op.m_token_name == Token::Name::DOUBLE_EQUAL || op.m_token_name == Token::Name::BANG_EQUAL)
			{
				bool bool_result = false;
				switch (op.m_token_name)
				{
				case Token::Name::LEFT_ANGLE:
					bool_result = left_val < right_val;
					break;
				case Token::Name::RIGHT_ANGLE:
					bool_result = left_val > right_val;
					break;
				case Token::Name::LESS_EQUAL:
					bool_result = left_val <= right_val;
					break;
				case Token::Name::GREATER_EQUAL:
					bool_result = left_val >= right_val;
					break;
				case Token::Name::DOUBLE_EQUAL:
					bool_result = left_val == right_val;
					break;
				case Token::Name::BANG_EQUAL:
					bool_result = left_val != right_val;
					break;
				default:
					break;
				}

				if (bool_result)
				{
					Token result_token("true", Token::Name::TRUE, op.m_line);
					m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(result_token));
				}
				else
				{
					Token result_token("false", Token::Name::FALSE, op.m_line);
					m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(result_token));
				}
				return;
			}
		}

		Token result_token(std::to_string(result), Token::Name::INTEGER_LITERAL, op.m_line);
		m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(result_token));
	}
	else if (binary.m_left->IsExpression<MidoriExpression::FloatLiteral>() && binary.m_right->IsExpression<MidoriExpression::FloatLiteral>())
	{
		MidoriFloat left_val = std::stod(binary.m_left->GetExpression<MidoriExpression::FloatLiteral>().m_token.m_lexeme);
		MidoriFloat right_val = std::stod(binary.m_right->GetExpression<MidoriExpression::FloatLiteral>().m_token.m_lexeme);
		MidoriFloat result = 0.0;

		switch (op.m_token_name)
		{
		case Token::Name::SINGLE_PLUS:
			result = left_val + right_val;
			break;
		case Token::Name::SINGLE_MINUS:
			result = left_val - right_val;
			break;
		case Token::Name::STAR:
			result = left_val * right_val;
			break;
		case Token::Name::SLASH:
			if (right_val == 0.0)
			{
				return;
			}
			result = left_val / right_val;
			break;
		default:
			return;
		}

		Token result_token(std::to_string(result), Token::Name::FLOAT_LITERAL, op.m_line);
		m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(result_token));
	}
	else if (binary.m_left->IsExpression<MidoriExpression::TextLiteral>() && binary.m_right->IsExpression<MidoriExpression::TextLiteral>())
	{
		std::string result = binary.m_left->GetExpression<MidoriExpression::TextLiteral>().m_token.m_lexeme + binary.m_right->GetExpression<MidoriExpression::TextLiteral>().m_token.m_lexeme;
		Token result_token(std::move(result), Token::Name::TEXT_LITERAL, op.m_line);
		m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::TextLiteral(result_token));
	}
	else if (binary.m_left->IsExpression<MidoriExpression::BoolLiteral>() && binary.m_right->IsExpression<MidoriExpression::BoolLiteral>())
	{
		MidoriBool left_val = (binary.m_left->GetExpression<MidoriExpression::BoolLiteral>().m_token.m_token_name == Token::Name::TRUE);
		MidoriBool right_val = (binary.m_right->GetExpression<MidoriExpression::BoolLiteral>().m_token.m_token_name == Token::Name::TRUE);
		bool result = false;

		switch (op.m_token_name)
		{
		case Token::Name::DOUBLE_AMPERSAND:
			result = left_val && right_val;
			break;
		case Token::Name::DOUBLE_BAR:
			result = left_val || right_val;
			break;
		case Token::Name::DOUBLE_EQUAL:
			result = left_val == right_val;
			break;
		case Token::Name::BANG_EQUAL:
			result = left_val != right_val;
			break;
		default:
			return;
		}

		if (result)
		{
			Token result_token("true", Token::Name::TRUE, op.m_line);
			m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(result_token));
		}
		else
		{
			Token result_token("false", Token::Name::FALSE, op.m_line);
			m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(result_token));
		}
	}

	if (m_pending_replacement)
	{
		m_pending_replacement->GetType() = original_type;
	}
}

void ConstantFolding::operator()(MidoriExpression::UnaryPrefix& unary)
{
	VisitAndReplace(unary.m_expr);

	std::shared_ptr<MidoriType> original_type = unary.m_type_data;

	Token& op = unary.m_op;

	if (unary.m_expr->IsExpression<MidoriExpression::IntegerLiteral>())
	{
		std::optional<MidoriInteger> val_opt = SafeParseInteger(unary.m_expr->GetExpression<MidoriExpression::IntegerLiteral>().m_token.m_lexeme);

		if (!val_opt.has_value())
		{
			return;
		}

		MidoriInteger val = val_opt.value();
		if (op.m_token_name == Token::Name::SINGLE_MINUS)
		{
			MidoriInteger result = -val;
			Token result_token(std::to_string(result), Token::Name::INTEGER_LITERAL, op.m_line);
			m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(result_token));
		}
		else if (op.m_token_name == Token::Name::TILDE)
		{
			MidoriInteger result = ~val;
			Token result_token(std::to_string(result), Token::Name::INTEGER_LITERAL, op.m_line);
			m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(result_token));
		}
	}
	else if (unary.m_expr->IsExpression<MidoriExpression::FloatLiteral>())
	{
		MidoriFloat val = std::stod(unary.m_expr->GetExpression<MidoriExpression::FloatLiteral>().m_token.m_lexeme);
		MidoriFloat result = -val;
		Token result_token(std::to_string(result), Token::Name::FLOAT_LITERAL, op.m_line);
		m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(result_token));
	}
	else if (unary.m_expr->IsExpression<MidoriExpression::BoolLiteral>())
	{
		MidoriBool val = (unary.m_expr->GetExpression<MidoriExpression::BoolLiteral>().m_token.m_token_name == Token::Name::TRUE);
		MidoriBool result = !val;
		if (result)
		{
			Token result_token("true", Token::Name::TRUE, op.m_line);
			m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(result_token));
		}
		else
		{
			Token result_token("false", Token::Name::FALSE, op.m_line);
			m_pending_replacement = std::make_unique<MidoriExpression>(MidoriExpression::BoolLiteral(result_token));
		}
	}

	if (m_pending_replacement)
	{
		m_pending_replacement->GetType() = original_type;
	}
}