#include <cmath>

#include "StrengthReduction.h"
#include "Compiler/Token/Token.h"

int StrengthReduction::Optimize(MidoriProgramTree& program_tree)
{
#ifdef DEBUG
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

#ifdef DEBUG
	return GetOptimizationsPerformed();
#else
	return 0;
#endif
}

std::string_view StrengthReduction::GetName() const
{
	return "StrengthReduction";
}

void StrengthReduction::operator()(MidoriExpression::Binary& binary)
{
	VisitAndReplace(binary.m_left);
	VisitAndReplace(binary.m_right);

	MidoriExpression::IntegerLiteral* left_int = binary.m_left->IsExpression<MidoriExpression::IntegerLiteral>() ? &binary.m_left->GetExpression<MidoriExpression::IntegerLiteral>() : nullptr;
	MidoriExpression::IntegerLiteral* right_int = binary.m_right->IsExpression<MidoriExpression::IntegerLiteral>() ? &binary.m_right->GetExpression<MidoriExpression::IntegerLiteral>() : nullptr;

	MidoriExpression::FloatLiteral* left_float = binary.m_left->IsExpression<MidoriExpression::FloatLiteral>() ? &binary.m_left->GetExpression<MidoriExpression::FloatLiteral>() : nullptr;
	MidoriExpression::FloatLiteral* right_float = binary.m_right->IsExpression<MidoriExpression::FloatLiteral>() ? &binary.m_right->GetExpression<MidoriExpression::FloatLiteral>() : nullptr;

	std::unique_ptr<MidoriExpression> reduced = TryReduceBinary(binary, binary.m_op, left_int, right_int, left_float, right_float);

	if (reduced)
	{
		m_pending_replacement = std::move(reduced);
	}
}

double StrengthReduction::GetFloatValue(MidoriExpression::FloatLiteral* float_lit)
{
	return std::stod(float_lit->m_token.m_lexeme);
}

int64_t StrengthReduction::IsPowerOfTwo(MidoriInteger value)
{
	if (value <= 0ll)
	{
		return -1ll;
	}

	if ((value & (value - 1ll)) != 0ll)
	{
		return -1ll;
	}

	int64_t exponent = 0ll;
	while (value > 1ll)
	{
		value >>= 1ll;
		exponent += 1ll;
	}

	return exponent;
}

std::unique_ptr<MidoriExpression> StrengthReduction::TryReduceBinary(MidoriExpression::Binary& binary, const Token& op, MidoriExpression::IntegerLiteral* left_int, MidoriExpression::IntegerLiteral* right_int, MidoriExpression::FloatLiteral* left_float, MidoriExpression::FloatLiteral* right_float)
{
	if (left_int || right_int)
	{
		// x * 0 → 0
		if (op.m_token_name == Token::Name::STAR)
		{
			if (right_int && std::stoll(right_int->m_token.m_lexeme) == 0ll)
			{
				Token zero_token("0", Token::Name::INTEGER_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(zero_token));
			}
			if (left_int && std::stoll(left_int->m_token.m_lexeme) == 0ll)
			{
				Token zero_token("0", Token::Name::INTEGER_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(zero_token));
			}
		}

		// x * 1 → x
		if (op.m_token_name == Token::Name::STAR)
		{
			if (right_int && std::stoll(right_int->m_token.m_lexeme) == 1ll)
			{
				return std::move(binary.m_left);
			}
			if (left_int && std::stoll(left_int->m_token.m_lexeme) == 1ll)
			{
				return std::move(binary.m_right);
			}
		}

		// x / 1 → x
		if (op.m_token_name == Token::Name::SLASH)
		{
			if (right_int && std::stoll(right_int->m_token.m_lexeme) == 1ll)
			{
				return std::move(binary.m_left);
			}
		}

		// x + 0 → x
		if (op.m_token_name == Token::Name::SINGLE_PLUS)
		{
			if (right_int && std::stoll(right_int->m_token.m_lexeme) == 0ll)
			{
				return std::move(binary.m_left);
			}
			if (left_int && std::stoll(left_int->m_token.m_lexeme) == 0ll)
			{
				return std::move(binary.m_right);
			}
		}

		// x - 0 → x
		if (op.m_token_name == Token::Name::SINGLE_MINUS)
		{
			if (right_int && std::stoll(right_int->m_token.m_lexeme) == 0ll)
			{
				return std::move(binary.m_left);
			}
		}

		// 0 - x → -x
		if (op.m_token_name == Token::Name::SINGLE_MINUS)
		{
			if (left_int && std::stoll(left_int->m_token.m_lexeme) == 0ll)
			{
				Token minus_token("-", Token::Name::SINGLE_MINUS, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::UnaryPrefix(minus_token, std::move(binary.m_right)));
			}
		}

		// x * -1 → -x
		if (op.m_token_name == Token::Name::STAR)
		{
			if (right_int && std::stoll(right_int->m_token.m_lexeme) == -1ll)
			{
				Token minus_token("-", Token::Name::SINGLE_MINUS, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::UnaryPrefix(minus_token, std::move(binary.m_left)));
			}
			if (left_int && std::stoll(left_int->m_token.m_lexeme) == -1ll)
			{
				Token minus_token("-", Token::Name::SINGLE_MINUS, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::UnaryPrefix(minus_token, std::move(binary.m_right)));
			}
		}

		// x * 2^n → x << n (only if right side is power of 2)
		if (op.m_token_name == Token::Name::STAR && right_int)
		{
			MidoriInteger right_val = std::stoll(right_int->m_token.m_lexeme);
			int64_t exponent = IsPowerOfTwo(right_val);
			if (exponent >= 0ll)
			{
				Token shift_token("<<", Token::Name::LEFT_SHIFT, op.m_line);
				Token exp_token(std::to_string(exponent), Token::Name::INTEGER_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::Binary(shift_token, std::move(binary.m_left), std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(exp_token))));
			}
		}

		// x / 2^n → x >> n (only if right side is power of 2)
		if (op.m_token_name == Token::Name::SLASH && right_int)
		{
			MidoriInteger right_val = std::stoll(right_int->m_token.m_lexeme);
			int64_t exponent = IsPowerOfTwo(right_val);
			if (exponent >= 0ll)
			{
				Token shift_token(">>", Token::Name::RIGHT_SHIFT, op.m_line);
				Token exp_token(std::to_string(exponent), Token::Name::INTEGER_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::Binary(shift_token, std::move(binary.m_left), std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(exp_token))));
			}
		}

		// x % 2^n → x & (2^n - 1)
		if (op.m_token_name == Token::Name::PERCENT && right_int)
		{
			MidoriInteger right_val = std::stoll(right_int->m_token.m_lexeme);
			int64_t exponent = IsPowerOfTwo(right_val);
			if (exponent >= 0ll)
			{
				Token and_token("&", Token::Name::SINGLE_AMPERSAND, op.m_line);
				MidoriInteger mask = right_val - 1;
				Token mask_token(std::to_string(mask), Token::Name::INTEGER_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::Binary(and_token, std::move(binary.m_left), std::make_unique<MidoriExpression>(MidoriExpression::IntegerLiteral(mask_token))));
			}
		}
	}

	// Float operations
	if (left_float || right_float)
	{
		// x * 0.0 → 0.0
		if (op.m_token_name == Token::Name::STAR)
		{
			if (right_float && GetFloatValue(right_float) == 0.0)
			{
				Token zero_token("0.0", Token::Name::FLOAT_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(zero_token));
			}
			if (left_float && GetFloatValue(left_float) == 0.0)
			{
				Token zero_token("0.0", Token::Name::FLOAT_LITERAL, op.m_line);
				return std::make_unique<MidoriExpression>(MidoriExpression::FloatLiteral(zero_token));
			}
		}

		// x * 1.0 → x
		if (op.m_token_name == Token::Name::STAR)
		{
			if (right_float && GetFloatValue(right_float) == 1.0)
			{
				return std::move(binary.m_left);
			}
			if (left_float && GetFloatValue(left_float) == 1.0)
			{
				return std::move(binary.m_right);
			}
		}

		// x / 1.0 → x
		if (op.m_token_name == Token::Name::SLASH)
		{
			if (right_float && GetFloatValue(right_float) == 1.0)
			{
				return std::move(binary.m_left);
			}
		}

		// x + 0.0 → x
		if (op.m_token_name == Token::Name::SINGLE_PLUS)
		{
			if (right_float && GetFloatValue(right_float) == 0.0)
			{
				return std::move(binary.m_left);
			}
			if (left_float && GetFloatValue(left_float) == 0.0)
			{
				return std::move(binary.m_right);
			}
		}

		// x - 0.0 → x
		if (op.m_token_name == Token::Name::SINGLE_MINUS)
		{
			if (right_float && GetFloatValue(right_float) == 0.0)
			{
				return std::move(binary.m_left);
			}
		}
	}

	return nullptr;
}
