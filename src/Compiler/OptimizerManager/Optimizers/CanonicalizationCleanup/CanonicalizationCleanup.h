#pragma once

#include "Compiler/OptimizerManager/Optimizers/BaseOptimizer/BaseOptimizer.h"

class CanonicalizationCleanup final : public MidoriOptimizer
{
public:
	MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) override;

	std::string_view GetName() const override;

	using MidoriOptimizer::operator();

	void operator()(MidoriExpression::As& as) override;

	void operator()(MidoriExpression::Binary& binary) override;

	void operator()(MidoriExpression::Group& group) override;

	void operator()(MidoriExpression::UnaryPrefix& unary) override;
};
