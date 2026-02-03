#pragma once

#include "Compiler/OptimizerManager/Optimizers/BaseOptimizer/BaseOptimizer.h"

class SelfConcatOptimization final : public MidoriOptimizer
{
public:
	MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) override;

	std::string_view GetName() const override;

protected:
	void operator()(MidoriExpression::Assignment& bind) override;
};
