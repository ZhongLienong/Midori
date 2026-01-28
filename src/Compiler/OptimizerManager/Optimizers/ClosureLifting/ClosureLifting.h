#pragma once

#include "Compiler/OptimizerManager/Optimizers/BaseOptimizer/BaseOptimizer.h"

class ClosureLifting : public MidoriOptimizer
{
public:
	MidoriResult::OptimizerResult Optimize(MidoriProgramTree program_tree) override;

	std::string_view GetName() const override;

protected:
	using MidoriOptimizer::operator();

	void operator()(MidoriExpression::Function& function) override;

	void operator()(MidoriExpression::Block& block) override;

private:
	std::vector<std::unique_ptr<MidoriStatement>> m_new_globals;
};
