#pragma once

#include "Common/Result/Result.h"

class Compiler
{
private:
	std::string m_source_code;
	std::string m_file_name;

public:
	Compiler(std::string&& source_code, std::string&& file_name);

	MidoriResult::CompilerResult Compile();
};
