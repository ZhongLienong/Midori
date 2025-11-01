#pragma once

#include "Compiler/Result/Result.h"
#include <vector>
#include <string>

class Compiler
{
private:
	std::string m_source_code;
	std::string m_file_name;
	std::vector<std::string> m_source_lines;

public:
	Compiler(std::string&& source_code, std::string&& file_name);

	MidoriResult::CompilerResult Compile();
};
