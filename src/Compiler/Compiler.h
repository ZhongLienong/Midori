#pragma once

#include "Compiler/Module/CompiledModule.h"
#include "Compiler/Result/Result.h"
#include <memory>
#include <string>
#include <vector>

class MidoriType;

class Compiler
{
private:
	std::string m_source_code;
	std::string m_file_name;
	std::vector<std::string> m_source_lines;

	static bool InstanceTypeArgsEqual(const std::vector<std::shared_ptr<MidoriType>>& left, const std::vector<std::shared_ptr<MidoriType>>& right);

protected:
	static void MergeInstanceMethods(std::vector<std::string>& target, const std::vector<std::string>& incoming);
	static void MergeInstanceTypeArgs(std::vector<std::vector<std::shared_ptr<MidoriType>>>& target, const std::vector<std::vector<std::shared_ptr<MidoriType>>>& incoming);
	static bool TypeclassDefinitionsMatch(const CompiledModule::TypeclassMetadata& left, const CompiledModule::TypeclassMetadata& right);

public:
	Compiler(std::string&& source_code, std::string&& file_name);

	MidoriResult::CompilerResult Compile();
};
