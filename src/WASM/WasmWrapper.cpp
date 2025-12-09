#ifdef __EMSCRIPTEN__

#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <emscripten/emscripten.h>
#include <sstream>
#include <string>
#include <iostream>

#include "Compiler/Compiler.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"

using namespace std::string_literals;

struct ExecutionResult
{
	bool success;
	std::string output;
	std::string error;
	int exit_code;
};

namespace
{
	std::ostringstream g_output_stream;
	std::ostringstream g_error_stream;
	std::streambuf* g_original_cout_buffer = nullptr;
	std::streambuf* g_original_cerr_buffer = nullptr;

	// Strip ANSI color codes from string
	std::string StripAnsiCodes(const std::string& str)
	{
		std::string result;
		result.reserve(str.size());

		bool in_escape = false;
		for (size_t i = 0u; i < str.size(); i += 1u)
		{
			if (str[i] == '\033')
			{
				in_escape = true;
				continue;
			}

			if (in_escape)
			{
				if (str[i] == 'm')
				{
					in_escape = false;
				}
				continue;
			}

			result += str[i];
		}

		return result;
	}

	void StartCapture()
	{
		g_output_stream.str("");
		g_output_stream.clear();
		g_error_stream.str("");
		g_error_stream.clear();

		g_original_cout_buffer = std::cout.rdbuf(g_output_stream.rdbuf());
		g_original_cerr_buffer = std::cerr.rdbuf(g_error_stream.rdbuf());
	}

	void StopCapture()
	{
		if (g_original_cout_buffer != nullptr)
		{
			std::cout.rdbuf(g_original_cout_buffer);
		}
		if (g_original_cerr_buffer != nullptr)
		{
			std::cerr.rdbuf(g_original_cerr_buffer);
		}
	}
}

ExecutionResult ExecuteMidoriCode(const std::string& source_code)
{
	ExecutionResult result;
	result.success = false;
	result.exit_code = EXIT_FAILURE;

	StartCapture();

	MidoriResult::CompilerResult compile_result = Compiler(std::string(source_code), "/playground.mdr"s).Compile();

	if (!compile_result.has_value())
	{
		result.error = StripAnsiCodes(std::move(compile_result.error()));
		result.output = StripAnsiCodes(g_output_stream.str());
		StopCapture();
		return result;
	}

	int exit_code = VirtualMachine(std::move(compile_result.value())).Execute();

	result.success = (exit_code == EXIT_SUCCESS);
	result.exit_code = exit_code;
	result.output = StripAnsiCodes(g_output_stream.str());
	result.error = StripAnsiCodes(g_error_stream.str());

	StopCapture();
	return result;
}

EMSCRIPTEN_BINDINGS(midori_module)
{
	emscripten::value_object<ExecutionResult>("ExecutionResult")
		.field("success", &ExecutionResult::success)
		.field("output", &ExecutionResult::output)
		.field("error", &ExecutionResult::error)
		.field("exitCode", &ExecutionResult::exit_code);

	emscripten::function("executeMidoriCode", &ExecuteMidoriCode);
}

#endif // __EMSCRIPTEN__
