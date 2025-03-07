#include <algorithm>
#include <fstream>
#include <sstream>

#include "Common/Printer/Printer.h"
#include "Compiler/Compiler.h"
#include "Interpreter/VirtualMachine/VirtualMachine.h"

using namespace std::string_literals;

std::string ReadFile(const char* filename)
{
	std::ifstream file(filename);
	if (!file.is_open())
	{
		Printer::Print<Printer::Color::RED>(std::format("Could not open file: {}\n", filename));
		std::exit(EXIT_FAILURE);
	}

	std::ostringstream buffer;
	buffer << file.rdbuf();
	if (!buffer)
	{
		Printer::Print<Printer::Color::RED>(std::format("Could not read file to buffer: {}\n", filename));
		std::exit(EXIT_FAILURE);
	}

	return buffer.str();
}

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        Printer::Print<Printer::Color::RED>("Usage: Midori.exe <source_file_path>\n");
        return EXIT_FAILURE;
    }

    std::string file_name = argv[1u];
    std::string file_content = ReadFile(file_name.data());

    return Compiler(std::move(file_content), std::move(file_name))
        .Compile()
        .and_then
        (
            [](MidoriExecutable&& executable) -> std::expected<int, std::string>
            {
                return VirtualMachine(std::move(executable))
                    .Execute();
            }
        )
        .or_else
        (
            [](std::string&& compilation_error) -> std::expected<int, std::string>
            {
                Printer::Print<Printer::Color::RED>("Compilation failed :( \n");
                Printer::Print<Printer::Color::RED>(std::format("{}", compilation_error));
                return EXIT_FAILURE;
            }
        )
        .value();
}