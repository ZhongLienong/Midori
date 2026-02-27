#include <bit>
#include <cctype>
#include <filesystem>
#include <format>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <set>

#include "CodeGenerator.h"
#include "Common/Constant/Constant.h"

using namespace std::string_literals;

namespace
{
	bool HasNameSuffix(const std::string& name, std::string_view suffix)
	{
		if (name == suffix)
		{
			return true;
		}
		if (!name.ends_with(suffix))
		{
			return false;
		}
		size_t pos = name.size() - suffix.size();
		return pos >= 2u && name[pos - 1u] == ':' && name[pos - 2u] == ':';
	}

	bool ExpressionContainsAsyncOrAwait(const MidoriExpression& expression);

	bool StatementContainsAsyncOrAwait(const MidoriStatement& statement)
	{
		return std::visit
		(
			[](const auto& node) -> bool
			{
				using StatementNode = std::decay_t<decltype(node)>;
				if constexpr (std::is_same_v<StatementNode, MidoriStatement::ExpressionStatement>)
				{
					return node.m_expr && ExpressionContainsAsyncOrAwait(*node.m_expr);
				}
				else if constexpr (std::is_same_v<StatementNode, MidoriStatement::VariableDefinition>)
				{
					return node.m_value && ExpressionContainsAsyncOrAwait(*node.m_value);
				}
				else if constexpr (std::is_same_v<StatementNode, MidoriStatement::TupleDefinition>)
				{
					return node.m_value && ExpressionContainsAsyncOrAwait(*node.m_value);
				}
				else if constexpr (std::is_same_v<StatementNode, MidoriStatement::FunctionDefinition>)
				{
					return node.m_body && ExpressionContainsAsyncOrAwait(*node.m_body);
				}
				else if constexpr (std::is_same_v<StatementNode, MidoriStatement::Class>)
				{
					return std::ranges::any_of
					(
						node.m_methods,
						[](const std::unique_ptr<MidoriStatement>& method)
						{
							return method && StatementContainsAsyncOrAwait(*method);
						}
					);
				}
				else if constexpr (std::is_same_v<StatementNode, MidoriStatement::Instance>)
				{
					return std::ranges::any_of
					(
						node.m_methods,
						[](const std::unique_ptr<MidoriStatement>& method)
						{
							return method && StatementContainsAsyncOrAwait(*method);
						}
					);
				}
				else
				{
					return false;
				}
			},
			*statement
		);
	}

	bool ExpressionContainsAsyncOrAwait(const MidoriExpression& expression)
	{
		return std::visit
		(
			[](const auto& node) -> bool
			{
				using ExpressionNode = std::decay_t<decltype(node)>;
				if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Async> || std::is_same_v<ExpressionNode, MidoriExpression::Await>)
				{
					return true;
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::As>)
				{
					return node.m_expr && ExpressionContainsAsyncOrAwait(*node.m_expr);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Binary>)
				{
					return (node.m_left && ExpressionContainsAsyncOrAwait(*node.m_left))
						|| (node.m_right && ExpressionContainsAsyncOrAwait(*node.m_right));
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Group>)
				{
					return node.m_expr_in && ExpressionContainsAsyncOrAwait(*node.m_expr_in);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Tuple>)
				{
					return std::ranges::any_of
					(
						node.m_elements,
						[](const std::unique_ptr<MidoriExpression>& element)
						{
							return element && ExpressionContainsAsyncOrAwait(*element);
						}
					);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::UnaryPrefix> || std::is_same_v<ExpressionNode, MidoriExpression::UnarySuffix>)
				{
					return node.m_expr && ExpressionContainsAsyncOrAwait(*node.m_expr);
				}
				else if constexpr
				(
					std::is_same_v<ExpressionNode, MidoriExpression::Assignment>
					|| std::is_same_v<ExpressionNode, MidoriExpression::AppendAssign>
					|| std::is_same_v<ExpressionNode, MidoriExpression::ExtendAssign>
					|| std::is_same_v<ExpressionNode, MidoriExpression::PrependAssign>
					|| std::is_same_v<ExpressionNode, MidoriExpression::CompoundAssign>
				)
				{
					return node.m_value && ExpressionContainsAsyncOrAwait(*node.m_value);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Call>)
				{
					const bool callee_contains_async = node.m_callee && ExpressionContainsAsyncOrAwait(*node.m_callee);
					const bool argument_contains_async = std::ranges::any_of
					(
						node.m_arguments,
						[](const std::unique_ptr<MidoriExpression>& argument)
						{
							return argument && ExpressionContainsAsyncOrAwait(*argument);
						}
					);

					return callee_contains_async || argument_contains_async;
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Function>)
				{
					return node.m_body && ExpressionContainsAsyncOrAwait(*node.m_body);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Construct>)
				{
					return std::ranges::any_of
					(
						node.m_params,
						[](const std::unique_ptr<MidoriExpression>& argument)
						{
							return argument && ExpressionContainsAsyncOrAwait(*argument);
						}
					);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::IfElse>)
				{
					return (node.m_condition && ExpressionContainsAsyncOrAwait(*node.m_condition))
						|| (node.m_true_branch && ExpressionContainsAsyncOrAwait(*node.m_true_branch))
						|| (node.m_else_branch && ExpressionContainsAsyncOrAwait(*node.m_else_branch));
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::MemberAccess>)
				{
					return node.m_struct && ExpressionContainsAsyncOrAwait(*node.m_struct);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::MemberAssignment>)
				{
					return (node.m_struct && ExpressionContainsAsyncOrAwait(*node.m_struct))
						|| (node.m_value && ExpressionContainsAsyncOrAwait(*node.m_value));
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Array>)
				{
					return std::ranges::any_of
					(
						node.m_elems,
						[](const std::unique_ptr<MidoriExpression>& element)
						{
							return element && ExpressionContainsAsyncOrAwait(*element);
						}
					);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::IndexAccess>)
				{
					const bool array_contains_async = node.m_arr_var && ExpressionContainsAsyncOrAwait(*node.m_arr_var);
					const bool index_contains_async = std::ranges::any_of
					(
						node.m_indices,
						[](const std::unique_ptr<MidoriExpression>& index)
						{
							return index && ExpressionContainsAsyncOrAwait(*index);
						}
					);

					return array_contains_async || index_contains_async;
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::IndexAssignment>)
				{
					const bool array_contains_async = node.m_arr_var && ExpressionContainsAsyncOrAwait(*node.m_arr_var);
					const bool index_contains_async = std::ranges::any_of
					(
						node.m_indices,
						[](const std::unique_ptr<MidoriExpression>& index)
						{
							return index && ExpressionContainsAsyncOrAwait(*index);
						}
					);
					const bool value_contains_async = node.m_value && ExpressionContainsAsyncOrAwait(*node.m_value);

					return array_contains_async || index_contains_async || value_contains_async;
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::ArrayComprehension>)
				{
					return (node.m_transform_expr && ExpressionContainsAsyncOrAwait(*node.m_transform_expr))
						|| (node.m_range && ExpressionContainsAsyncOrAwait(*node.m_range));
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::RangeBinary>)
				{
					return (node.m_start && ExpressionContainsAsyncOrAwait(*node.m_start))
						|| (node.m_end && ExpressionContainsAsyncOrAwait(*node.m_end));
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::RangeTernary>)
				{
					return (node.m_start && ExpressionContainsAsyncOrAwait(*node.m_start))
						|| (node.m_step && ExpressionContainsAsyncOrAwait(*node.m_step))
						|| (node.m_end && ExpressionContainsAsyncOrAwait(*node.m_end));
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Block>)
				{
					const bool statement_contains_async = std::ranges::any_of
					(
						node.m_stmts,
						[](const std::unique_ptr<MidoriStatement>& statement)
						{
							return statement && StatementContainsAsyncOrAwait(*statement);
						}
					);
					const bool final_expression_contains_async = node.m_final_expr.has_value()
						&& node.m_final_expr.value()
						&& ExpressionContainsAsyncOrAwait(*node.m_final_expr.value());

					return statement_contains_async || final_expression_contains_async;
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Match>)
				{
					const bool argument_contains_async = node.m_arg_expr && ExpressionContainsAsyncOrAwait(*node.m_arg_expr);
					const bool case_contains_async = std::ranges::any_of
					(
						node.m_cases,
						[](const std::unique_ptr<MidoriExpression>& case_expression)
						{
							return case_expression && ExpressionContainsAsyncOrAwait(*case_expression);
						}
					);

					return argument_contains_async || case_contains_async;
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Case> || std::is_same_v<ExpressionNode, MidoriExpression::Default>)
				{
					return node.m_expr && ExpressionContainsAsyncOrAwait(*node.m_expr);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Loop>)
				{
					return node.m_body && ExpressionContainsAsyncOrAwait(*node.m_body);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::Return> || std::is_same_v<ExpressionNode, MidoriExpression::Break>)
				{
					return node.m_value && ExpressionContainsAsyncOrAwait(*node.m_value);
				}
				else if constexpr (std::is_same_v<ExpressionNode, MidoriExpression::For>)
				{
					return (node.m_range && ExpressionContainsAsyncOrAwait(*node.m_range))
						|| (node.m_body && ExpressionContainsAsyncOrAwait(*node.m_body));
				}
				else
				{
					return false;
				}
			},
			*expression
		);
	}

	bool ProgramContainsAsyncOrAwait(const MidoriProgramTree& program)
	{
		return std::ranges::any_of
		(
			program,
			[](const std::unique_ptr<MidoriStatement>& statement)
			{
				return statement && StatementContainsAsyncOrAwait(*statement);
			}
		);
	}
}

CodeGenerator::BytecodeBuilder CodeGenerator::BytecodeBuilder::EmitByte(OpCode byte, int line) &&
{
	m_last_opcode = byte;
	m_procedures[m_current_procedure_index].AddByteCode(byte, line);
	return std::move(*this);
}

CodeGenerator::BytecodeBuilder CodeGenerator::BytecodeBuilder::PopByte(int line) &&
{
	m_procedures[m_current_procedure_index].PopByteCode(line);
	return std::move(*this);
}

void CodeGenerator::EmitByte(OpCode byte, int line)
{
	m_builder = std::move(m_builder).EmitByte(byte, line);
}

void CodeGenerator::AddError(const CompilerError& error)
{
	m_errors.append(error.Rendered());
	m_errors.push_back('\n');
}

void CodeGenerator::PopByte(int line)
{
	m_builder = std::move(m_builder).PopByte(line);
}

void CodeGenerator::EmitTextConstant(std::string_view data, int line)
{
	if (m_builder.m_string_pool_index + 1 >= MAX_SIZE_OP_CONSTANT_LONG)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Too many text constants", line, m_file_name, m_source_lines));
		return;
	}

	m_builder.m_string_pool.emplace_back(data);
	const int string_index = m_builder.m_string_pool_index++;
	EmitByte(OpCode::LOAD_STRING_WIDE, line);
	EmitTwoBytes(string_index & BYTE_MASK, (string_index >> SHIFT_8_BITS) & BYTE_MASK, line);
}

void CodeGenerator::EmitTwoBytes(int byte1, int byte2, int line)
{
	EmitByte(static_cast<OpCode>(byte1 & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(byte2 & BYTE_MASK), line);
}

void CodeGenerator::EmitThreeBytes(int byte1, int byte2, int byte3, int line)
{
	EmitByte(static_cast<OpCode>(byte1 & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(byte2 & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(byte3 & BYTE_MASK), line);
}

void CodeGenerator::EmitNumericConstant(MidoriInteger val, int line, bool is_integer)
{
	int byte1 = val & BYTE_MASK;
	int byte2 = (val >> SHIFT_8_BITS) & BYTE_MASK;
	int byte3 = (val >> SHIFT_16_BITS) & BYTE_MASK;
	int byte4 = (val >> SHIFT_24_BITS) & BYTE_MASK;
	int byte5 = (val >> SHIFT_32_BITS) & BYTE_MASK;
	int byte6 = (val >> SHIFT_40_BITS) & BYTE_MASK;
	int byte7 = (val >> SHIFT_48_BITS) & BYTE_MASK;
	int byte8 = (val >> SHIFT_56_BITS) & BYTE_MASK;

	if (is_integer)
	{
		EmitByte(OpCode::INTEGER_CONSTANT, line);
	}
	else
	{
		EmitByte(OpCode::FLOAT_CONSTANT, line);
	}
	EmitByte(static_cast<OpCode>(byte1), line);
	EmitByte(static_cast<OpCode>(byte2), line);
	EmitByte(static_cast<OpCode>(byte3), line);
	EmitByte(static_cast<OpCode>(byte4), line);
	EmitByte(static_cast<OpCode>(byte5), line);
	EmitByte(static_cast<OpCode>(byte6), line);
	EmitByte(static_cast<OpCode>(byte7), line);
	EmitByte(static_cast<OpCode>(byte8), line);
}

void CodeGenerator::EmitFloatConstant(MidoriFloat value, int line)
{
	MidoriInteger reinterpreted_int = std::bit_cast<MidoriInteger>(value);
	EmitNumericConstant(reinterpreted_int, line, false);
}

void CodeGenerator::EmitIntegerConstant(MidoriInteger value, int line)
{
	// Optimize common small integer constants
	switch (value)
	{
	case -1:
		EmitByte(OpCode::INT_MINUS_1, line);
		return;
	case 0:
		EmitByte(OpCode::INT_0, line);
		return;
	case 1:
		EmitByte(OpCode::INT_1, line);
		return;
	case 2:
		EmitByte(OpCode::INT_2, line);
		return;
	case 3:
		EmitByte(OpCode::INT_3, line);
		return;
	case 4:
		EmitByte(OpCode::INT_4, line);
		return;
	case 5:
		EmitByte(OpCode::INT_5, line);
		return;
	case 10:
		EmitByte(OpCode::INT_10, line);
		return;
	default:
		EmitNumericConstant(value, line, true);
	}
}

void CodeGenerator::EmitByteConstant(MidoriByte value, int line)
{
	EmitByte(OpCode::BYTE_CONSTANT, line);
	EmitByte(static_cast<OpCode>(value), line);
}

void CodeGenerator::EmitWordConstant(MidoriWord value, int line)
{
	EmitByte(OpCode::WORD_CONSTANT, line);
	int byte1 = value & BYTE_MASK;
	int byte2 = (value >> SHIFT_8_BITS) & BYTE_MASK;
	int byte3 = (value >> SHIFT_16_BITS) & BYTE_MASK;
	int byte4 = (value >> SHIFT_24_BITS) & BYTE_MASK;
	int byte5 = (value >> SHIFT_32_BITS) & BYTE_MASK;
	int byte6 = (value >> SHIFT_40_BITS) & BYTE_MASK;
	int byte7 = (value >> SHIFT_48_BITS) & BYTE_MASK;
	int byte8 = (value >> SHIFT_56_BITS) & BYTE_MASK;
	EmitByte(static_cast<OpCode>(byte1), line);
	EmitByte(static_cast<OpCode>(byte2), line);
	EmitByte(static_cast<OpCode>(byte3), line);
	EmitByte(static_cast<OpCode>(byte4), line);
	EmitByte(static_cast<OpCode>(byte5), line);
	EmitByte(static_cast<OpCode>(byte6), line);
	EmitByte(static_cast<OpCode>(byte7), line);
	EmitByte(static_cast<OpCode>(byte8), line);
}

void CodeGenerator::EnsureProcedureMetadataSize(size_t procedure_index)
{
	if (m_shared_cell_procedure_flags.size() <= procedure_index)
	{
		m_shared_cell_procedure_flags.resize(procedure_index + 1u, false);
	}
	if (m_procedure_local_kinds.size() <= procedure_index)
	{
		m_procedure_local_kinds.resize(procedure_index + 1u);
	}
	if (m_procedure_capture_counts.size() <= procedure_index)
	{
		m_procedure_capture_counts.resize(procedure_index + 1u, 0);
	}
}

void CodeGenerator::EnsureLocalKindCapacity(size_t procedure_index, int local_count)
{
	if (local_count <= 0)
	{
		return;
	}

	EnsureProcedureMetadataSize(procedure_index);
	std::vector<LocalStorageKind>& kinds = m_procedure_local_kinds[procedure_index];
	if (static_cast<int>(kinds.size()) < local_count)
	{
		kinds.resize(static_cast<size_t>(local_count), LocalStorageKind::ValueLocal);
	}
}

int CodeGenerator::CurrentProcedureCaptureCount() const
{
	const size_t proc_index = m_builder.m_current_procedure_index;
	if (proc_index >= m_procedure_capture_counts.size())
	{
		return 0;
	}
	return m_procedure_capture_counts[proc_index];
}

void CodeGenerator::NoteCaptureBinding(int captured_count, bool uses_shared_cells)
{
	if (captured_count <= 0)
	{
		return;
	}

	const int parent_capture_count = CurrentProcedureCaptureCount();
	const int local_capture_count = captured_count - parent_capture_count;
	if (local_capture_count <= 0)
	{
		return;
	}

	const size_t proc_index = m_builder.m_current_procedure_index;
	EnsureLocalKindCapacity(proc_index, local_capture_count);
	std::vector<LocalStorageKind>& kinds = m_procedure_local_kinds[proc_index];
	for (int local_index = 0; local_index < local_capture_count; local_index += 1)
	{
		LocalStorageKind& kind = kinds[static_cast<size_t>(local_index)];
		const LocalStorageKind previous_kind = kind;
		if (uses_shared_cells)
		{
			kind = LocalStorageKind::SharedCellLocal;
		}
		else if (kind != LocalStorageKind::SharedCellLocal)
		{
			kind = LocalStorageKind::CellLocal;
		}

		if (kind != previous_kind)
		{
			RewriteEmittedLocalOps(local_index, previous_kind, kind);
		}
	}
}

CodeGenerator::LocalStorageKind CodeGenerator::GetLocalStorageKind(int variable_index) const
{
	if (variable_index < 0)
	{
		return LocalStorageKind::ValueLocal;
	}

	const size_t proc_index = m_builder.m_current_procedure_index;
	if (proc_index >= m_procedure_local_kinds.size())
	{
		return LocalStorageKind::ValueLocal;
	}

	const std::vector<LocalStorageKind>& kinds = m_procedure_local_kinds[proc_index];
	const size_t local_index = static_cast<size_t>(variable_index);
	if (local_index >= kinds.size())
	{
		return LocalStorageKind::ValueLocal;
	}

	return kinds[local_index];
}

void CodeGenerator::RewriteEmittedLocalOps(int variable_index, LocalStorageKind previous_kind, LocalStorageKind new_kind)
{
	if (variable_index < 0 || previous_kind == new_kind)
	{
		return;
	}

	auto map_opcode = [previous_kind, new_kind](OpCode opcode) -> OpCode
	{
		if (previous_kind == LocalStorageKind::ValueLocal)
		{
			if (new_kind == LocalStorageKind::CellLocal)
			{
				switch (opcode)
				{
				case OpCode::GET_LOCAL:
					return OpCode::GET_LOCAL_CELL;
				case OpCode::SET_LOCAL:
					return OpCode::SET_LOCAL_CELL;
				case OpCode::GET_LOCAL_WIDE:
					return OpCode::GET_LOCAL_CELL_WIDE;
				case OpCode::SET_LOCAL_WIDE:
					return OpCode::SET_LOCAL_CELL_WIDE;
				default:
					return opcode;
				}
			}
			else if (new_kind == LocalStorageKind::SharedCellLocal)
			{
				switch (opcode)
				{
				case OpCode::GET_LOCAL:
					return OpCode::GET_LOCAL_SHARED;
				case OpCode::SET_LOCAL:
					return OpCode::SET_LOCAL_SHARED;
				case OpCode::GET_LOCAL_WIDE:
					return OpCode::GET_LOCAL_SHARED_WIDE;
				case OpCode::SET_LOCAL_WIDE:
					return OpCode::SET_LOCAL_SHARED_WIDE;
				default:
					return opcode;
				}
			}
		}
		else if (previous_kind == LocalStorageKind::CellLocal && new_kind == LocalStorageKind::SharedCellLocal)
		{
			switch (opcode)
			{
			case OpCode::GET_LOCAL_CELL:
				return OpCode::GET_LOCAL_SHARED;
			case OpCode::SET_LOCAL_CELL:
				return OpCode::SET_LOCAL_SHARED;
			case OpCode::GET_LOCAL_CELL_WIDE:
				return OpCode::GET_LOCAL_SHARED_WIDE;
			case OpCode::SET_LOCAL_CELL_WIDE:
				return OpCode::SET_LOCAL_SHARED_WIDE;
			default:
				return opcode;
			}
		}

		return opcode;
	};

	BytecodeStream& procedure = m_builder.m_procedures[m_builder.m_current_procedure_index];
	const int target_index = variable_index;

	auto read_u16 = [&procedure](int offset) -> int
	{
		const int high = static_cast<int>(procedure.ReadByteCode(offset + 1));
		const int low = static_cast<int>(procedure.ReadByteCode(offset + 2));
		return (high << 8) | low;
	};

	for (int offset = 0; offset < procedure.GetByteCodeSize();)
	{
		const OpCode opcode = procedure.ReadByteCode(offset);
		int advance = 1;

		switch (opcode)
		{
		case OpCode::GET_LOCAL:
		case OpCode::SET_LOCAL:
		case OpCode::GET_LOCAL_CELL:
		case OpCode::SET_LOCAL_CELL:
		case OpCode::GET_LOCAL_SHARED:
		case OpCode::SET_LOCAL_SHARED:
		{
			const int index = static_cast<int>(procedure.ReadByteCode(offset + 1));
			if (index == target_index)
			{
				procedure.SetByteCode(offset, map_opcode(opcode));
			}
			advance = 2;
			break;
		}
		case OpCode::GET_LOCAL_WIDE:
		case OpCode::SET_LOCAL_WIDE:
		case OpCode::GET_LOCAL_CELL_WIDE:
		case OpCode::SET_LOCAL_CELL_WIDE:
		case OpCode::GET_LOCAL_SHARED_WIDE:
		case OpCode::SET_LOCAL_SHARED_WIDE:
		{
			const int index = read_u16(offset);
			if (index == target_index)
			{
				procedure.SetByteCode(offset, map_opcode(opcode));
			}
			advance = 3;
			break;
		}
		case OpCode::INTEGER_CONSTANT:
		case OpCode::FLOAT_CONSTANT:
		case OpCode::WORD_CONSTANT:
			advance = 9;
			break;
		case OpCode::BYTE_CONSTANT:
			advance = 2;
			break;
		case OpCode::CREATE_ARRAY:
			advance = 4;
			break;
		case OpCode::LOAD_STRING_WIDE:
			advance = 3;
			break;
		case OpCode::JUMP:
		case OpCode::JUMP_IF_FALSE:
		case OpCode::JUMP_IF_TRUE:
		case OpCode::JUMP_BACK:
		case OpCode::BREAK:
		case OpCode::IF_INTEGER_EQUAL:
		case OpCode::IF_INTEGER_NOT_EQUAL:
		case OpCode::IF_INTEGER_GREATER:
		case OpCode::IF_INTEGER_GREATER_EQUAL:
		case OpCode::IF_INTEGER_LESS:
		case OpCode::IF_INTEGER_LESS_EQUAL:
		case OpCode::IF_FLOAT_EQUAL:
		case OpCode::IF_FLOAT_NOT_EQUAL:
		case OpCode::IF_FLOAT_GREATER:
		case OpCode::IF_FLOAT_GREATER_EQUAL:
		case OpCode::IF_FLOAT_LESS:
		case OpCode::IF_FLOAT_LESS_EQUAL:
			advance = 3;
			break;
		case OpCode::MATCH_JUMP_TABLE:
			advance = 2 + (static_cast<int>(procedure.ReadByteCode(offset + 1)) * 2);
			break;
		case OpCode::CALL_FOREIGN:
			advance = 3;
			break;
		case OpCode::CALL_FOREIGN_INDEXED:
			advance = 4;
			break;
		case OpCode::CALL_PROC:
		case OpCode::CALL_GLOBAL:
		case OpCode::CALL_GLOBAL_SHARED:
			advance = 3;
			break;
		case OpCode::CALL_PROC_0:
		case OpCode::CALL_PROC_1:
		case OpCode::CALL_PROC_2:
		case OpCode::CALL_PROC_3:
			advance = 2;
			break;
		case OpCode::CALL_GLOBAL_WIDE:
		case OpCode::CALL_GLOBAL_SHARED_WIDE:
			advance = 4;
			break;
		case OpCode::GET_LOCAL_0:
		case OpCode::GET_LOCAL_1:
		case OpCode::GET_LOCAL_2:
		case OpCode::GET_LOCAL_3:
		case OpCode::SET_LOCAL_0:
		case OpCode::SET_LOCAL_1:
		case OpCode::SET_LOCAL_2:
		case OpCode::SET_LOCAL_3:
			advance = 1;
			break;
		case OpCode::DEFINE_GLOBAL:
		case OpCode::GET_GLOBAL:
		case OpCode::SET_GLOBAL:
		case OpCode::GET_CELL:
		case OpCode::SET_CELL:
		case OpCode::DEFINE_GLOBAL_SHARED:
		case OpCode::GET_GLOBAL_SHARED:
		case OpCode::SET_GLOBAL_SHARED:
		case OpCode::GET_SHARED_CELL:
		case OpCode::SET_SHARED_CELL:
		case OpCode::MAKE_CLOSURE:
		case OpCode::MAKE_FUNCTION:
		case OpCode::LOAD_STRING:
		case OpCode::CALL:
		case OpCode::BIND_CAPTURES:
		case OpCode::BIND_CAPTURES_SHARED:
		case OpCode::GET_MEMBER:
		case OpCode::SET_MEMBER:
		case OpCode::POP_VALUES:
		case OpCode::POP_LOCAL_SCOPE:
		case OpCode::POP_BLOCK_SCOPE:
		case OpCode::POP_MATCH_SCOPE:
		case OpCode::TAIL_CALL:
		case OpCode::GET_ARRAY:
		case OpCode::SET_ARRAY:
		case OpCode::CONSTRUCT_STRUCT:
		case OpCode::CONSTRUCT_UNION:
		case OpCode::SET_TAG:
			advance = 2;
			break;
		case OpCode::DEFINE_GLOBAL_WIDE:
		case OpCode::GET_GLOBAL_WIDE:
		case OpCode::SET_GLOBAL_WIDE:
		case OpCode::GET_CELL_WIDE:
		case OpCode::SET_CELL_WIDE:
		case OpCode::DEFINE_GLOBAL_SHARED_WIDE:
		case OpCode::GET_GLOBAL_SHARED_WIDE:
		case OpCode::SET_GLOBAL_SHARED_WIDE:
		case OpCode::GET_SHARED_CELL_WIDE:
		case OpCode::SET_SHARED_CELL_WIDE:
			advance = 3;
			break;
		default:
			advance = 1;
			break;
		}

		offset += advance;
	}
}

OpCode CodeGenerator::GetLocalLoadOpcode(int variable_index) const
{
	switch (GetLocalStorageKind(variable_index))
	{
	case LocalStorageKind::CellLocal:
		return OpCode::GET_LOCAL_CELL;
	case LocalStorageKind::SharedCellLocal:
		return OpCode::GET_LOCAL_SHARED;
	default:
		return OpCode::GET_LOCAL;
	}
}

OpCode CodeGenerator::GetLocalStoreOpcode(int variable_index) const
{
	switch (GetLocalStorageKind(variable_index))
	{
	case LocalStorageKind::CellLocal:
		return OpCode::SET_LOCAL_CELL;
	case LocalStorageKind::SharedCellLocal:
		return OpCode::SET_LOCAL_SHARED;
	default:
		return OpCode::SET_LOCAL;
	}
}

void CodeGenerator::EmitVariable(int variable_index, OpCode op, int line)
{
	if (op == OpCode::GET_LOCAL)
	{
		op = GetLocalLoadOpcode(variable_index);
	}
	else if (op == OpCode::SET_LOCAL)
	{
		op = GetLocalStoreOpcode(variable_index);
	}

	if (variable_index <= MAX_LOCAL_VARIABLES)
	{
		EmitByte(op, line);
		EmitByte(static_cast<OpCode>(variable_index), line);
		return;
	}

	if (variable_index > MAX_VARIABLES)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many variables (max {})", MAX_VARIABLES), line, m_file_name, m_source_lines));
		return;
	}

	OpCode wide_op = op;
	switch (op)
	{
	case OpCode::DEFINE_GLOBAL:
		wide_op = OpCode::DEFINE_GLOBAL_WIDE;
		break;
	case OpCode::GET_GLOBAL:
		wide_op = OpCode::GET_GLOBAL_WIDE;
		break;
	case OpCode::SET_GLOBAL:
		wide_op = OpCode::SET_GLOBAL_WIDE;
		break;
	case OpCode::GET_LOCAL:
		wide_op = OpCode::GET_LOCAL_WIDE;
		break;
	case OpCode::SET_LOCAL:
		wide_op = OpCode::SET_LOCAL_WIDE;
		break;
	case OpCode::GET_LOCAL_CELL:
		wide_op = OpCode::GET_LOCAL_CELL_WIDE;
		break;
	case OpCode::SET_LOCAL_CELL:
		wide_op = OpCode::SET_LOCAL_CELL_WIDE;
		break;
	case OpCode::GET_LOCAL_SHARED:
		wide_op = OpCode::GET_LOCAL_SHARED_WIDE;
		break;
	case OpCode::SET_LOCAL_SHARED:
		wide_op = OpCode::SET_LOCAL_SHARED_WIDE;
		break;
	case OpCode::GET_CELL:
		wide_op = OpCode::GET_CELL_WIDE;
		break;
	case OpCode::SET_CELL:
		wide_op = OpCode::SET_CELL_WIDE;
		break;
	case OpCode::DEFINE_GLOBAL_SHARED:
		wide_op = OpCode::DEFINE_GLOBAL_SHARED_WIDE;
		break;
	case OpCode::GET_GLOBAL_SHARED:
		wide_op = OpCode::GET_GLOBAL_SHARED_WIDE;
		break;
	case OpCode::SET_GLOBAL_SHARED:
		wide_op = OpCode::SET_GLOBAL_SHARED_WIDE;
		break;
	case OpCode::GET_SHARED_CELL:
		wide_op = OpCode::GET_SHARED_CELL_WIDE;
		break;
	case OpCode::SET_SHARED_CELL:
		wide_op = OpCode::SET_SHARED_CELL_WIDE;
		break;
	default:
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid opcode for wide variable operation", line, m_file_name, m_source_lines));
		return;
	}

	EmitByte(wide_op, line);
	EmitTwoBytes(variable_index >> 8, variable_index & 0xFF, line);
}

OpCode CodeGenerator::GetCellLoadOpcode() const
{
	return CurrentProcedureUsesSharedCells() ? OpCode::GET_SHARED_CELL : OpCode::GET_CELL;
}

OpCode CodeGenerator::GetCellStoreOpcode() const
{
	return CurrentProcedureUsesSharedCells() ? OpCode::SET_SHARED_CELL : OpCode::SET_CELL;
}

bool CodeGenerator::CurrentProcedureUsesSharedCells() const
{
	const size_t index = m_builder.m_current_procedure_index;
	return index < m_shared_cell_procedure_flags.size() && m_shared_cell_procedure_flags[index];
}

void CodeGenerator::RewriteGlobalsForAsyncModule(BytecodeModule& module) const
{
	for (BytecodeStream& procedure : module.m_procedures)
	{
		const int bytecode_size = procedure.GetByteCodeSize();
		for (int offset = 0; offset < bytecode_size;)
		{
			const OpCode opcode = procedure.ReadByteCode(offset);
			int advance = 1;
			switch (opcode)
			{
			case OpCode::INTEGER_CONSTANT:
			case OpCode::FLOAT_CONSTANT:
			case OpCode::WORD_CONSTANT:
				advance = 9;
				break;
			case OpCode::BYTE_CONSTANT:
				advance = 2;
				break;
			case OpCode::CREATE_ARRAY:
				advance = 4;
				break;
			case OpCode::LOAD_STRING_WIDE:
				advance = 3;
				break;
			case OpCode::JUMP:
			case OpCode::JUMP_IF_FALSE:
			case OpCode::JUMP_IF_TRUE:
			case OpCode::JUMP_BACK:
			case OpCode::BREAK:
			case OpCode::IF_INTEGER_EQUAL:
			case OpCode::IF_INTEGER_NOT_EQUAL:
			case OpCode::IF_INTEGER_GREATER:
			case OpCode::IF_INTEGER_GREATER_EQUAL:
			case OpCode::IF_INTEGER_LESS:
			case OpCode::IF_INTEGER_LESS_EQUAL:
			case OpCode::IF_FLOAT_EQUAL:
			case OpCode::IF_FLOAT_NOT_EQUAL:
			case OpCode::IF_FLOAT_GREATER:
			case OpCode::IF_FLOAT_GREATER_EQUAL:
			case OpCode::IF_FLOAT_LESS:
			case OpCode::IF_FLOAT_LESS_EQUAL:
				advance = 3;
				break;
			case OpCode::MATCH_JUMP_TABLE:
				advance = 2 + (static_cast<int>(procedure.ReadByteCode(offset + 1)) * 2);
				break;
			case OpCode::CALL_FOREIGN:
				advance = 3;
				break;
			case OpCode::CALL_FOREIGN_INDEXED:
				advance = 4;
				break;
			case OpCode::CALL_PROC:
				advance = 3;
				break;
			case OpCode::CALL_PROC_0:
			case OpCode::CALL_PROC_1:
			case OpCode::CALL_PROC_2:
			case OpCode::CALL_PROC_3:
				advance = 2;
				break;
			case OpCode::CALL_GLOBAL:
				procedure.SetByteCode(offset, OpCode::CALL_GLOBAL_SHARED);
				advance = 3;
				break;
			case OpCode::CALL_GLOBAL_WIDE:
				procedure.SetByteCode(offset, OpCode::CALL_GLOBAL_SHARED_WIDE);
				advance = 4;
				break;
			case OpCode::DEFINE_GLOBAL_WIDE:
				procedure.SetByteCode(offset, OpCode::DEFINE_GLOBAL_SHARED_WIDE);
				advance = 3;
				break;
			case OpCode::GET_GLOBAL_WIDE:
				procedure.SetByteCode(offset, OpCode::GET_GLOBAL_SHARED_WIDE);
				advance = 3;
				break;
			case OpCode::SET_GLOBAL_WIDE:
				procedure.SetByteCode(offset, OpCode::SET_GLOBAL_SHARED_WIDE);
				advance = 3;
				break;
			case OpCode::GET_LOCAL_WIDE:
			case OpCode::SET_LOCAL_WIDE:
			case OpCode::GET_LOCAL_CELL_WIDE:
			case OpCode::SET_LOCAL_CELL_WIDE:
			case OpCode::GET_LOCAL_SHARED_WIDE:
			case OpCode::SET_LOCAL_SHARED_WIDE:
			case OpCode::GET_CELL_WIDE:
			case OpCode::SET_CELL_WIDE:
			case OpCode::GET_SHARED_CELL_WIDE:
			case OpCode::SET_SHARED_CELL_WIDE:
				advance = 3;
				break;
			case OpCode::DEFINE_GLOBAL:
				procedure.SetByteCode(offset, OpCode::DEFINE_GLOBAL_SHARED);
				advance = 2;
				break;
			case OpCode::GET_GLOBAL:
				procedure.SetByteCode(offset, OpCode::GET_GLOBAL_SHARED);
				advance = 2;
				break;
			case OpCode::SET_GLOBAL:
				procedure.SetByteCode(offset, OpCode::SET_GLOBAL_SHARED);
				advance = 2;
				break;
			case OpCode::CALL_GLOBAL_SHARED:
				advance = 3;
				break;
			case OpCode::CALL_GLOBAL_SHARED_WIDE:
				advance = 4;
				break;
			case OpCode::MAKE_CLOSURE:
			case OpCode::MAKE_FUNCTION:
			case OpCode::LOAD_STRING:
			case OpCode::GET_LOCAL:
			case OpCode::SET_LOCAL:
			case OpCode::GET_LOCAL_CELL:
			case OpCode::SET_LOCAL_CELL:
			case OpCode::GET_LOCAL_SHARED:
			case OpCode::SET_LOCAL_SHARED:
			case OpCode::GET_CELL:
			case OpCode::SET_CELL:
			case OpCode::GET_SHARED_CELL:
			case OpCode::SET_SHARED_CELL:
			case OpCode::CALL:
			case OpCode::BIND_CAPTURES:
			case OpCode::BIND_CAPTURES_SHARED:
			case OpCode::GET_MEMBER:
			case OpCode::SET_MEMBER:
			case OpCode::POP_VALUES:
			case OpCode::POP_LOCAL_SCOPE:
			case OpCode::POP_BLOCK_SCOPE:
			case OpCode::POP_MATCH_SCOPE:
			case OpCode::TAIL_CALL:
			case OpCode::GET_ARRAY:
			case OpCode::SET_ARRAY:
			case OpCode::CONSTRUCT_STRUCT:
			case OpCode::CONSTRUCT_UNION:
			case OpCode::SET_TAG:
				advance = 2;
				break;
			default:
				advance = 1;
				break;
			}

			offset += advance;
		}
	}
}

void CodeGenerator::EmitCall(int arity, int line)
{
	switch (arity)
	{
	case 0:
		EmitByte(OpCode::CALL_0, line);
		return;
	case 1:
		EmitByte(OpCode::CALL_1, line);
		return;
	case 2:
		EmitByte(OpCode::CALL_2, line);
		return;
	case 3:
		EmitByte(OpCode::CALL_3, line);
		return;
	default:
		break;
	}

	EmitByte(OpCode::CALL, line);
	EmitByte(static_cast<OpCode>(arity), line);
}

void CodeGenerator::EmitCallProc(int proc_index, int arity, int line)
{
	switch (arity)
	{
	case 0:
		EmitByte(OpCode::CALL_PROC_0, line);
		EmitByte(static_cast<OpCode>(proc_index), line);
		return;
	case 1:
		EmitByte(OpCode::CALL_PROC_1, line);
		EmitByte(static_cast<OpCode>(proc_index), line);
		return;
	case 2:
		EmitByte(OpCode::CALL_PROC_2, line);
		EmitByte(static_cast<OpCode>(proc_index), line);
		return;
	case 3:
		EmitByte(OpCode::CALL_PROC_3, line);
		EmitByte(static_cast<OpCode>(proc_index), line);
		return;
	default:
		break;
	}

	EmitByte(OpCode::CALL_PROC, line);
	EmitByte(static_cast<OpCode>(proc_index), line);
	EmitByte(static_cast<OpCode>(arity), line);
}

void CodeGenerator::EmitCallGlobal(int global_index, int arity, int line)
{
	if (global_index > MAX_VARIABLES)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many global variables (max {})", MAX_VARIABLES), line, m_file_name, m_source_lines));
		return;
	}

	if (global_index <= MAX_LOCAL_VARIABLES)
	{
		EmitByte(OpCode::CALL_GLOBAL, line);
		EmitByte(static_cast<OpCode>(global_index), line);
		EmitByte(static_cast<OpCode>(arity), line);
		return;
	}

	EmitByte(OpCode::CALL_GLOBAL_WIDE, line);
	EmitTwoBytes(global_index >> 8, global_index & 0xFF, line);
	EmitByte(static_cast<OpCode>(arity), line);
}

bool CodeGenerator::MatchInstanceTypeArg(const std::shared_ptr<MidoriType>& pattern, const std::shared_ptr<MidoriType>& concrete, TypeEnvironment& substitutions, std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash>& visited) const
{
	if (!pattern || !concrete)
	{
		return false;
	}

	std::pair<MidoriType*, MidoriType*> key{ pattern.get(), concrete.get() };
	if (visited.contains(key))
	{
		return true;
	}
	visited.emplace(key);

	if (pattern->IsType<MidoriType::GenericParam>())
	{
		const std::string& param_name = pattern->GetType<MidoriType::GenericParam>().m_name;
		if (substitutions.contains(param_name))
		{
			return *substitutions.at(param_name) == *concrete;
		}
		substitutions.emplace(param_name, concrete);
		return true;
	}

	if (pattern->IsType<MidoriType::ArrayType>())
	{
		if (!concrete->IsType<MidoriType::ArrayType>())
		{
			return false;
		}
		return MatchInstanceTypeArg(pattern->GetType<MidoriType::ArrayType>().m_element_type, concrete->GetType<MidoriType::ArrayType>().m_element_type, substitutions, visited);
	}

	if (pattern->IsType<MidoriType::RangeType>())
	{
		if (!concrete->IsType<MidoriType::RangeType>())
		{
			return false;
		}
		return MatchInstanceTypeArg(pattern->GetType<MidoriType::RangeType>().m_element_type, concrete->GetType<MidoriType::RangeType>().m_element_type, substitutions, visited);
	}

	if (pattern->IsType<MidoriType::FutureType>())
	{
		if (!concrete->IsType<MidoriType::FutureType>())
		{
			return false;
		}
		return MatchInstanceTypeArg(pattern->GetType<MidoriType::FutureType>().m_element_type, concrete->GetType<MidoriType::FutureType>().m_element_type, substitutions, visited);
	}

	if (pattern->IsType<MidoriType::TupleType>())
	{
		if (!concrete->IsType<MidoriType::TupleType>())
		{
			return false;
		}

		const MidoriType::TupleType& pattern_tuple = pattern->GetType<MidoriType::TupleType>();
		const MidoriType::TupleType& concrete_tuple = concrete->GetType<MidoriType::TupleType>();
		if (pattern_tuple.m_element_types.size() != concrete_tuple.m_element_types.size())
		{
			return false;
		}
		for (size_t i = 0u; i < pattern_tuple.m_element_types.size(); i += 1u)
		{
			if (!MatchInstanceTypeArg(pattern_tuple.m_element_types[i], concrete_tuple.m_element_types[i], substitutions, visited))
			{
				return false;
			}
		}
		return true;
	}

	if (pattern->IsType<MidoriType::FunctionType>())
	{
		if (!concrete->IsType<MidoriType::FunctionType>())
		{
			return false;
		}

		const MidoriType::FunctionType& pattern_func = pattern->GetType<MidoriType::FunctionType>();
		const MidoriType::FunctionType& concrete_func = concrete->GetType<MidoriType::FunctionType>();
		if (pattern_func.m_param_types.size() != concrete_func.m_param_types.size())
		{
			return false;
		}
		for (size_t i = 0u; i < pattern_func.m_param_types.size(); i += 1u)
		{
			if (!MatchInstanceTypeArg(pattern_func.m_param_types[i], concrete_func.m_param_types[i], substitutions, visited))
			{
				return false;
			}
		}
		return MatchInstanceTypeArg(pattern_func.m_return_type, concrete_func.m_return_type, substitutions, visited);
	}

	if (pattern->IsType<MidoriType::StructType>())
	{
		if (!concrete->IsType<MidoriType::StructType>())
		{
			return false;
		}

		const MidoriType::StructType& pattern_struct = pattern->GetType<MidoriType::StructType>();
		const MidoriType::StructType& concrete_struct = concrete->GetType<MidoriType::StructType>();
		if (pattern_struct.m_name != concrete_struct.m_name ||
			pattern_struct.m_member_types.size() != concrete_struct.m_member_types.size())
		{
			return false;
		}

		for (size_t i = 0u; i < pattern_struct.m_member_types.size(); i += 1u)
		{
			if (!MatchInstanceTypeArg(pattern_struct.m_member_types[i], concrete_struct.m_member_types[i], substitutions, visited))
			{
				return false;
			}
		}
		return true;
	}

	if (pattern->IsType<MidoriType::UnionType>())
	{
		if (!concrete->IsType<MidoriType::UnionType>())
		{
			return false;
		}

		const MidoriType::UnionType& pattern_union = pattern->GetType<MidoriType::UnionType>();
		const MidoriType::UnionType& concrete_union = concrete->GetType<MidoriType::UnionType>();
		if (pattern_union.m_name != concrete_union.m_name ||
			pattern_union.m_member_info.size() != concrete_union.m_member_info.size())
		{
			return false;
		}

		for (const auto& [member_name, pattern_ctx] : pattern_union.m_member_info)
		{
			std::unordered_map<std::string, MidoriType::UnionType::UnionMemberContext>::const_iterator concrete_it = concrete_union.m_member_info.find(member_name);
			if (concrete_it == concrete_union.m_member_info.end())
			{
				return false;
			}
			const MidoriType::UnionType::UnionMemberContext& concrete_ctx = concrete_it->second;
			if (pattern_ctx.m_member_types.size() != concrete_ctx.m_member_types.size())
			{
				return false;
			}
			for (size_t i = 0u; i < pattern_ctx.m_member_types.size(); i += 1u)
			{
				if (!MatchInstanceTypeArg(pattern_ctx.m_member_types[i], concrete_ctx.m_member_types[i], substitutions, visited))
				{
					return false;
				}
			}
		}
		return true;
	}

	return *pattern == *concrete;
}

bool CodeGenerator::EmitIterableNextCall(const std::shared_ptr<MidoriType>& iter_type, const std::shared_ptr<MidoriType>& item_type, int line)
{
	if (!iter_type || !item_type)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Iterable iteration missing type information", line, m_file_name, m_source_lines));
		return false;
	}

	std::string qualified_method_name = std::string(ITERABLE_CLASS_NAME) + std::string(NameSeparator) + std::string(NEXT_METHOD_NAME);
	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(qualified_method_name);

	if (resolution_it != m_method_resolution_map.end())
	{
		std::string iter_name = iter_type->ToString();
		std::string item_name = item_type->ToString();
		for (const ResolvedMethodCandidate& candidate : resolution_it->second)
		{
			if (candidate.m_first_type_name == iter_name &&
			    (candidate.m_second_type_name.empty() || candidate.m_second_type_name == item_name))
			{
				if (!candidate.m_has_instance)
				{
					AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unresolved Iterable::Next instance for iterator type '"s + iter_name + "'", line, m_file_name, m_source_lines));
					return false;
				}

				if (EmitResolvedNameGetGlobal(candidate.m_resolved_name, line))
				{
					EmitCall(1, line);
					return true;
				}

				return false;
			}
		}
	}

	if (!iter_type->IsType<MidoriType::TypeVariable>() && !item_type->IsType<MidoriType::TypeVariable>())
	{
		std::vector<std::shared_ptr<MidoriType>> type_args;
		type_args.emplace_back(iter_type);
		type_args.emplace_back(item_type);
		std::string mangled_name = MidoriType::MangleInstanceMethodName(std::string(NEXT_METHOD_NAME), std::string(ITERABLE_CLASS_NAME), type_args);

		std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
		if (it != m_global_variables.end())
		{
			EmitVariable(it->second, OpCode::GET_GLOBAL, line);
			EmitCall(1, line);
			return true;
		}

		std::optional<std::string> resolved_name;
		TypeclassInstanceTypeMap::iterator instance_args_it = m_class_instance_type_args.find(std::string(ITERABLE_CLASS_NAME));
		if (instance_args_it != m_class_instance_type_args.end())
		{
			for (const std::vector<std::shared_ptr<MidoriType>>& candidate_args : instance_args_it->second)
			{
				if (candidate_args.size() != 2u)
				{
					continue;
				}

				TypeEnvironment substitutions;
				std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash> visited;
				if (!MatchInstanceTypeArg(candidate_args[0u], iter_type, substitutions, visited))
				{
					continue;
				}
				if (!MatchInstanceTypeArg(candidate_args[1u], item_type, substitutions, visited))
				{
					continue;
				}

				std::string candidate_base = MidoriType::MangleInstanceMethodName(std::string(NEXT_METHOD_NAME), std::string(ITERABLE_CLASS_NAME), candidate_args);
				std::optional<std::string> candidate_name = ResolveInstanceName(std::string(ITERABLE_CLASS_NAME), candidate_base);
				if (!candidate_name.has_value())
				{
					continue;
				}

				if (resolved_name.has_value() && resolved_name.value() != candidate_name.value())
				{
					AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Iterable instance method resolution is ambiguous for iterator type '"s + iter_type->ToString() + "'"s, line, m_file_name, m_source_lines));
					return false;
				}

				resolved_name = std::move(candidate_name);
			}
		}

		if (resolved_name.has_value())
		{
			if (EmitResolvedNameGetGlobal(resolved_name.value(), line))
			{
				EmitCall(1, line);
				return true;
			}

			return false;
		}

		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Iterable instance method '"s + mangled_name + "' not found"s, line, m_file_name, m_source_lines));
		return false;
	}

	AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Cannot resolve Iterable instance for type variables outside of specialization context"s, line, m_file_name, m_source_lines));
	return false;
}

std::optional<std::string> CodeGenerator::ResolveInstanceName(const std::string& class_name, const std::string& base_name) const
{
	if (m_global_variables.contains(base_name))
	{
		return base_name;
	}

	TypeclassInstanceMap::const_iterator instances_it = m_class_instances.find(class_name);
	if (instances_it == m_class_instances.end())
	{
		return std::nullopt;
	}

	std::string pattern_with_at = base_name + ModuleSeparator;
	for (const std::string& instance_method : instances_it->second)
	{
		if (instance_method == base_name || instance_method.starts_with(pattern_with_at))
		{
			return instance_method;
		}
	}

	return std::nullopt;
}

int CodeGenerator::GetImportPlaceholder(const std::string& module_name, const std::string& symbol_name, int line)
{
	int import_slot = -1;
	for (size_t i = 0u; i < m_tracked_imports.size(); i += 1u)
	{
		if (m_tracked_imports[i].m_from_module == module_name && m_tracked_imports[i].m_name == symbol_name)
		{
			import_slot = static_cast<int>(i);
			break;
		}
	}

	if (import_slot < 0)
	{
		if (m_tracked_imports.size() >= static_cast<size_t>(MAX_IMPORT_PLACEHOLDERS))
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many imports (max {})", MAX_IMPORT_PLACEHOLDERS), line, m_file_name, m_source_lines));
			return -1;
		}

		m_tracked_imports.emplace_back(symbol_name, module_name);
		import_slot = static_cast<int>(m_tracked_imports.size() - 1u);
	}

	return IMPORT_PLACEHOLDER_BASE + import_slot;
}

int CodeGenerator::EmitJump(OpCode op, int line)
{
	EmitByte(op, line);
	EmitByte(static_cast<OpCode>(BYTE_MASK), line);
	EmitByte(static_cast<OpCode>(BYTE_MASK), line);
	return m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize() - 2;
}

void CodeGenerator::PatchJump(int offset, int line)
{
	int jump = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize() - offset - 2;
	if (jump > MAX_JUMP_SIZE)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too much code to jump over (max {})", MAX_JUMP_SIZE + 1), line, m_file_name, m_source_lines));
		return;
	}

	m_builder.m_procedures[m_builder.m_current_procedure_index].SetByteCode(offset, static_cast<OpCode>(jump & BYTE_MASK));
	m_builder.m_procedures[m_builder.m_current_procedure_index].SetByteCode(offset + 1, static_cast<OpCode>((jump >> SHIFT_8_BITS) & BYTE_MASK));
}

void CodeGenerator::EmitLoop(int loop_start, int line)
{
	EmitByte(OpCode::JUMP_BACK, line);

	int offset = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize() - loop_start + 2;
	if (offset > MAX_JUMP_SIZE)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Loop body too large (max {})", MAX_JUMP_SIZE + 1), line, m_file_name, m_source_lines));
		return;
	}

	EmitByte(static_cast<OpCode>(offset & BYTE_MASK), line);
	EmitByte(static_cast<OpCode>((offset >> SHIFT_8_BITS) & BYTE_MASK), line);
}

void CodeGenerator::EmitEquatableEquals(const std::shared_ptr<MidoriType>& operand_type, int line)
{
	std::string mangled_name = INTERNAL_NAME_PREFIX + std::string(EQUALS_MANGLED_PREFIX) + operand_type->ToString();
	std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
	if (it != m_global_variables.end())
	{
		EmitVariable(it->second, OpCode::GET_GLOBAL, line);
		EmitCall(2, line);
	}
	else
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Equatable instance method '"s + mangled_name + "' not found"s, line, m_file_name, m_source_lines));
	}
}

void CodeGenerator::EmitOrderableCompare(const std::shared_ptr<MidoriType>& operand_type, int line)
{
	std::string mangled_name = INTERNAL_NAME_PREFIX + std::string(COMPARE_MANGLED_PREFIX) + operand_type->ToString();
	std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
	if (it != m_global_variables.end())
	{
		EmitVariable(it->second, OpCode::GET_GLOBAL, line);
		EmitCall(2, line);
	}
	else
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Orderable instance method '"s + mangled_name + "' not found"s, line, m_file_name, m_source_lines));
	}
}

void CodeGenerator::EmitPopCount(int count, int line)
{
	while (count > 0)
	{
		int chunk = std::min(count, static_cast<int>(UINT8_MAX));
		if (chunk == 1)
		{
			EmitByte(OpCode::POP, line);
		}
		else
		{
			EmitByte(OpCode::POP_VALUES, line);
			EmitByte(static_cast<OpCode>(chunk), line);
		}
		count -= chunk;
	}
}

bool CodeGenerator::AreTypeArgsEqual(const std::vector<std::shared_ptr<MidoriType>>& left, const std::vector<std::shared_ptr<MidoriType>>& right) const
{
	if (left.size() != right.size())
	{
		return false;
	}
	for (size_t i = 0u; i < left.size(); i += 1u)
	{
		if (*left[i] != *right[i])
		{
			return false;
		}
	}
	return true;
}

void CodeGenerator::AddInstanceTypeArgs(const std::string& class_name, const std::vector<std::shared_ptr<MidoriType>>& type_args)
{
	std::vector<std::vector<std::shared_ptr<MidoriType>>>& existing_args = m_class_instance_type_args[class_name];
	const bool already_present = std::ranges::any_of
	(
		existing_args,
		[this, &type_args](const std::vector<std::shared_ptr<MidoriType>>& candidate)
		{
			return AreTypeArgsEqual(candidate, type_args);
		}
	);

	if (!already_present)
	{
		existing_args.push_back(type_args);
	}
}

void CodeGenerator::EmitInstanceMethodDefinitions()
{
	std::vector<std::unique_ptr<MidoriStatement>> rewritten;
	rewritten.reserve(m_program_tree.size());

	for (std::unique_ptr<MidoriStatement>& statement : m_program_tree)
	{
		if (!statement->IsStatement<MidoriStatement::Instance>())
		{
			rewritten.emplace_back(std::move(statement));
			continue;
		}

		MidoriStatement::Instance& instance_stmt = statement->GetStatement<MidoriStatement::Instance>();
		AddInstanceTypeArgs(instance_stmt.m_class_name.m_lexeme, instance_stmt.m_type_args);

		for (std::unique_ptr<MidoriStatement>& method : instance_stmt.m_methods)
		{
			if (!method->IsStatement<MidoriStatement::FunctionDefinition>())
			{
				continue;
			}

			MidoriStatement::FunctionDefinition& defun = method->GetStatement<MidoriStatement::FunctionDefinition>();
			std::vector<std::string>& instance_methods = m_class_instances[instance_stmt.m_class_name.m_lexeme];
			if (std::ranges::find(instance_methods, defun.m_name.m_lexeme) == instance_methods.cend())
			{
				instance_methods.emplace_back(defun.m_name.m_lexeme);
			}

			rewritten.emplace_back(std::move(method));
		}
		instance_stmt.m_methods.clear();
		rewritten.emplace_back(std::move(statement));
	}

	m_program_tree = std::move(rewritten);
}

int CodeGenerator::CountPatternBindings(const MidoriPattern& pattern) const
{
	if (pattern.IsPattern<MidoriPattern::Binding>())
	{
		return 1;
	}
	if (pattern.IsPattern<MidoriPattern::Literal>())
	{
		return 0;
	}
	if (pattern.IsPattern<MidoriPattern::Tuple>())
	{
		int total = 0;
		const MidoriPattern::Tuple& tuple = pattern.GetPattern<MidoriPattern::Tuple>();
		for (const std::unique_ptr<MidoriPattern>& elem : tuple.m_elements)
		{
			total += CountPatternBindings(*elem);
		}
		return total;
	}
	if (pattern.IsPattern<MidoriPattern::Array>())
	{
		int total = 0;
		const MidoriPattern::Array& array = pattern.GetPattern<MidoriPattern::Array>();
		for (const std::unique_ptr<MidoriPattern>& elem : array.m_elements)
		{
			total += CountPatternBindings(*elem);
		}
		return total;
	}
	if (pattern.IsPattern<MidoriPattern::Constructor>())
	{
		int total = 0;
		const MidoriPattern::Constructor& ctor = pattern.GetPattern<MidoriPattern::Constructor>();
		for (const std::unique_ptr<MidoriPattern>& arg : ctor.m_args)
		{
			total += CountPatternBindings(*arg);
		}
		return total;
	}

	return 0;
}

void CodeGenerator::EmitPatternLiteralConstant(const MidoriPattern::Literal& literal)
{
	int line = literal.m_token.m_line;
	const std::string& lexeme = literal.m_token.m_lexeme;
	try
	{
		switch (literal.m_kind)
		{
		case MidoriPattern::LiteralKind::Bool:
			EmitByte(lexeme == "true"s ? OpCode::OP_TRUE : OpCode::OP_FALSE, line);
			break;
		case MidoriPattern::LiteralKind::Float:
			EmitFloatConstant(std::stod(lexeme), line);
			break;
		case MidoriPattern::LiteralKind::Integer:
			EmitIntegerConstant(std::stoll(lexeme), line);
			break;
		case MidoriPattern::LiteralKind::Byte:
		{
			uint64_t value = 0u;
			if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X'))
			{
				value = std::stoull(lexeme, nullptr, 16);
			}
			else if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'b' || lexeme[1u] == 'B'))
			{
				value = std::stoull(lexeme, nullptr, 2);
			}
			else
			{
				value = std::stoull(lexeme);
			}
			EmitByteConstant(static_cast<MidoriByte>(value), line);
			break;
		}
		case MidoriPattern::LiteralKind::Word:
		{
			uint64_t value = 0u;
			if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X'))
			{
				value = std::stoull(lexeme, nullptr, 16);
			}
			else if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'b' || lexeme[1u] == 'B'))
			{
				value = std::stoull(lexeme, nullptr, 2);
			}
			else
			{
				value = std::stoull(lexeme);
			}
			EmitWordConstant(value, line);
			break;
		}
		case MidoriPattern::LiteralKind::Text:
			EmitTextConstant(lexeme, line);
			break;
		case MidoriPattern::LiteralKind::Unit:
			EmitByte(OpCode::OP_UNIT, line);
			break;
		}
	}
	catch (const std::exception&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid literal in pattern '" + lexeme + "'", literal.m_token, m_file_name, m_source_lines));
	}
}

void CodeGenerator::EmitPatternLiteralEquals(const MidoriPattern::Literal& literal, int line)
{
	switch (literal.m_kind)
	{
	case MidoriPattern::LiteralKind::Float:
		EmitByte(OpCode::EQUAL_FLOAT, line);
		break;
	case MidoriPattern::LiteralKind::Byte:
		EmitByte(OpCode::EQUAL_BYTE, line);
		break;
	case MidoriPattern::LiteralKind::Word:
		EmitByte(OpCode::EQUAL_WORD, line);
		break;
	case MidoriPattern::LiteralKind::Text:
		EmitByte(OpCode::EQUAL_TEXT, line);
		break;
	case MidoriPattern::LiteralKind::Unit:
		// Unit patterns are irrefutable once types match
		break;
	default:
		EmitByte(OpCode::EQUAL_INTEGER, line);
		break;
	}
}

void CodeGenerator::EmitPatternCheck(const MidoriPattern& pattern, std::vector<int>& failure_jumps, int extra_pops)
{
	struct PatternCheckVisitor
	{
		CodeGenerator* m_self = nullptr;
		std::vector<int>* m_failure_jumps = nullptr;
		int m_extra_pops = 0;

		void operator()(const MidoriPattern::Binding& node) const
		{
			m_self->EmitByte(OpCode::POP, node.m_name.m_line);
		}

		void operator()(const MidoriPattern::Literal& node) const
		{
			const int line = node.m_token.m_line;
			if (node.m_kind == MidoriPattern::LiteralKind::Unit)
			{
				m_self->EmitByte(OpCode::POP, line);
				m_self->EmitByte(OpCode::OP_TRUE, line);
			}
			else
			{
				m_self->EmitPatternLiteralConstant(node);
				m_self->EmitPatternLiteralEquals(node, line);
			}

			int jump_if_false = m_self->EmitJump(OpCode::JUMP_IF_FALSE, line);
			m_self->EmitByte(OpCode::POP, line);
			int jump_over_failure = m_self->EmitJump(OpCode::JUMP, line);
			m_self->PatchJump(jump_if_false, line);
			m_self->EmitByte(OpCode::POP, line);
			m_self->EmitPopCount(m_extra_pops, line);
			m_failure_jumps->emplace_back(m_self->EmitJump(OpCode::JUMP, line));
			m_self->PatchJump(jump_over_failure, line);
		}

		void operator()(const MidoriPattern::Tuple& node) const
		{
			const int line = node.m_left_paren.m_line;
			for (int i = static_cast<int>(node.m_elements.size()) - 1; i >= 0; i -= 1)
			{
				m_self->EmitByte(OpCode::DUP, line);
				m_self->EmitIntegerConstant(static_cast<MidoriInteger>(i), line);
				m_self->EmitByte(OpCode::GET_ARRAY, line);
				m_self->EmitByte(static_cast<OpCode>(1), line);
				m_self->EmitPatternCheck(*node.m_elements[static_cast<size_t>(i)], *m_failure_jumps, m_extra_pops + 1);
			}
			m_self->EmitByte(OpCode::POP, line);
		}

		void operator()(const MidoriPattern::Array& node) const
		{
			const int line = node.m_left_bracket.m_line;
			m_self->EmitByte(OpCode::DUP, line);
			m_self->EmitByte(OpCode::GET_ARRAY_LENGTH, line);
			m_self->EmitIntegerConstant(static_cast<MidoriInteger>(node.m_elements.size()), line);
			m_self->EmitByte(OpCode::EQUAL_INTEGER, line);
			int length_fail = m_self->EmitJump(OpCode::JUMP_IF_FALSE, line);
			m_self->EmitByte(OpCode::POP, line);

			for (int i = static_cast<int>(node.m_elements.size()) - 1; i >= 0; i -= 1)
			{
				m_self->EmitByte(OpCode::DUP, line);
				m_self->EmitIntegerConstant(static_cast<MidoriInteger>(i), line);
				m_self->EmitByte(OpCode::GET_ARRAY, line);
				m_self->EmitByte(static_cast<OpCode>(1), line);
				m_self->EmitPatternCheck(*node.m_elements[static_cast<size_t>(i)], *m_failure_jumps, m_extra_pops + 1);
			}
			m_self->EmitByte(OpCode::POP, line);
			int jump_over_failure = m_self->EmitJump(OpCode::JUMP, line);
			m_self->PatchJump(length_fail, line);
			m_self->EmitByte(OpCode::POP, line);
			m_self->EmitPopCount(m_extra_pops + 1, line);
			m_failure_jumps->emplace_back(m_self->EmitJump(OpCode::JUMP, line));
			m_self->PatchJump(jump_over_failure, line);
		}

		void operator()(const MidoriPattern::Constructor& node) const
		{
			const int line = node.m_name_token.m_line;
			if (node.m_is_union)
			{
				m_self->EmitByte(OpCode::DUP, line);
				m_self->EmitByte(OpCode::GET_TAG, line);
				m_self->EmitIntegerConstant(static_cast<MidoriInteger>(node.m_tag), line);
				m_self->EmitByte(OpCode::EQUAL_INTEGER, line);
				int tag_fail = m_self->EmitJump(OpCode::JUMP_IF_FALSE, line);
				m_self->EmitByte(OpCode::POP, line);

				m_self->EmitByte(OpCode::LOAD_TAG, line);
				m_self->EmitByte(OpCode::POP, line);

				for (int i = static_cast<int>(node.m_args.size()) - 1; i >= 0; i -= 1)
				{
					m_self->EmitPatternCheck(*node.m_args[static_cast<size_t>(i)], *m_failure_jumps, m_extra_pops + i);
				}

				int jump_over_failure = m_self->EmitJump(OpCode::JUMP, line);
				m_self->PatchJump(tag_fail, line);
				m_self->EmitByte(OpCode::POP, line);
				m_self->EmitByte(OpCode::POP, line);
				m_self->EmitPopCount(m_extra_pops, line);
				m_failure_jumps->emplace_back(m_self->EmitJump(OpCode::JUMP, line));
				m_self->PatchJump(jump_over_failure, line);
			}
			else
			{
				for (int i = static_cast<int>(node.m_args.size()) - 1; i >= 0; i -= 1)
				{
					m_self->EmitByte(OpCode::DUP, line);
					m_self->EmitByte(OpCode::GET_MEMBER, line);
					m_self->EmitByte(static_cast<OpCode>(i), line);
					m_self->EmitPatternCheck(*node.m_args[static_cast<size_t>(i)], *m_failure_jumps, m_extra_pops + 1);
				}
				m_self->EmitByte(OpCode::POP, line);
			}
		}
	};

	std::visit(PatternCheckVisitor{ this, &failure_jumps, extra_pops }, *pattern);
}

void CodeGenerator::EmitPatternBind(const MidoriPattern& pattern)
{
	struct PatternBindVisitor
	{
		CodeGenerator* m_self = nullptr;

		void operator()(const MidoriPattern::Binding& node) const
		{
			const int line = node.m_name.m_line;
			if (node.m_local_index.has_value())
			{
				m_self->EmitVariable(node.m_local_index.value(), OpCode::SET_LOCAL, line);
				m_self->EmitByte(OpCode::POP, line);
			}
			else
			{
				m_self->EmitByte(OpCode::POP, line);
			}
		}

		void operator()(const MidoriPattern::Literal& node) const
		{
			m_self->EmitByte(OpCode::POP, node.m_token.m_line);
		}

		void operator()(const MidoriPattern::Tuple& node) const
		{
			const int line = node.m_left_paren.m_line;
			for (int i = static_cast<int>(node.m_elements.size()) - 1; i >= 0; i -= 1)
			{
				m_self->EmitByte(OpCode::DUP, line);
				m_self->EmitIntegerConstant(static_cast<MidoriInteger>(i), line);
				m_self->EmitByte(OpCode::GET_ARRAY, line);
				m_self->EmitByte(static_cast<OpCode>(1), line);
				m_self->EmitPatternBind(*node.m_elements[static_cast<size_t>(i)]);
			}
			m_self->EmitByte(OpCode::POP, line);
		}

		void operator()(const MidoriPattern::Array& node) const
		{
			const int line = node.m_left_bracket.m_line;
			for (int i = static_cast<int>(node.m_elements.size()) - 1; i >= 0; i -= 1)
			{
				m_self->EmitByte(OpCode::DUP, line);
				m_self->EmitIntegerConstant(static_cast<MidoriInteger>(i), line);
				m_self->EmitByte(OpCode::GET_ARRAY, line);
				m_self->EmitByte(static_cast<OpCode>(1), line);
				m_self->EmitPatternBind(*node.m_elements[static_cast<size_t>(i)]);
			}
			m_self->EmitByte(OpCode::POP, line);
		}

		void operator()(const MidoriPattern::Constructor& node) const
		{
			const int line = node.m_name_token.m_line;
			if (node.m_is_union)
			{
				m_self->EmitByte(OpCode::LOAD_TAG, line);
				m_self->EmitByte(OpCode::POP, line);
				for (int i = static_cast<int>(node.m_args.size()) - 1; i >= 0; i -= 1)
				{
					m_self->EmitPatternBind(*node.m_args[static_cast<size_t>(i)]);
				}
			}
			else
			{
				for (int i = static_cast<int>(node.m_args.size()) - 1; i >= 0; i -= 1)
				{
					m_self->EmitByte(OpCode::DUP, line);
					m_self->EmitByte(OpCode::GET_MEMBER, line);
					m_self->EmitByte(static_cast<OpCode>(i), line);
					m_self->EmitPatternBind(*node.m_args[static_cast<size_t>(i)]);
				}
				m_self->EmitByte(OpCode::POP, line);
			}
		}
	};

	std::visit(PatternBindVisitor{ this }, *pattern);
}

void CodeGenerator::BeginLoop(int loop_start)
{
	m_loop_contexts.emplace(std::vector<int>(), loop_start, loop_start);
}

void CodeGenerator::EndLoop(int line)
{
	LoopContext loop = m_loop_contexts.top();
	m_loop_contexts.pop();
	std::ranges::for_each
	(
		loop.m_break_positions,
		[line, this](int break_position){ PatchJump(break_position, line); }
	);
}

void CodeGenerator::Visit(const std::unique_ptr<MidoriStatement>& statement)
{
	DispatchStatement(*statement);
}

void CodeGenerator::Visit(const std::unique_ptr<MidoriExpression>& expression)
{
	DispatchExpression(*expression);
}

void CodeGenerator::Visit(const std::shared_ptr<MidoriExpression>& expression)
{
	DispatchExpression(*expression);
}

void CodeGenerator::DispatchStatement(MidoriStatement& statement)
{
	struct StatementDispatcher
	{
		CodeGenerator* m_self = nullptr;

		void operator()(MidoriStatement::ExpressionStatement& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::VariableDefinition& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::TupleDefinition& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::FunctionDefinition& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::Continue& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::ForeignDefinition& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::Struct& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::Union& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::Class& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::Instance& arg) const { (*m_self)(arg); }
		void operator()(MidoriStatement::TypeAlias& arg) const { (*m_self)(arg); }
	};

	std::visit(StatementDispatcher{ this }, *statement);
}

void CodeGenerator::DispatchExpression(MidoriExpression& expression)
{
	struct ExpressionDispatcher
	{
		CodeGenerator* m_self = nullptr;

		void operator()(MidoriExpression::As& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Binary& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Group& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Tuple& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::TextLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::BoolLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::FloatLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::IntegerLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::ByteLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::WordLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::UnitLiteral& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::UnaryPrefix& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::UnarySuffix& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Assignment& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::AppendAssign& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::ExtendAssign& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::PrependAssign& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::CompoundAssign& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::NameAccess& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Call& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Function& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Construct& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::IfElse& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::MemberAccess& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::MemberAssignment& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Array& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::IndexAccess& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::IndexAssignment& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::ArrayComprehension& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::RangeBinary& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::RangeTernary& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Block& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Match& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Case& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Default& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Loop& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::For& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Return& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Break& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Async& arg) const { (*m_self)(arg); }
		void operator()(MidoriExpression::Await& arg) const { (*m_self)(arg); }
	};

	std::visit(ExpressionDispatcher{ this }, *expression);
}

CodeGenerator::CodeGenerator(MidoriProgramTree&& program_tree, std::string_view file_name, const std::vector<std::string>& source_lines, std::string module_name, std::unordered_set<std::string> export_symbols, const TypeclassMethodMap& imported_class_methods, const TypeclassInstanceMap& imported_class_instances, const TypeclassInstanceTypeMap& imported_class_instance_type_args, const std::unordered_map<std::string, GenericFunctionInfo>& imported_generic_functions)
	: m_program_tree(std::move(program_tree)),
	m_file_name(file_name),
	m_source_lines(source_lines),
	m_module_name(std::move(module_name)),
	m_export_symbols(std::move(export_symbols)),
	m_generic_functions(imported_generic_functions),
	m_class_methods(imported_class_methods),
	m_class_instances(imported_class_instances),
	m_class_instance_type_args(imported_class_instance_type_args)
{
	std::string main_proc_name = std::string(MAIN_PROCEDURE_PREFIX) + "@"s + (m_module_name.has_value() ? m_module_name.value() : std::string(file_name));
	m_builder.m_procedure_names.emplace_back(main_proc_name.c_str());
}

MidoriResult::CodeGeneratorResult CodeGenerator::GenerateModuleBytecode() &
{
	return std::move(*this).GenerateModuleBytecode();
}

MidoriResult::CodeGeneratorResult CodeGenerator::GenerateModuleBytecode() &&
{
	EmitInstanceMethodDefinitions();
	EnsureProcedureMetadataSize(0u);
	m_shared_cell_procedure_flags[0u] = ProgramContainsAsyncOrAwait(m_program_tree);

	struct ExportTracker
	{
		CodeGenerator* m_self = nullptr;

		void operator()(const MidoriStatement::FunctionDefinition& stmt) const
		{
			const std::string& function_name = stmt.m_name.m_lexeme;
			if (m_self->m_export_symbols.contains(function_name))
			{
				const size_t procedure_index = m_self->m_builder.m_procedures.size() - 1u;
				const size_t global_index = static_cast<size_t>(m_self->m_global_variables[function_name]);

				m_self->m_tracked_exports.emplace_back(function_name, procedure_index, global_index, BytecodeModule::SymbolType::FUNCTION);
			}
		}

		void operator()(const MidoriStatement::Struct& stmt) const
		{
			const std::string& struct_name = stmt.m_name.m_lexeme;
			if (m_self->m_export_symbols.contains(struct_name))
			{
				m_self->m_tracked_exports.emplace_back
				(
					struct_name,
					0uz,
					0uz,
					BytecodeModule::SymbolType::STRUCT_TYPE
				);
			}
		}

		void operator()(const MidoriStatement::Union& stmt) const
		{
			const std::string& union_name = stmt.m_name.m_lexeme;
			if (m_self->m_export_symbols.contains(union_name))
			{
				m_self->m_tracked_exports.emplace_back
				(
					union_name,
					0uz,
					0uz,
					BytecodeModule::SymbolType::UNION_TYPE
				);
			}
		}

		void operator()(const MidoriStatement::ForeignDefinition& stmt) const
		{
			const std::string& foreign_name = stmt.m_function_name.m_lexeme;
			if (m_self->m_export_symbols.contains(foreign_name))
			{
				const size_t global_index = static_cast<size_t>(m_self->m_global_variables[foreign_name]);
				m_self->m_tracked_exports.emplace_back
				(
					foreign_name,
					0uz,
					global_index,
					BytecodeModule::SymbolType::FOREIGN_FUNCTION
				);
			}
		}

		void operator()(const MidoriStatement::VariableDefinition& stmt) const
		{
			const std::string& var_name = stmt.m_name.m_lexeme;
			if (m_self->m_export_symbols.contains(var_name))
			{
				const size_t global_index = static_cast<size_t>(m_self->m_global_variables[var_name]);
				m_self->m_tracked_exports.emplace_back
				(
					var_name,
					0uz,
					global_index,
					BytecodeModule::SymbolType::GLOBAL_VARIABLE
				);
			}
		}

		void operator()(const MidoriStatement::ExpressionStatement&) const {}
		void operator()(const MidoriStatement::TupleDefinition&) const {}
		void operator()(const MidoriStatement::Continue&) const {}
		void operator()(const MidoriStatement::Class&) const {}
		void operator()(const MidoriStatement::Instance&) const {}
		void operator()(const MidoriStatement::TypeAlias&) const {}
	};

	std::ranges::for_each
	(
		m_program_tree,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			Visit(statement);

			// Track exports: after processing DefineFunction, check if it's exported
			std::visit(ExportTracker{ this }, **statement);
		}
	);

	// Add RETURN to end the global procedure (procedure 0)
	// This ensures the instruction pointer doesn't run past the end of the procedure
	// Global procedures return Unit
	EmitByte(OpCode::OP_UNIT, 0);
	EmitByte(OpCode::RETURN, 0);

	if (!m_errors.empty())
	{
		return std::unexpected(std::move(m_errors));
	}

	BytecodeModule module(m_module_name.value_or(""s), std::filesystem::path(m_file_name));
	module.m_procedures = std::move(m_builder.m_procedures);
	module.m_procedure_names = std::move(m_builder.m_procedure_names);
	module.m_string_pool = std::move(m_builder.m_string_pool);
	module.m_exports = std::move(m_tracked_exports);
	module.m_imports = std::move(m_tracked_imports);
	module.m_generic_functions = std::move(m_generic_functions);
	module.m_has_async = m_has_async;

	std::vector<std::pair<std::string, int>> sorted_globals(m_global_variables.begin(), m_global_variables.end());
	std::ranges::sort(sorted_globals, [](const std::pair<std::string, int>& a, const std::pair<std::string, int>& b) { return a.second < b.second; });

	module.m_global_variables.reserve(sorted_globals.size());
	for (const std::pair<std::string, int>& entry : sorted_globals)
	{
		module.m_global_variables.emplace_back(entry.first.c_str());
	}

	if (m_has_async)
	{
		RewriteGlobalsForAsyncModule(module);
	}

	return module;
}



void CodeGenerator::operator()(MidoriStatement::ExpressionStatement& simple)
{
	Visit(simple.m_expr);
	EmitByte(OpCode::POP, simple.m_semicolon.m_line);
}

void CodeGenerator::operator()(MidoriStatement::VariableDefinition& def)
{
	int line = def.m_name.m_line;
	bool is_global = !def.m_local_index.has_value();
	std::optional<int> index = std::nullopt;
	if (is_global)
	{
		MidoriText variable_name(def.m_name.m_lexeme.c_str());
		index.emplace(m_executable.AddGlobalVariable(std::move(variable_name)));
		m_global_variables[def.m_name.m_lexeme] = index.value();
	}

	if (is_global)
	{
		Visit(def.m_value);
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
	}
	else
	{
		// Reserve the local slot before evaluating the initializer so nested scopes
		// (e.g., comprehensions) align with local indices.
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);

		Visit(def.m_value);

		EmitVariable(def.m_local_index.value(), OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::TupleDefinition& def_tuple)
{
	int line = def_tuple.m_names.empty() ? 0 : def_tuple.m_names[0].m_line;

	// For each binding, extract the corresponding array element
	// We regenerate the tuple expression each time since GET_ARRAY consumes it
	for (size_t i = 0u; i < def_tuple.m_names.size(); i += 1u)
	{
		bool is_global = !def_tuple.m_local_indices[i].has_value();

		// Generate the tuple expression (loads it onto stack)
		Visit(def_tuple.m_value);

		// Push the index
		EmitIntegerConstant(static_cast<MidoriInteger>(i), line);

		// Get array element at index i (consumes the array from stack)
		EmitByte(OpCode::GET_ARRAY, line);
		EmitByte(static_cast<OpCode>(1), line); // 1 index

		if (is_global)
		{
			MidoriText variable_name(def_tuple.m_names[i].m_lexeme.c_str());
			int index = m_executable.AddGlobalVariable(std::move(variable_name));
			m_global_variables[def_tuple.m_names[i].m_lexeme] = index;
			EmitVariable(index, OpCode::DEFINE_GLOBAL, line);
		}
		// For local variables, the element is left on stack and becomes the local
	}
}

void CodeGenerator::operator()(MidoriStatement::FunctionDefinition& defun)
{
	int line = defun.m_name.m_line;

	// Mangled names have at least 3 parts separated by underscores, with a capital letter after first underscore
	bool is_instance_method = false;
	{
		size_t first_underscore = defun.m_name.m_lexeme.find('_');
		if (first_underscore != std::string::npos && first_underscore + 1u < defun.m_name.m_lexeme.size())
		{
			if (std::isupper(defun.m_name.m_lexeme[first_underscore + 1u]))
			{
				size_t second_underscore = defun.m_name.m_lexeme.find('_', first_underscore + 1u);
				is_instance_method = (second_underscore != std::string::npos);
			}
		}
	}

	// Instance methods should always be global, regardless of local_index
	bool is_global = !defun.m_local_index.has_value() || is_instance_method;
	std::optional<int> index = std::nullopt;
	bool is_generic = !defun.m_generic_params.empty();

	if (is_generic && is_global)
	{
		m_generic_functions.emplace(defun.m_name.m_lexeme, GenericFunctionInfo(defun.m_name.m_lexeme, defun.m_params, defun.m_param_types, defun.m_generic_params, defun.m_constraints, defun.m_return_type, std::shared_ptr<MidoriExpression>(std::move(defun.m_body)), defun.m_captured_count));
		return;
	}

	if (is_global)
	{
		MidoriText variable_name(defun.m_name.m_lexeme.c_str());
		index.emplace(m_executable.AddGlobalVariable(std::move(variable_name)));
		m_global_variables[defun.m_name.m_lexeme] = index.value();
	}

	EmitFunction(defun.m_params, defun.m_body, defun.m_name.m_lexeme, line, defun.m_captured_count);

	if (is_global)
	{
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
	}
	else
	{
		// Local function - store in local variable
		EmitVariable(defun.m_local_index.value(), OpCode::SET_LOCAL, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::Continue& continue_stmt)
{
	int line = continue_stmt.m_keyword.m_line;

	while (continue_stmt.m_number_to_pop > 0)
	{
		int count_to_pop = std::min(continue_stmt.m_number_to_pop, static_cast<int>(UINT8_MAX));
		EmitByte(OpCode::POP_VALUES, line);
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		continue_stmt.m_number_to_pop -= count_to_pop;
	}

	EmitLoop(m_loop_contexts.top().m_continue_target, line);
}

void CodeGenerator::operator()(MidoriStatement::ForeignDefinition& foreign)
{
	int line = foreign.m_function_name.m_line;

	const MidoriType::FunctionType& type = foreign.m_type->GetType<MidoriType::FunctionType>();
	if (!(type.m_return_type->IsType<MidoriType::IntegerType>() || type.m_return_type->IsType<MidoriType::FloatType>() || type.m_return_type->IsType<MidoriType::BoolType>() || type.m_return_type->IsType<MidoriType::UnitType>() || type.m_return_type->IsType<MidoriType::TextType>() || type.m_return_type->IsType<MidoriType::ArrayType>() || type.m_return_type->IsType<MidoriType::ByteType>() || type.m_return_type->IsType<MidoriType::WordType>()))
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported return type for foreign function", foreign.m_function_name, m_file_name, m_source_lines));
		return;
	}

	std::optional<size_t> ffi_index = MidoriFFIRegistry::FindIndex(foreign.m_foreign_name);
	if (ffi_index.has_value())
	{
		m_ffi_indices[foreign.m_function_name.m_lexeme] = ffi_index.value();
	}

	bool is_global = !foreign.m_local_index.has_value();
	std::optional<int> index = std::nullopt;
	if (is_global)
	{
		MidoriText foreign_function_name(foreign.m_function_name.m_lexeme.c_str());
		index.emplace(m_executable.AddGlobalVariable(std::move(foreign_function_name)));
		m_global_variables[foreign.m_function_name.m_lexeme] = index.value();
	}

	EmitTextConstant(foreign.m_foreign_name, line);

	if (is_global)
	{
		EmitVariable(index.value(), OpCode::DEFINE_GLOBAL, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::Struct&)
{
	return;
}

void CodeGenerator::operator()(MidoriStatement::Union&)
{
	return;
}

void CodeGenerator::operator()(MidoriStatement::Class& class_stmt)
{
	std::unordered_set<std::string> method_names;

	for (const std::unique_ptr<MidoriStatement>& method : class_stmt.m_methods)
	{
		if (method->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			const MidoriStatement::FunctionDefinition& defun = method->GetStatement<MidoriStatement::FunctionDefinition>();
			method_names.insert(defun.m_name.m_lexeme);
		}
	}

	m_class_methods[class_stmt.m_name.m_lexeme] = std::move(method_names);
	return;
}

void CodeGenerator::operator()(MidoriStatement::Instance& instance_stmt)
{
	for (const std::unique_ptr<MidoriStatement>& method : instance_stmt.m_methods)
	{
		if (!method->IsStatement<MidoriStatement::FunctionDefinition>())
		{
			Visit(method);
			continue;
		}

		MidoriStatement::FunctionDefinition& defun = method->GetStatement<MidoriStatement::FunctionDefinition>();
		std::vector<std::string>& instance_methods = m_class_instances[instance_stmt.m_class_name.m_lexeme];
		if (std::ranges::find(instance_methods, defun.m_name.m_lexeme) == instance_methods.cend())
		{
			instance_methods.emplace_back(defun.m_name.m_lexeme);
		}

		int line = defun.m_name.m_line;
		int index = 0;
		std::unordered_map<std::string, int>::iterator global_it = m_global_variables.find(defun.m_name.m_lexeme);
		if (global_it != m_global_variables.end())
		{
			index = global_it->second;
		}
		else
		{
			MidoriText variable_name(defun.m_name.m_lexeme.c_str());
			index = m_executable.AddGlobalVariable(std::move(variable_name));
			m_global_variables[defun.m_name.m_lexeme] = index;
		}

		EmitFunction(defun.m_params, defun.m_body, defun.m_name.m_lexeme, line, defun.m_captured_count);
		EmitVariable(index, OpCode::DEFINE_GLOBAL, line);
	}
}

void CodeGenerator::operator()(MidoriStatement::TypeAlias&)
{
	// Type aliases are resolved at compile time, no runtime code generation needed
	return;
}

void CodeGenerator::operator()(MidoriExpression::As& as)
{
	int line = as.m_as_keyword.m_line;

	Visit(as.m_expr);

	std::shared_ptr<MidoriType> from_type = as.m_from_type.lock();
	const std::shared_ptr<MidoriType>& target_type = as.m_to_type;

	// Handle conversions that use Convertable typeclass or involve type variables
	if (as.m_uses_convertable || from_type->IsType<MidoriType::TypeVariable>() || target_type->IsType<MidoriType::TypeVariable>())
	{
		// First, check if we're inside a specialized generic function and can resolve via method resolution map
		std::string qualified_method_name = "Convertable"s + std::string(NameSeparator) + "Convert"s;
		std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(qualified_method_name);

		if (resolution_it != m_method_resolution_map.end())
		{
			// Resolve using the method resolution map (we're inside a specialized generic function)
			// Get the concrete type for the expression being converted
			std::shared_ptr<MidoriType> concrete_from_type = GetConcreteTypeForExpression(as.m_expr);
			std::string from_type_str = concrete_from_type->ToString();
			std::string to_type_str = target_type->ToString();

			std::string resolved_method;
			bool found = false;
			for (const ResolvedMethodCandidate& candidate : resolution_it->second)
			{
				if (candidate.m_first_type_name == from_type_str && candidate.m_second_type_name == to_type_str && candidate.m_has_instance)
				{
					resolved_method = candidate.m_resolved_name;
					found = true;
					break;
				}
			}

			if (found)
			{
				if (EmitResolvedNameGetGlobal(resolved_method, line))
				{
					EmitCall(1, line);
					return;
				}
			}
		}

		// Not in a specialized context, try direct lookup for concrete Convertable instances
		if (!from_type->IsType<MidoriType::TypeVariable>() && !target_type->IsType<MidoriType::TypeVariable>())
		{
			std::vector<std::shared_ptr<MidoriType>> concrete_args;
			concrete_args.emplace_back(from_type);
			concrete_args.emplace_back(target_type);
			std::string mangled_name = MidoriType::MangleInstanceMethodName("Convert", "Convertable", concrete_args);
			std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
			if (it != m_global_variables.end())
			{
				EmitVariable(it->second, OpCode::GET_GLOBAL, line);
				EmitCall(1, line);
				return;
			}

			std::optional<std::string> resolved_name;
			TypeclassInstanceTypeMap::iterator instance_args_it = m_class_instance_type_args.find("Convertable");
			if (instance_args_it != m_class_instance_type_args.end())
			{
				for (const std::vector<std::shared_ptr<MidoriType>>& candidate_args : instance_args_it->second)
				{
					if (candidate_args.size() != 2u)
					{
						continue;
					}

					TypeEnvironment substitutions;
					std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash> visited;
					if (!MatchInstanceTypeArg(candidate_args[0u], from_type, substitutions, visited))
					{
						continue;
					}
					if (!MatchInstanceTypeArg(candidate_args[1u], target_type, substitutions, visited))
					{
						continue;
					}

					std::string candidate_base = MidoriType::MangleInstanceMethodName("Convert", "Convertable", candidate_args);
					std::optional<std::string> candidate_name = ResolveInstanceName("Convertable", candidate_base);
					if (!candidate_name.has_value())
					{
						continue;
					}

					if (resolved_name.has_value() && resolved_name.value() != candidate_name.value())
					{
						AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Convertable instance method resolution is ambiguous for types '"s + from_type->ToString() + "' -> '"s + target_type->ToString() + "'"s, as.m_as_keyword, m_file_name, m_source_lines));
						return;
					}

					resolved_name = std::move(candidate_name);
				}
			}

			if (resolved_name.has_value())
			{
				if (EmitResolvedNameGetGlobal(resolved_name.value(), line))
				{
					EmitCall(1, line);
					return;
				}
			}
			else if (as.m_uses_convertable)
			{
				AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Convertable instance method '"s + mangled_name + "' not found"s, as.m_as_keyword, m_file_name, m_source_lines));
				return;
			}
		}
		else if (as.m_uses_convertable)
		{
			// Type variables without resolution - this shouldn't happen if type checking was correct
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Cannot resolve Convertable instance for type variables outside of specialization context"s, as.m_as_keyword, m_file_name, m_source_lines));
			return;
		}
	}

	// If we reach here with type variables, fall through to check for built-in conversions
	if (target_type->IsType<MidoriType::BoolType>())
	{
		// Do nothing
	}
	else if (target_type->IsType<MidoriType::FloatType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			EmitByte(OpCode::TEXT_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_FLOAT, line);
		}
		else if (from_type->IsType<MidoriType::FloatType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to float' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::IntegerType>())
	{
		if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			EmitByte(OpCode::TEXT_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_INT, line);
		}
		else if (from_type->IsType<MidoriType::IntegerType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to int' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::ByteType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_BYTE, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_BYTE, line);
		}
		else if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_BYTE, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to byte' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::WordType>())
	{
		if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_WORD, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_WORD, line);
		}
		else if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_WORD, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to word' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else if (target_type->IsType<MidoriType::UnitType>())
	{
		EmitByte(OpCode::OP_UNIT, line);
	}
	else if (target_type->IsType<MidoriType::TextType>())
	{
		if (from_type->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::FLOAT_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::IntegerType>())
		{
			EmitByte(OpCode::INT_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::ByteType>())
		{
			EmitByte(OpCode::BYTE_TO_INT, line);
			EmitByte(OpCode::INT_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::WordType>())
		{
			EmitByte(OpCode::WORD_TO_INT, line);
			EmitByte(OpCode::INT_TO_TEXT, line);
		}
		else if (from_type->IsType<MidoriType::TextType>())
		{
			// Do nothing
		}
		else
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported 'cast to text' instruction", as.m_as_keyword, m_file_name, m_source_lines));
		}
	}
	else
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Unsupported type casting instruction", as.m_as_keyword, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::Binary& binary)
{
	int line = binary.m_op.m_line;

	if (binary.m_op.m_token_name == Token::Name::DOUBLE_BAR)
	{
		Visit(binary.m_left);
		int jump_if_true = EmitJump(OpCode::JUMP_IF_TRUE, line);
		EmitByte(OpCode::POP, line);
		Visit(binary.m_right);
		PatchJump(jump_if_true, line);
		return;
	}
	else if (binary.m_op.m_token_name == Token::Name::DOUBLE_AMPERSAND)
	{
		Visit(binary.m_left);
		int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		Visit(binary.m_right);
		PatchJump(jump_if_false, line);
	}
	else
	{
		Visit(binary.m_left);
		Visit(binary.m_right);
		const std::shared_ptr<MidoriType>& operand_type = GetConcreteTypeForExpression(binary.m_left);

		switch (binary.m_op.m_token_name)
		{
		case Token::Name::SINGLE_PLUS:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::ADD_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::ADD_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::ADD_WORD, line);
			}
			else
			{
				EmitByte(OpCode::ADD_INTEGER, line);
			}
			break;
		}
		case Token::Name::DOUBLE_PLUS:
		{
			if (operand_type->IsType<MidoriType::TextType>())
			{
				EmitByte(OpCode::CONCAT_TEXT, line);
			}
			else if (operand_type->IsType<MidoriType::ArrayType>())
			{
				EmitByte(OpCode::CONCAT_ARRAY, line);
			}
			else
			{
				const std::string actual_type = operand_type ? operand_type->ToString() : "Unknown";
				AddError
				(
					MidoriError::GenerateCodeGeneratorErrorWithContext
					(
						std::format("Concatenation operator '++' requires Text or Array type (got {})", actual_type),
						binary.m_op,
						m_file_name,
						m_source_lines
					)
				);
			}
			break;
		}
		case Token::Name::SINGLE_MINUS:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::SUBTRACT_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::SUBTRACT_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::SUBTRACT_WORD, line);
			}
			else
			{
				EmitByte(OpCode::SUBTRACT_INTEGER, line);
			}
			break;
		}
		case Token::Name::STAR:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::MULTIPLY_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::IntegerType>())
			{
				EmitByte(OpCode::MULTIPLY_INTEGER, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::MULTIPLY_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::MULTIPLY_WORD, line);
			}
			else if (operand_type->IsType<MidoriType::ArrayType>())
			{
				EmitByte(OpCode::DUP_ARRAY, line);
			}
			else
			{
				const std::string actual_type = operand_type ? operand_type->ToString() : "Unknown";
				AddError
				(
					MidoriError::GenerateCodeGeneratorErrorWithContext
					(
						std::format("Binary '*' requires numeric or array type (got {})", actual_type),
						binary.m_op,
						m_file_name,
						m_source_lines
					)
				);
			}
			break;
		}
		case Token::Name::SLASH:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::DIVIDE_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::DIVIDE_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::DIVIDE_WORD, line);
			}
			else
			{
				EmitByte(OpCode::DIVIDE_INTEGER, line);
			}
			break;
		}
		case Token::Name::PERCENT:
		{
			if (operand_type->IsType<MidoriType::FloatType>())
			{
				EmitByte(OpCode::MODULO_FLOAT, line);
			}
			else if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::MODULO_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::MODULO_WORD, line);
			}
			else
			{
				EmitByte(OpCode::MODULO_INTEGER, line);
			}
			break;
		}
		case Token::Name::LEFT_SHIFT:
		{
			if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::LEFT_SHIFT_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::LEFT_SHIFT_WORD, line);
			}
			else
			{
				EmitByte(OpCode::LEFT_SHIFT, line);
			}
			break;
		}
		case Token::Name::RIGHT_SHIFT:
		{
			if (operand_type->IsType<MidoriType::ByteType>())
			{
				EmitByte(OpCode::RIGHT_SHIFT_BYTE, line);
			}
			else if (operand_type->IsType<MidoriType::WordType>())
			{
				EmitByte(OpCode::RIGHT_SHIFT_WORD, line);
			}
			else
			{
				EmitByte(OpCode::RIGHT_SHIFT, line);
			}
			break;
		}
		case Token::Name::LEFT_ANGLE:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::LESS_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::LESS_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::LESS_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::LESS_WORD, line);
				}
				else
				{
					EmitByte(OpCode::LESS_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::LESS_EQUAL:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::LESS_EQUAL_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::LESS_EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::LESS_EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::LESS_EQUAL_WORD, line);
				}
				else
				{
					EmitByte(OpCode::LESS_EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::RIGHT_ANGLE:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::GREATER_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::GREATER_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::GREATER_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::GREATER_WORD, line);
				}
				else
				{
					EmitByte(OpCode::GREATER_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::GREATER_EQUAL:
		{
			if (binary.m_uses_orderable)
			{
				EmitOrderableCompare(operand_type, line);
				EmitIntegerConstant(0, line);
				EmitByte(OpCode::GREATER_EQUAL_INTEGER, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::GREATER_EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::GREATER_EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::GREATER_EQUAL_WORD, line);
				}
				else
				{
					EmitByte(OpCode::GREATER_EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::BANG_EQUAL:
		{
			if (binary.m_uses_equatable)
			{
				EmitEquatableEquals(operand_type, line);
				EmitByte(OpCode::NOT, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::NOT_EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::NOT_EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::NOT_EQUAL_WORD, line);
				}
				else
				{
					EmitByte(OpCode::NOT_EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::DOUBLE_EQUAL:
		{
			if (binary.m_uses_equatable)
			{
				EmitEquatableEquals(operand_type, line);
			}
			else
			{
				if (operand_type->IsType<MidoriType::FloatType>())
				{
					EmitByte(OpCode::EQUAL_FLOAT, line);
				}
				else if (operand_type->IsType<MidoriType::IntegerType>())
				{
					EmitByte(OpCode::EQUAL_INTEGER, line);
				}
				else if (operand_type->IsType<MidoriType::ByteType>())
				{
					EmitByte(OpCode::EQUAL_BYTE, line);
				}
				else if (operand_type->IsType<MidoriType::WordType>())
				{
					EmitByte(OpCode::EQUAL_WORD, line);
				}
				else if (operand_type->IsType<MidoriType::TextType>())
				{
					EmitByte(OpCode::EQUAL_TEXT, line);
				}
				else
				{
					EmitByte(OpCode::EQUAL_INTEGER, line);
				}
			}
			break;
		}
		case Token::Name::SINGLE_AMPERSAND:
		{
			EmitByte(OpCode::BITWISE_AND, line);
			break;
		}
		case Token::Name::SINGLE_BAR:
		{
			EmitByte(OpCode::BITWISE_OR, line);
			break;
		}
		case Token::Name::CARET:
		{
			EmitByte(OpCode::BITWISE_XOR, line);
			break;
		}
		default:
		{
#ifdef _MSC_VER
			__assume(0);
#else
			__builtin_unreachable();
#endif
		}
		}
	}
}

void CodeGenerator::operator()(MidoriExpression::Group& group)
{
	Visit(group.m_expr_in);
}

void CodeGenerator::operator()(MidoriExpression::Tuple& tuple)
{
	int line = tuple.m_op.m_line;
	int size = static_cast<int>(tuple.m_elements.size());

	// Emit code for each element (they will be pushed onto the stack)
	std::ranges::for_each
	(
		tuple.m_elements,
		[this](const std::unique_ptr<MidoriExpression>& elem)
		{
			Visit(elem);
		}
	);

	// At runtime, tuples are represented as arrays (heterogeneous)
	// Type checking ensures type safety
	EmitByte(OpCode::CREATE_ARRAY, line);
	EmitThreeBytes(size, size >> 8, size >> 16, line);
}

bool CodeGenerator::EmitGenericLengthCall(const std::string& function_name, const std::shared_ptr<MidoriType>& operand_type, int line)
{
	std::vector<std::shared_ptr<MidoriType>> arg_types;
	arg_types.emplace_back(operand_type);
	int specialized_proc_index = SpecializeGenericFunction(function_name, arg_types, line);
	if (specialized_proc_index == -1)
	{
		return false;
	}

	GenericFunctionInfo& generic_info = m_generic_functions[function_name];
	if (generic_info.m_captured_count == 0)
	{
		EmitCallProc(specialized_proc_index, 1, line);
	}
	else
	{
		const bool uses_shared_captures = CurrentProcedureUsesSharedCells();
		EmitByte(OpCode::MAKE_CLOSURE, line);
		EmitByte(static_cast<OpCode>(specialized_proc_index), line);
		NoteCaptureBinding(generic_info.m_captured_count, uses_shared_captures);
		EmitByte(uses_shared_captures ? OpCode::BIND_CAPTURES_SHARED : OpCode::BIND_CAPTURES, line);
		EmitByte(static_cast<OpCode>(generic_info.m_captured_count), line);
		EmitCall(1, line);
	}

	return true;
}

bool CodeGenerator::EmitCountableCall(const MidoriExpression::UnaryPrefix& unary, const std::shared_ptr<MidoriType>& count_type, int line)
{
	std::string qualified_method_name = std::string(COUNTABLE_CLASS_NAME) + std::string(NameSeparator) + std::string(COUNT_METHOD_NAME);
	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(qualified_method_name);

	if (resolution_it != m_method_resolution_map.end())
	{
		std::shared_ptr<MidoriType> concrete_type = GetConcreteTypeForExpression(unary.m_expr);
		std::string type_name = concrete_type->ToString();

		std::string resolved_method;
		bool found = false;
		for (const ResolvedMethodCandidate& candidate : resolution_it->second)
		{
			if (candidate.m_first_type_name == type_name && candidate.m_has_instance)
			{
				resolved_method = candidate.m_resolved_name;
				found = true;
				break;
			}
		}

		if (found)
		{
			if (EmitResolvedNameGetGlobal(resolved_method, line))
			{
				EmitCall(1, line);
				return true;
			}
		}
	}

	if (!count_type->IsType<MidoriType::TypeVariable>())
	{
		std::string mangled_name = INTERNAL_NAME_PREFIX + std::string(COUNT_MANGLED_PREFIX) + count_type->ToString();
		std::unordered_map<std::string, int>::iterator it = m_global_variables.find(mangled_name);
		if (it != m_global_variables.end())
		{
			EmitVariable(it->second, OpCode::GET_GLOBAL, line);
			EmitCall(1, line);
			return true;
		}

		if (unary.m_uses_countable)
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Countable instance method '"s + mangled_name + "' not found"s, unary.m_op, m_file_name, m_source_lines));
		}
	}
	else if (unary.m_uses_countable)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Cannot resolve Countable instance for type variables outside of specialization context"s, unary.m_op, m_file_name, m_source_lines));
	}

	return false;
}

void CodeGenerator::operator()(MidoriExpression::UnaryPrefix& unary)
{
	Visit(unary.m_expr);

	switch (unary.m_op.m_token_name)
	{
	case Token::Name::SINGLE_MINUS:
	{
		if (GetConcreteTypeForExpression(unary.m_expr)->IsType<MidoriType::FloatType>())
		{
			EmitByte(OpCode::NEGATE_FLOAT, unary.m_op.m_line);
		}
		else
		{
			EmitByte(OpCode::NEGATE_INTEGER, unary.m_op.m_line);
		}
		break;
	}
	case Token::Name::SINGLE_PLUS:
	{
		break;
	}
	case Token::Name::BANG:
	{
		EmitByte(OpCode::NOT, unary.m_op.m_line);
		break;
	}
	case Token::Name::TILDE:
	{
		EmitByte(OpCode::BITWISE_NOT, unary.m_op.m_line);
		break;
	}
	case Token::Name::HASH:
	{
		int line = unary.m_op.m_line;
		std::shared_ptr<MidoriType> operand_type = GetConcreteTypeForExpression(unary.m_expr);

		if (operand_type->IsType<MidoriType::ArrayType>())
		{
			EmitByte(OpCode::GET_ARRAY_LENGTH, line);
			break;
		}

		const bool is_list_type = operand_type->IsType<MidoriType::UnionType>() &&
			HasNameSuffix(operand_type->GetType<MidoriType::UnionType>().m_name, "List");
		const bool is_map_type = operand_type->IsType<MidoriType::StructType>() &&
			(
				HasNameSuffix(operand_type->GetType<MidoriType::StructType>().m_name, "MapData") ||
				HasNameSuffix(operand_type->GetType<MidoriType::StructType>().m_name, "Map")
			);
		const bool is_set_type = operand_type->IsType<MidoriType::StructType>() &&
			(
				HasNameSuffix(operand_type->GetType<MidoriType::StructType>().m_name, "SetData") ||
				HasNameSuffix(operand_type->GetType<MidoriType::StructType>().m_name, "Set")
			);

		if (!unary.m_uses_countable)
		{
			if (is_list_type)
			{
				EmitGenericLengthCall("ListLength", operand_type, line);
				break;
			}
			if (is_map_type)
			{
				EmitGenericLengthCall("MapCount", operand_type, line);
				break;
			}
			if (is_set_type)
			{
				EmitGenericLengthCall("SetCount", operand_type, line);
				break;
			}
		}

		if (EmitCountableCall(unary, operand_type, line))
		{
			break;
		}

		break;
	}
	default:
	{
		return;
	}
	}

	return;
}

void CodeGenerator::operator()(MidoriExpression::UnarySuffix&)
{
	// TODO: no suffix operators at the moment
	return;
}

void CodeGenerator::operator()(MidoriExpression::Call& call)
{
	int line = call.m_paren.m_line;
	int arity = static_cast<int>(call.m_arguments.size());
	if (arity > MAX_FUNCTION_ARITY)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many arguments (max {})", MAX_FUNCTION_ARITY + 1), call.m_paren, m_file_name, m_source_lines));
		return;
	}

	bool is_generic_call = false;
	std::string function_name;
	std::optional<std::string> resolved_method_name = std::nullopt;

	if (call.m_callee->IsExpression<MidoriExpression::NameAccess>())
	{
		MidoriExpression::NameAccess& callee_name = call.m_callee->GetExpression<MidoriExpression::NameAccess>();
		function_name = callee_name.m_name.m_lexeme;

		std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_method_resolution_map.find(function_name);
		if (resolution_it != m_method_resolution_map.end())
		{
			resolved_method_name = ResolveMethodNameForCall(function_name, call, line);
			if (!resolved_method_name.has_value())
			{
				return;
			}
			function_name = resolved_method_name.value();
		}

		std::unordered_map<std::string, GenericFunctionInfo>::iterator generic_it = m_generic_functions.find(function_name);
		if (generic_it != m_generic_functions.end())
		{
			is_generic_call = true;
		}
		else if (function_name.find("::") != std::string::npos)
		{
			// Try suffix lookup for qualified names
			std::string suffix = function_name.substr(function_name.rfind("::") + 2);
			if (m_generic_functions.contains(suffix))
			{
				is_generic_call = true;
				function_name = suffix;
			}
		}
	}

	if (is_generic_call)
	{
		std::vector<std::shared_ptr<MidoriType>> concrete_arg_types;
		for (std::unique_ptr<MidoriExpression>& arg : call.m_arguments)
		{
			// Get concrete type if we're in a specialization context
			concrete_arg_types.push_back(GetConcreteTypeForExpression(arg));
		}

		int specialized_proc_index = SpecializeGenericFunction(function_name, concrete_arg_types, line);
		if (specialized_proc_index == -1)
		{
			return;
		}

		// Update the call expression type with the concrete return type for this specialization.
		GenericFunctionInfo& generic_info = m_generic_functions[function_name];
		if (generic_info.m_generic_return_type)
		{
			TypeEnvironment local_type_map;
			std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash> visited;
			for (size_t i = 0u; i < generic_info.m_param_types.size() && i < concrete_arg_types.size(); i += 1u)
			{
				DeduceGenericTypesRecursive(generic_info.m_param_types[i], concrete_arg_types[i], local_type_map, visited);
			}
			if (!local_type_map.empty())
			{
				call.m_type_data = SubstituteGenericTypes(generic_info.m_generic_return_type, local_type_map);
			}
			else
			{
				call.m_type_data = generic_info.m_generic_return_type;
			}
		}

		std::ranges::for_each
		(
			call.m_arguments,
			[this](std::unique_ptr<MidoriExpression>& param)
			{
				Visit(param);
			}
		);


		if (generic_info.m_captured_count == 0)
		{
			if (call.m_is_tail_call)
			{
				const bool uses_shared_captures = CurrentProcedureUsesSharedCells();
				// Fallback to closure call for tail calls
				EmitByte(OpCode::MAKE_CLOSURE, line);
				EmitByte(static_cast<OpCode>(specialized_proc_index), line);
				NoteCaptureBinding(0, uses_shared_captures);
				EmitByte(uses_shared_captures ? OpCode::BIND_CAPTURES_SHARED : OpCode::BIND_CAPTURES, line);
				EmitByte(static_cast<OpCode>(0), line);
				EmitByte(OpCode::TAIL_CALL, line);
				EmitByte(static_cast<OpCode>(arity), line);
			}
			else
			{
				EmitCallProc(specialized_proc_index, arity, line);
			}
		}
		else
		{
			const bool uses_shared_captures = CurrentProcedureUsesSharedCells();
			// Push the specialized closure
			EmitByte(OpCode::MAKE_CLOSURE, line);
			EmitByte(static_cast<OpCode>(specialized_proc_index), line);

			NoteCaptureBinding(generic_info.m_captured_count, uses_shared_captures);
			EmitByte(uses_shared_captures ? OpCode::BIND_CAPTURES_SHARED : OpCode::BIND_CAPTURES, line);
			EmitByte(static_cast<OpCode>(generic_info.m_captured_count), line);

			if (call.m_is_tail_call)
			{
				EmitByte(OpCode::TAIL_CALL, line);
			}
			else
			{
				EmitCall(arity, line);
			}
			if (call.m_is_tail_call)
			{
				EmitByte(static_cast<OpCode>(arity), line);
			}
		}
	}
	else
	{
		std::ranges::for_each
		(
			call.m_arguments,
			[this](std::unique_ptr<MidoriExpression>& param)
			{
				Visit(param);
			}
		);

		bool can_emit_call_global = false;
		int call_global_index = -1;

		std::optional<size_t> ffi_index_opt = std::nullopt;
		if (call.m_is_foreign && call.m_callee->IsExpression<MidoriExpression::NameAccess>())
		{
			std::unordered_map<std::string, size_t>::iterator ffi_it = m_ffi_indices.find(function_name);
			if (ffi_it != m_ffi_indices.end())
			{
				ffi_index_opt = ffi_it->second;
			}
		}

		if (!ffi_index_opt.has_value() && !call.m_is_foreign && !call.m_is_tail_call)
		{
			if (resolved_method_name.has_value())
			{
				std::optional<int> resolved_global_index = ResolveResolvedNameGlobalIndex(resolved_method_name.value(), line);
				if (!resolved_global_index.has_value())
				{
					return;
				}

				can_emit_call_global = true;
				call_global_index = resolved_global_index.value();
			}
			else if (call.m_callee->IsExpression<MidoriExpression::NameAccess>())
			{
				const MidoriExpression::NameAccess& callee_name = call.m_callee->GetExpression<MidoriExpression::NameAccess>();
				if (std::holds_alternative<MidoriExpression::NameContext::Global>(callee_name.m_name_ctx))
				{
					std::unordered_map<std::string, int>::iterator global_it = m_global_variables.find(callee_name.m_name.m_lexeme);
					if (global_it != m_global_variables.end())
					{
						can_emit_call_global = true;
						call_global_index = global_it->second;
					}
				}
			}
		}

		if (!ffi_index_opt.has_value() && !can_emit_call_global)
		{
			if (resolved_method_name.has_value())
			{
				if (!EmitResolvedNameGetGlobal(resolved_method_name.value(), line))
				{
					return;
				}
			}
			else
			{
				Visit(call.m_callee);
			}
		}

		if (call.m_is_foreign)
		{
			uint8_t return_type_tag = 0;
			if (call.m_type_data->IsType<MidoriType::TextType>())
			{
				return_type_tag = 1;
			}
			else if (call.m_type_data->IsType<MidoriType::ArrayType>())
			{
				return_type_tag = 2;
			}

			if (ffi_index_opt.has_value())
			{
				EmitByte(OpCode::CALL_FOREIGN_INDEXED, line);
				EmitByte(static_cast<OpCode>(ffi_index_opt.value()), line);
				EmitByte(static_cast<OpCode>(arity), line);
				EmitByte(static_cast<OpCode>(return_type_tag), line);
			}
			else
			{
				EmitByte(OpCode::CALL_FOREIGN, line);
				EmitByte(static_cast<OpCode>(arity), line);
				EmitByte(static_cast<OpCode>(return_type_tag), line);
			}
		}
		else if (can_emit_call_global)
		{
			EmitCallGlobal(call_global_index, arity, line);
		}
		else if (call.m_is_tail_call)
		{
			EmitByte(OpCode::TAIL_CALL, line);
			EmitByte(static_cast<OpCode>(arity), line);
		}
		else
		{
			EmitCall(arity, line);
		}
	}
}

void CodeGenerator::operator()(MidoriExpression::MemberAccess& get)
{
	int line = get.m_member_name.m_line;

	Visit(get.m_struct);
	EmitByte(OpCode::GET_MEMBER, line);
	EmitByte(static_cast<OpCode>(get.m_index), line);
}

void CodeGenerator::operator()(MidoriExpression::MemberAssignment& set)
{
	int line = set.m_member_name.m_line;

	Visit(set.m_struct);
	Visit(set.m_value);
	EmitByte(OpCode::SET_MEMBER, line);
	EmitByte(static_cast<OpCode>(set.m_index), line);
}

void CodeGenerator::operator()(MidoriExpression::NameAccess& variable)
{
	struct NameAccessVisitor
	{
		CodeGenerator* m_self = nullptr;
		MidoriExpression::NameAccess* m_variable = nullptr;

		void operator()(const MidoriExpression::NameContext::Local& arg) const
		{
			const int line = m_variable->m_name.m_line;
			m_self->EmitVariable(arg.m_index, OpCode::GET_LOCAL, line);
		}

		void operator()(const MidoriExpression::NameContext::Global&) const
		{
			const int line = m_variable->m_name.m_line;
			const std::string& name = m_variable->m_name.m_lexeme;

			std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator resolution_it = m_self->m_method_resolution_map.find(name);
			if (resolution_it != m_self->m_method_resolution_map.end())
			{
				const std::vector<ResolvedMethodCandidate>& candidates = resolution_it->second;
				if (candidates.size() != 1u)
				{
					m_self->AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': cannot use method value when multiple class constraints are in scope.", name), line, m_self->m_file_name, m_self->m_source_lines));
					return;
				}
				if (!candidates[0u].m_has_instance)
				{
					m_self->AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Unresolved method '{}': no matching instance found.", name), line, m_self->m_file_name, m_self->m_source_lines));
					return;
				}
				m_self->EmitResolvedNameGetGlobal(candidates[0u].m_resolved_name, line);
				return;
			}

			if (name.find(NameSeparator) != std::string::npos)
			{
				size_t separator_pos = name.find(NameSeparator);
				std::string module_name = name.substr(0u, separator_pos);
				std::string symbol_name = name.substr(separator_pos + 2u);
				int import_placeholder = m_self->GetImportPlaceholder(module_name, symbol_name, line);
				if (import_placeholder < 0)
				{
					return;
				}

				m_self->EmitVariable(import_placeholder, OpCode::GET_GLOBAL, line);
			}
			else
			{
				m_self->EmitVariable(m_self->m_global_variables[name], OpCode::GET_GLOBAL, line);
			}
		}

		void operator()(const MidoriExpression::NameContext::Cell& arg) const
		{
			const int line = m_variable->m_name.m_line;
			m_self->EmitVariable(arg.m_index, m_self->GetCellLoadOpcode(), line);
		}
	};

	std::visit(NameAccessVisitor{ this, &variable }, variable.m_name_ctx);
}

void CodeGenerator::operator()(MidoriExpression::AppendAssign& append_assign)
{
	int line = append_assign.m_name.m_line;

	struct AppendAssignVisitor
	{
		CodeGenerator* m_self = nullptr;
		MidoriExpression::AppendAssign* m_assign = nullptr;
		int m_line = 0;

		void operator()(const MidoriExpression::NameContext::Local& arg) const
		{
			m_self->EmitVariable(arg.m_index, OpCode::GET_LOCAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Global&) const
		{
			const std::string& name = m_assign->m_name.m_lexeme;
			m_self->EmitVariable(m_self->m_global_variables[name], OpCode::GET_GLOBAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Cell& arg) const
		{
			m_self->EmitVariable(arg.m_index, m_self->GetCellLoadOpcode(), m_line);
		}
	};

	std::visit(AppendAssignVisitor{ this, &append_assign, line }, append_assign.m_name_ctx);

	Visit(append_assign.m_value);

	if (append_assign.m_type_data->IsType<MidoriType::ArrayType>())
	{
		EmitByte(OpCode::APPEND_ARRAY, line);
	}
	else if (append_assign.m_type_data->IsType<MidoriType::TextType>())
	{
		EmitByte(OpCode::APPEND_TEXT, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::ExtendAssign& extend_assign)
{
	int line = extend_assign.m_name.m_line;

	struct ExtendAssignVisitor
	{
		CodeGenerator* m_self = nullptr;
		MidoriExpression::ExtendAssign* m_assign = nullptr;
		int m_line = 0;

		void operator()(const MidoriExpression::NameContext::Local& arg) const
		{
			m_self->EmitVariable(arg.m_index, OpCode::GET_LOCAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Global&) const
		{
			const std::string& name = m_assign->m_name.m_lexeme;
			m_self->EmitVariable(m_self->m_global_variables[name], OpCode::GET_GLOBAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Cell& arg) const
		{
			m_self->EmitVariable(arg.m_index, m_self->GetCellLoadOpcode(), m_line);
		}
	};

	std::visit(ExtendAssignVisitor{ this, &extend_assign, line }, extend_assign.m_name_ctx);

	Visit(extend_assign.m_value);
	EmitByte(OpCode::EXTEND_ARRAY, line);
}

void CodeGenerator::operator()(MidoriExpression::PrependAssign& prepend_assign)
{
	int line = prepend_assign.m_name.m_line;

	struct PrependAssignVisitor
	{
		CodeGenerator* m_self = nullptr;
		MidoriExpression::PrependAssign* m_assign = nullptr;
		int m_line = 0;

		void operator()(const MidoriExpression::NameContext::Local& arg) const
		{
			m_self->EmitVariable(arg.m_index, OpCode::GET_LOCAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Global&) const
		{
			const std::string& name = m_assign->m_name.m_lexeme;
			m_self->EmitVariable(m_self->m_global_variables[name], OpCode::GET_GLOBAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Cell& arg) const
		{
			m_self->EmitVariable(arg.m_index, m_self->GetCellLoadOpcode(), m_line);
		}
	};

	std::visit(PrependAssignVisitor{ this, &prepend_assign, line }, prepend_assign.m_name_ctx);

	Visit(prepend_assign.m_value);

	if (prepend_assign.m_type_data->IsType<MidoriType::ArrayType>())
	{
		EmitByte(OpCode::PREPEND_ARRAY, line);
	}
	else if (prepend_assign.m_type_data->IsType<MidoriType::TextType>())
	{
		EmitByte(OpCode::PREPEND_TEXT, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::CompoundAssign& compound_assign)
{
	int line = compound_assign.m_name.m_line;

	struct CompoundAssignVisitor
	{
		CodeGenerator* m_self = nullptr;
		MidoriExpression::CompoundAssign* m_assign = nullptr;
		int m_line = 0;

		void operator()(const MidoriExpression::NameContext::Local& arg) const
		{
			m_self->EmitVariable(arg.m_index, OpCode::GET_LOCAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Global&) const
		{
			const std::string& name = m_assign->m_name.m_lexeme;
			m_self->EmitVariable(m_self->m_global_variables[name], OpCode::GET_GLOBAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Cell& arg) const
		{
			m_self->EmitVariable(arg.m_index, m_self->GetCellLoadOpcode(), m_line);
		}
	};

	std::visit(CompoundAssignVisitor{ this, &compound_assign, line }, compound_assign.m_name_ctx);

	Visit(compound_assign.m_value);

	bool is_float = compound_assign.m_type_data->IsType<MidoriType::FloatType>();
	switch (compound_assign.m_op.m_token_name)
	{
	case Token::Name::PLUS_EQUAL:
		EmitByte(is_float ? OpCode::ADD_ASSIGN_FLOAT : OpCode::ADD_ASSIGN_INT, line);
		break;
	case Token::Name::MINUS_EQUAL:
		EmitByte(is_float ? OpCode::SUB_ASSIGN_FLOAT : OpCode::SUB_ASSIGN_INT, line);
		break;
	case Token::Name::STAR_EQUAL:
		EmitByte(is_float ? OpCode::MUL_ASSIGN_FLOAT : OpCode::MUL_ASSIGN_INT, line);
		break;
	case Token::Name::SLASH_EQUAL:
		EmitByte(is_float ? OpCode::DIV_ASSIGN_FLOAT : OpCode::DIV_ASSIGN_INT, line);
		break;
	case Token::Name::PERCENT_EQUAL:
		EmitByte(is_float ? OpCode::MOD_ASSIGN_FLOAT : OpCode::MOD_ASSIGN_INT, line);
		break;
	case Token::Name::AMPERSAND_EQUAL:
		EmitByte(OpCode::AND_ASSIGN_INT, line);
		break;
	case Token::Name::BAR_EQUAL:
		EmitByte(OpCode::OR_ASSIGN_INT, line);
		break;
	case Token::Name::CARET_EQUAL:
		EmitByte(OpCode::XOR_ASSIGN_INT, line);
		break;
	case Token::Name::LEFT_SHIFT_EQUAL:
		EmitByte(OpCode::LEFT_SHIFT_ASSIGN, line);
		break;
	case Token::Name::RIGHT_SHIFT_EQUAL:
		EmitByte(OpCode::RIGHT_SHIFT_ASSIGN, line);
		break;
	}
}

void CodeGenerator::operator()(MidoriExpression::Assignment& bind)
{
	int line = bind.m_name.m_line;
	Visit(bind.m_value);

	struct AssignmentVisitor
	{
		CodeGenerator* m_self = nullptr;
		MidoriExpression::Assignment* m_bind = nullptr;
		int m_line = 0;

		void operator()(const MidoriExpression::NameContext::Local& arg) const
		{
			m_self->EmitVariable(arg.m_index, OpCode::SET_LOCAL, m_line);
		}

		void operator()(const MidoriExpression::NameContext::Global&) const
		{
			const std::string& name = m_bind->m_name.m_lexeme;

			if (name.find(NameSeparator) != std::string::npos)
			{
				size_t separator_pos = name.find(NameSeparator);
				std::string module_name = name.substr(0u, separator_pos);
				std::string symbol_name = name.substr(separator_pos + 2u);
				int import_placeholder = m_self->GetImportPlaceholder(module_name, symbol_name, m_line);
				if (import_placeholder < 0)
				{
					return;
				}

				m_self->EmitVariable(import_placeholder, OpCode::SET_GLOBAL, m_line);
			}
			else
			{
				m_self->EmitVariable(m_self->m_global_variables[name], OpCode::SET_GLOBAL, m_line);
			}
		}

		void operator()(const MidoriExpression::NameContext::Cell& arg) const
		{
			m_self->EmitVariable(arg.m_index, m_self->GetCellStoreOpcode(), m_line);
		}
	};

	std::visit(AssignmentVisitor{ this, &bind, line }, bind.m_name_ctx);
}

void CodeGenerator::operator()(MidoriExpression::TextLiteral& text)
{
	EmitTextConstant(text.m_token.m_lexeme, text.m_token.m_line);
}

void CodeGenerator::operator()(MidoriExpression::BoolLiteral& bool_expr)
{
	int line = bool_expr.m_token.m_line;
	EmitByte(bool_expr.m_token.m_lexeme == "true"s ? OpCode::OP_TRUE : OpCode::OP_FALSE, line);
}

void CodeGenerator::operator()(MidoriExpression::FloatLiteral& float_literal)
{
	int line = float_literal.m_token.m_line;
	EmitFloatConstant(std::stod(float_literal.m_token.m_lexeme), line);
}

void CodeGenerator::operator()(MidoriExpression::IntegerLiteral& integer)
{
	int line = integer.m_token.m_line;
	try
	{
		EmitIntegerConstant(std::stoll(integer.m_token.m_lexeme), line);
	}
	catch (const std::out_of_range&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Integer literal '" + integer.m_token.m_lexeme + "' is out of range. Maximum value is 9223372036854775807 (2^63 - 1), minimum value is -9223372036854775807.", integer.m_token, m_file_name, m_source_lines));
	}
	catch (const std::invalid_argument&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid integer literal '" + integer.m_token.m_lexeme + "'", integer.m_token, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::ByteLiteral& byte_literal)
{
	int line = byte_literal.m_token.m_line;
	const std::string& lexeme = byte_literal.m_token.m_lexeme;
	try
	{
		uint64_t value = 0u;
		if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X'))
		{
			value = std::stoull(lexeme, nullptr, 16);
		}
		else if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'b' || lexeme[1u] == 'B'))
		{
			value = std::stoull(lexeme, nullptr, 2);
		}
		else
		{
			value = std::stoull(lexeme);
		}

		if (value > 0xFF)
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Byte literal '" + lexeme + "' is out of range. Maximum value is 255 (0xFF).", byte_literal.m_token, m_file_name, m_source_lines));
			return;
		}
		EmitByteConstant(static_cast<MidoriByte>(value), line);
	}
	catch (const std::out_of_range&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Byte literal '" + lexeme + "' is out of range. Maximum value is 255 (0xFF).", byte_literal.m_token, m_file_name, m_source_lines));
	}
	catch (const std::invalid_argument&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid byte literal '" + lexeme + "'", byte_literal.m_token, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::WordLiteral& word_literal)
{
	int line = word_literal.m_token.m_line;
	const std::string& lexeme = word_literal.m_token.m_lexeme;
	try
	{
		uint64_t value = 0u;
		if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'x' || lexeme[1u] == 'X'))
		{
			value = std::stoull(lexeme, nullptr, 16);
		}
		else if (lexeme.size() >= 3 && lexeme[0u] == '0' && (lexeme[1u] == 'b' || lexeme[1u] == 'B'))
		{
			value = std::stoull(lexeme, nullptr, 2);
		}
		else
		{
			value = std::stoull(lexeme);
		}
		EmitWordConstant(value, line);
	}
	catch (const std::out_of_range&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Word literal '" + lexeme + "' is out of range. Maximum value is 18446744073709551615 (0xFFFFFFFFFFFFFFFF).", word_literal.m_token, m_file_name, m_source_lines));
	}
	catch (const std::invalid_argument&)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid word literal '" + lexeme + "'", word_literal.m_token, m_file_name, m_source_lines));
	}
}

void CodeGenerator::operator()(MidoriExpression::UnitLiteral& unit)
{
	int line = unit.m_token.m_line;
	EmitByte(OpCode::OP_UNIT, line);
}

void CodeGenerator::operator()(MidoriExpression::Function& function)
{
	int line = function.m_function_keyword.m_line;
	EmitFunction(function.m_params, function.m_body, "Anonymous Function at line: " + std::to_string(line), line, function.m_captured_count);
}

void CodeGenerator::operator()(MidoriExpression::Construct& construct)
{
	int line = construct.m_data_name.m_line;
	OpCode size = static_cast<OpCode>(construct.m_params.size());
	bool is_struct = std::holds_alternative<MidoriExpression::Construct::Struct>(construct.m_construct_ctx);

	std::ranges::for_each
	(
		construct.m_params,
		[this](std::unique_ptr<MidoriExpression>& param)
		{
			Visit(param);
		}
	);

	if (is_struct)
	{
		EmitByte(OpCode::CONSTRUCT_STRUCT, line);
	}
	else
	{
		EmitByte(OpCode::CONSTRUCT_UNION, line);
	}
	EmitByte(size, line);

	if (!is_struct)
	{
		int tag = std::get<MidoriExpression::Construct::Union>(construct.m_construct_ctx).m_index;

		if (tag > MAX_UNION_TAG)
		{
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Union tag too large (max {})", MAX_UNION_TAG + 1), construct.m_data_name, m_file_name, m_source_lines));
			return;
		}

		EmitByte(OpCode::SET_TAG, line);
		EmitByte(static_cast<OpCode>(tag), line);
	}
}

void CodeGenerator::operator()(MidoriExpression::Array& array)
{
	int line = array.m_op.m_line;

	int length = static_cast<int>(array.m_elems.size());
	if (length > MAX_ARRAY_SIZE)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many array elements (max {})", MAX_ARRAY_SIZE + 1), array.m_op, m_file_name, m_source_lines));
		return;
	}

	std::ranges::for_each
	(
		array.m_elems,
		[this](std::unique_ptr<MidoriExpression>& elem)
		{
			Visit(elem);
		}
	);
	EmitByte(OpCode::CREATE_ARRAY, line);
	EmitThreeBytes(length, length >> 8, length >> 16, line);
}

void CodeGenerator::operator()(MidoriExpression::IndexAccess& array_get)
{
	int line = array_get.m_op.m_line;

	if (array_get.m_indices.size() > MAX_NESTED_ARRAY_INDEX)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many array indices (max {})", MAX_NESTED_ARRAY_INDEX + 1), array_get.m_op, m_file_name, m_source_lines));
		return;
	}

	Visit(array_get.m_arr_var);

	std::ranges::for_each
	(
		array_get.m_indices,
		[this](std::unique_ptr<MidoriExpression>& index)
		{
			Visit(index);
		}
	);

	EmitByte(OpCode::GET_ARRAY, line);
	EmitByte(static_cast<OpCode>(array_get.m_indices.size()), line);
}

void CodeGenerator::operator()(MidoriExpression::IndexAssignment& array_set)
{
	int line = array_set.m_op.m_line;

	if (array_set.m_indices.size() > MAX_NESTED_ARRAY_INDEX)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many array indices (max {})", MAX_NESTED_ARRAY_INDEX + 1), array_set.m_op, m_file_name, m_source_lines));
		return;
	}

	Visit(array_set.m_arr_var);

	std::ranges::for_each
	(
		array_set.m_indices,
		[this](std::unique_ptr<MidoriExpression>& index)
		{
			Visit(index);
		}
	);

	Visit(array_set.m_value);

	EmitByte(OpCode::SET_ARRAY, line);
	EmitByte(static_cast<OpCode>(array_set.m_indices.size()), line);
}

void CodeGenerator::operator()(MidoriExpression::RangeBinary& range_binary)
{
	int line = range_binary.m_range_op.m_line;

	Visit(range_binary.m_start);

	// Generate code for default step (1 for Int, 1.0 for Float)
	if (range_binary.m_type_data->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::IntegerType>())
	{
		EmitByte(OpCode::INT_1, line);
	}
	else
	{
		EmitFloatConstant(1.0, line);
	}

	Visit(range_binary.m_end);

	if (range_binary.m_type_data->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::IntegerType>())
	{
		EmitByte(OpCode::CREATE_INT_RANGE, line);
	}
	else
	{
		EmitByte(OpCode::CREATE_FLOAT_RANGE, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::RangeTernary& range_ternary)
{
	int line = range_ternary.m_first_range_op.m_line;

	Visit(range_ternary.m_start);
	Visit(range_ternary.m_step);
	Visit(range_ternary.m_end);

	if (range_ternary.m_type_data->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::IntegerType>())
	{
		EmitByte(OpCode::CREATE_INT_RANGE, line);
	}
	else
	{
		EmitByte(OpCode::CREATE_FLOAT_RANGE, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::IfElse& if_else)
{
	int line = if_else.m_if_token.m_line;
	Visit(if_else.m_condition);

	if (if_else.m_condition_operand_type == MidoriExpression::ConditionOperandType::INTEGER || if_else.m_condition_operand_type == MidoriExpression::ConditionOperandType::FLOAT)
	{
		EmitNumericConditionalJump(if_else.m_condition_operand_type, if_else.m_true_branch, if_else.m_else_branch, line);
	}
	else
	{
		int jump_if_false = EmitJump(OpCode::JUMP_IF_FALSE, line);
		EmitByte(OpCode::POP, line);
		Visit(if_else.m_true_branch);
		int jump = EmitJump(OpCode::JUMP, line);
		PatchJump(jump_if_false, line);
		EmitByte(OpCode::POP, line);
		Visit(if_else.m_else_branch);
		PatchJump(jump, line);
	}
}

void CodeGenerator::operator()(MidoriExpression::Block& block)
{
	std::ranges::for_each
	(
		block.m_stmts,
		[this](std::unique_ptr<MidoriStatement>& statement)
		{
			Visit(statement);
		}
	);

	// Discard everything else when encountered "return"
	if (!m_builder.m_procedures[m_builder.m_current_procedure_index].IsByteCodeEmpty() && m_builder.m_procedures[m_builder.m_current_procedure_index].ReadByteCode(m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize() - 1) == OpCode::RETURN)
	{
		return;
	}
	else
	{
		if (block.m_final_expr.has_value())
		{
			Visit(*block.m_final_expr);
		}
		else
		{
			EmitByte(OpCode::OP_UNIT, block.m_right_brace.m_line);
		}

		while (block.m_local_count > 0)
		{
			int count_to_pop = std::min(block.m_local_count, static_cast<int>(UINT8_MAX));

			if (count_to_pop == block.m_local_count)
			{
				EmitByte(OpCode::POP_BLOCK_SCOPE, block.m_right_brace.m_line);
			}
			else
			{
				EmitByte(OpCode::POP_LOCAL_SCOPE, block.m_right_brace.m_line);
			}

			EmitByte(static_cast<OpCode>(count_to_pop), block.m_right_brace.m_line);
			block.m_local_count -= count_to_pop;
		}
	}
}

void CodeGenerator::operator()(MidoriExpression::Match& match)
{
	int line = match.m_match_keyword.m_line;
	if (match.m_match_value_index < 0)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Match expression missing hidden value slot", match.m_match_keyword, m_file_name, m_source_lines));
		return;
	}

	EmitByte(OpCode::PUSH_PLACEHOLDER, line);
	if (m_local_count < match.m_match_value_index + 1)
	{
		m_local_count = match.m_match_value_index + 1;
	}

	Visit(match.m_arg_expr);
	EmitVariable(match.m_match_value_index, OpCode::SET_LOCAL, line);
	EmitByte(OpCode::POP, line);

	std::vector<int> end_jumps;
	for (std::unique_ptr<MidoriExpression>& case_expr : match.m_cases)
	{
		if (case_expr->IsExpression<MidoriExpression::Default>())
		{
			Visit(case_expr);
			end_jumps.emplace_back(EmitJump(OpCode::JUMP, line));
			break;
		}

		MidoriExpression::Case& match_case = case_expr->GetExpression<MidoriExpression::Case>();
		std::vector<int> failure_jumps;
		EmitVariable(match.m_match_value_index, OpCode::GET_LOCAL, line);
		EmitPatternCheck(*match_case.m_pattern, failure_jumps, 0);

		int binding_count = match_case.m_binding_count;
		for (int i = 0; i < binding_count; i += 1)
		{
			EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		}

		EmitVariable(match.m_match_value_index, OpCode::GET_LOCAL, line);
		EmitPatternBind(*match_case.m_pattern);

		Visit(match_case.m_expr);

		if (binding_count > 0)
		{
			int remaining = binding_count;
			while (remaining > 0)
			{
				int count_to_pop = std::min(remaining, static_cast<int>(UINT8_MAX));
				if (count_to_pop == remaining)
				{
					EmitByte(OpCode::POP_MATCH_SCOPE, line);
				}
				else
				{
					EmitByte(OpCode::POP_VALUES, line);
				}
				EmitByte(static_cast<OpCode>(count_to_pop), line);
				remaining -= count_to_pop;
			}
		}

		end_jumps.emplace_back(EmitJump(OpCode::JUMP, line));

		for (int jump_addr : failure_jumps)
		{
			PatchJump(jump_addr, line);
		}
	}

	for (int jump_addr : end_jumps)
	{
		PatchJump(jump_addr, line);
	}

	EmitByte(OpCode::SWAP, line);
	EmitByte(OpCode::POP, line);
}

void CodeGenerator::operator()(MidoriExpression::Case& case_expr)
{
	Visit(case_expr.m_expr);
}

void CodeGenerator::operator()(MidoriExpression::Default& default_expr)
{
	Visit(default_expr.m_expr);
}

void CodeGenerator::operator()(MidoriExpression::Loop& loop)
{
	int line = loop.m_loop_keyword.m_line;

	int loop_start = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
	BeginLoop(loop_start);

	Visit(loop.m_body);
	EmitByte(OpCode::POP, line);

	EmitLoop(loop_start, line);
	EndLoop(line);
}

void CodeGenerator::operator()(MidoriExpression::For& for_expr)
{
	int line = for_expr.m_for_keyword.m_line;

	if (for_expr.m_is_iterable_iteration)
	{
		if (m_local_count < for_expr.m_hidden_array_index + 1)
		{
			m_local_count = for_expr.m_hidden_array_index + 1;
		}

		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // loop variable
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // unused (step)
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // unused (end)
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // iterator ref

		Visit(for_expr.m_range);
		EmitVariable(for_expr.m_hidden_array_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		int loop_start = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		BeginLoop(loop_start);
		m_loop_contexts.top().m_continue_target = loop_start;

		std::shared_ptr<MidoriType> iter_type = GetConcreteTypeForExpression(for_expr.m_range);
		std::shared_ptr<MidoriType> item_type = for_expr.m_iterable_item_type;
		if (!m_generic_type_substitution.empty() && item_type)
		{
			item_type = SubstituteGenericTypes(item_type, m_generic_type_substitution);
		}

		EmitVariable(for_expr.m_hidden_array_index, OpCode::GET_LOCAL, line);
		if (!EmitIterableNextCall(iter_type, item_type, line))
		{
			return;
		}

		EmitByte(OpCode::DUP, line);
		EmitByte(OpCode::GET_TAG, line);
		EmitIntegerConstant(static_cast<MidoriInteger>(for_expr.m_iterable_some_tag), line);
		int exit_jump = EmitJump(OpCode::IF_INTEGER_EQUAL, line);

		EmitByte(OpCode::LOAD_TAG, line);
		EmitByte(OpCode::POP, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		Visit(for_expr.m_body);
		EmitByte(OpCode::POP, line);

		EmitLoop(loop_start, line);

		PatchJump(exit_jump, line);
		EmitByte(OpCode::POP, line);  // Pop Option

		EmitByte(OpCode::POP, line);  // Pop iterator ref
		EmitByte(OpCode::POP, line);  // Pop unused end
		EmitByte(OpCode::POP, line);  // Pop unused step
		EmitByte(OpCode::POP, line);  // Pop loop variable

		EmitByte(OpCode::OP_UNIT, line);

		EndLoop(line);
	}
	else if (for_expr.m_is_array_iteration)
	{
		// Array iteration
		// The parser has reserved 4 local variable slots:
		// for_expr.m_loop_variable_index: the element value
		// for_expr.m_hidden_step_index: current index (reused)
		// for_expr.m_hidden_end_index: array length (reused)
		// for_expr.m_hidden_array_index: array reference

		// Update m_local_count to account for the 4 reserved locals
		if (m_local_count < for_expr.m_hidden_array_index + 1)
		{
			m_local_count = for_expr.m_hidden_array_index + 1;
		}

		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // loop variable (element)
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // index
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // length
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // array ref

		// Evaluate array expression
		Visit(for_expr.m_range);
		// Stack: [0, 0, 0, 0, array]

		// Store array reference
		EmitByte(OpCode::DUP, line);
		EmitVariable(for_expr.m_hidden_array_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
		// Stack: [0, 0, 0, array, array]

		// Get and store array length
		EmitByte(OpCode::GET_ARRAY_LENGTH, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
		// Stack: [0, 0, length, array]

		// Initialize index to 0
		EmitByte(OpCode::INT_0, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);
		// Stack: [0, 0, length, array]

		// Jump to condition check
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment index
		int continue_target = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::INT_1, line);
		EmitByte(OpCode::ADD_INTEGER, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);
		int loop_start = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		BeginLoop(loop_start);

		m_loop_contexts.top().m_continue_target = continue_target;

		// Check if index < length
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::GET_LOCAL, line);
		int exit_jump = EmitJump(OpCode::IF_INTEGER_LESS, line);

		// Get element at current index and store in loop variable
		EmitVariable(for_expr.m_hidden_array_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::GET_ARRAY, line);
		EmitByte(static_cast<OpCode>(1), line);  // 1 index dimension
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Execute body
		Visit(for_expr.m_body);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(exit_jump, line);

		EmitByte(OpCode::POP, line);  // Pop array ref
		EmitByte(OpCode::POP, line);  // Pop length
		EmitByte(OpCode::POP, line);  // Pop index
		EmitByte(OpCode::POP, line);  // Pop loop variable

		// Push unit value as result for normal loop exit
		EmitByte(OpCode::OP_UNIT, line);

		EndLoop(line);
	}
	else
	{
		// Range iteration (original code)
		bool is_float = for_expr.m_range->GetType()->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::FloatType>();

		// The parser has reserved 4 local variable slots (but we only use 3 for range)
		// for_expr.m_loop_variable_index: the loop variable
		// for_expr.m_hidden_step_index: hidden step value
		// for_expr.m_hidden_end_index: hidden end value

		// Update m_local_count to account for the 4 reserved locals
		if (m_local_count < for_expr.m_hidden_array_index + 1)
		{
			m_local_count = for_expr.m_hidden_array_index + 1;
		}

		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);
		EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // Extra placeholder for consistency

		Visit(for_expr.m_range);
		// Stack: [0, 0, 0, 0, range]

		// Duplicate range for each extraction
		EmitByte(OpCode::DUP, line);
		EmitByte(OpCode::DUP, line);
		// Stack: [0, 0, 0, 0, range, range, range]

		EmitByte(OpCode::GET_RANGE_START, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_STEP, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_END, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Jump to condition check (skip increment on first iteration)
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment loop variable before checking condition
		int continue_target = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		EmitVariable(for_expr.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(is_float ? OpCode::ADD_FLOAT : OpCode::ADD_INTEGER, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);
		int loop_start = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		BeginLoop(loop_start);

		m_loop_contexts.top().m_continue_target = continue_target;

		// Runtime check for step direction to support both forward and backward iteration
		EmitVariable(for_expr.m_hidden_step_index, OpCode::GET_LOCAL, line);
		if (is_float)
		{
			EmitFloatConstant(0.0, line);
		}
		else
		{
			EmitByte(OpCode::INT_0, line);
		}

		OpCode step_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_jump = EmitJump(step_comparison, line);

		// Forward iteration (step > 0): exit loop if i >= end
		EmitVariable(for_expr.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode forward_comparison = is_float ? OpCode::IF_FLOAT_LESS : OpCode::IF_INTEGER_LESS;
		int forward_exit_jump = EmitJump(forward_comparison, line);

		int body_jump = EmitJump(OpCode::JUMP, line);

		// Backward iteration (step <= 0): exit loop if i <= end
		PatchJump(backward_jump, line);
		EmitVariable(for_expr.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(for_expr.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode backward_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_exit_jump = EmitJump(backward_comparison, line);

		PatchJump(body_jump, line);
		Visit(for_expr.m_body);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(forward_exit_jump, line);
		PatchJump(backward_exit_jump, line);

		EmitByte(OpCode::POP, line);  // Pop unused array slot
		EmitByte(OpCode::POP, line);  // Pop end
		EmitByte(OpCode::POP, line);  // Pop step
		EmitByte(OpCode::POP, line);  // Pop loop variable

		// Push unit value as result for normal loop exit
		EmitByte(OpCode::OP_UNIT, line);

		EndLoop(line);
	}
}

void CodeGenerator::operator()(MidoriExpression::ArrayComprehension& comp)
{
	int line = comp.m_bracket.m_line;

	// Update m_local_count to account for the 5 reserved locals
	if (m_local_count < comp.m_result_array_index + 1)
	{
		m_local_count = comp.m_result_array_index + 1;
	}

	// Push 5 placeholders for: loop_var, step/index, end/length, array_ref, result_array
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // loop variable
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // step/index
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // end/length
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // array ref (for array iteration)
	EmitByte(OpCode::PUSH_PLACEHOLDER, line);  // result array

	// Create empty result array and store it
	EmitByte(OpCode::CREATE_ARRAY, line);
	EmitThreeBytes(0, 0, 0, line);  // Empty array with 0 elements
	EmitVariable(comp.m_result_array_index, OpCode::SET_LOCAL, line);
	EmitByte(OpCode::POP, line);

	if (comp.m_is_iterable_iteration)
	{
		Visit(comp.m_range);
		EmitVariable(comp.m_hidden_array_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		int loop_start = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();

		std::shared_ptr<MidoriType> iter_type = GetConcreteTypeForExpression(comp.m_range);
		std::shared_ptr<MidoriType> item_type = comp.m_iterable_item_type;
		if (!m_generic_type_substitution.empty() && item_type)
		{
			item_type = SubstituteGenericTypes(item_type, m_generic_type_substitution);
		}

		EmitVariable(comp.m_hidden_array_index, OpCode::GET_LOCAL, line);
		if (!EmitIterableNextCall(iter_type, item_type, line))
		{
			return;
		}

		EmitByte(OpCode::DUP, line);
		EmitByte(OpCode::GET_TAG, line);
		EmitIntegerConstant(static_cast<MidoriInteger>(comp.m_iterable_some_tag), line);
		int exit_jump = EmitJump(OpCode::IF_INTEGER_EQUAL, line);

		EmitByte(OpCode::LOAD_TAG, line);
		EmitByte(OpCode::POP, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		Visit(comp.m_transform_expr);
		EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::SWAP, line);
		EmitByte(OpCode::ADD_BACK_ARRAY, line);
		EmitByte(OpCode::POP, line);

		EmitLoop(loop_start, line);

		PatchJump(exit_jump, line);
		EmitByte(OpCode::POP, line);  // Pop Option
	}
	else if (comp.m_is_array_iteration)
	{
		Visit(comp.m_range);

		// Store array reference
		EmitByte(OpCode::DUP, line);
		EmitVariable(comp.m_hidden_array_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Get and store array length
		EmitByte(OpCode::GET_ARRAY_LENGTH, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Initialize index to 0
		EmitByte(OpCode::INT_0, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Jump to condition check
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment index
		int continue_target = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::INT_1, line);
		EmitByte(OpCode::ADD_INTEGER, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);

		// Check if index < length
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::GET_LOCAL, line);
		int exit_jump = EmitJump(OpCode::IF_INTEGER_LESS, line);

		// Get element at current index and store in loop variable
		EmitVariable(comp.m_hidden_array_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::GET_ARRAY, line);
		EmitByte(static_cast<OpCode>(1), line);  // 1 index dimension
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Evaluate transform expression first, then get result array, swap and append
		Visit(comp.m_transform_expr);
		EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::SWAP, line);
		EmitByte(OpCode::ADD_BACK_ARRAY, line);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(exit_jump, line);
	}
	else
	{
		bool is_float = comp.m_range->GetType()->GetType<MidoriType::RangeType>().m_element_type->IsType<MidoriType::FloatType>();

		Visit(comp.m_range);

		// Duplicate range for each extraction
		EmitByte(OpCode::DUP, line);
		EmitByte(OpCode::DUP, line);

		EmitByte(OpCode::GET_RANGE_START, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_STEP, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		EmitByte(OpCode::GET_RANGE_END, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		// Jump to condition check
		int skip_first_increment = EmitJump(OpCode::JUMP, line);

		// Continue target: Increment loop variable
		int continue_target = m_builder.m_procedures[m_builder.m_current_procedure_index].GetByteCodeSize();
		EmitVariable(comp.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		EmitByte(is_float ? OpCode::ADD_FLOAT : OpCode::ADD_INTEGER, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::SET_LOCAL, line);
		EmitByte(OpCode::POP, line);

		PatchJump(skip_first_increment, line);

		// Runtime check for step direction
		EmitVariable(comp.m_hidden_step_index, OpCode::GET_LOCAL, line);
		if (is_float)
		{
			EmitFloatConstant(0.0, line);
		}
		else
		{
			EmitByte(OpCode::INT_0, line);
		}

		OpCode step_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_jump = EmitJump(step_comparison, line);

		// Forward iteration (step > 0): exit loop if i >= end
		EmitVariable(comp.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode forward_comparison = is_float ? OpCode::IF_FLOAT_LESS : OpCode::IF_INTEGER_LESS;
		int forward_exit_jump = EmitJump(forward_comparison, line);

		int body_jump = EmitJump(OpCode::JUMP, line);

		// Backward iteration (step <= 0): exit loop if i <= end
		PatchJump(backward_jump, line);
		EmitVariable(comp.m_loop_variable_index, OpCode::GET_LOCAL, line);
		EmitVariable(comp.m_hidden_end_index, OpCode::GET_LOCAL, line);
		OpCode backward_comparison = is_float ? OpCode::IF_FLOAT_GREATER : OpCode::IF_INTEGER_GREATER;
		int backward_exit_jump = EmitJump(backward_comparison, line);

		PatchJump(body_jump, line);

		// Evaluate transform expression first, then get result array, swap and append
		Visit(comp.m_transform_expr);
		EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
		EmitByte(OpCode::SWAP, line);
		EmitByte(OpCode::ADD_BACK_ARRAY, line);
		EmitByte(OpCode::POP, line);

		EmitLoop(continue_target, line);

		PatchJump(forward_exit_jump, line);
		PatchJump(backward_exit_jump, line);
	}

	// Clean up: pop 4 placeholders and the result placeholder, then push result array
	// Stack: [..., loop_var, step, end, array_ref, result_placeholder]
	EmitByte(OpCode::POP, line);  // Pop array ref / unused slot
	EmitByte(OpCode::POP, line);  // Pop end/length
	EmitByte(OpCode::POP, line);  // Pop step/index
	EmitByte(OpCode::POP, line);  // Pop loop variable
	// Stack: [..., result_placeholder]

	// Pop the result placeholder and push the actual result array
	EmitByte(OpCode::POP, line);  // Pop result_placeholder
	// Stack: [...]

	// Push the result array as the expression value
	EmitVariable(comp.m_result_array_index, OpCode::GET_LOCAL, line);
	// Stack: [..., result_array]
}

void CodeGenerator::operator()(MidoriExpression::Break& break_expr)
{
	int line = break_expr.m_keyword.m_line;

	Visit(break_expr.m_value);

	while (break_expr.m_number_to_pop > 0)
	{
		int count_to_pop = std::min(break_expr.m_number_to_pop, static_cast<int>(UINT8_MAX));
		if (count_to_pop == break_expr.m_number_to_pop)
		{
			EmitByte(OpCode::POP_BLOCK_SCOPE, line);
		}
		else
		{
			EmitByte(OpCode::POP_LOCAL_SCOPE, line);
		}
		EmitByte(static_cast<OpCode>(count_to_pop), line);
		break_expr.m_number_to_pop -= count_to_pop;
	}

	m_loop_contexts.top().m_break_positions.emplace_back(EmitJump(OpCode::BREAK, line));
}

void CodeGenerator::operator()(MidoriExpression::Return& return_expr)
{
	int line = return_expr.m_keyword.m_line;
	Visit(return_expr.m_value);
	EmitByte(OpCode::RETURN, line);
}

void CodeGenerator::operator()(MidoriExpression::Async& async_expr)
{
	int line = async_expr.m_keyword.m_line;
	int captured_count = async_expr.m_captured_count;
	m_has_async = true;
	EnsureProcedureMetadataSize(m_builder.m_current_procedure_index);
	m_shared_cell_procedure_flags[m_builder.m_current_procedure_index] = true;

	if (captured_count > MAX_CAPTURED_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many captured variables (max {})", MAX_CAPTURED_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	size_t prev_index = m_builder.m_current_procedure_index;
	const size_t async_proc_index = m_builder.m_procedures.size();
	m_builder.m_current_procedure_index = async_proc_index;
	m_builder.m_procedures.emplace_back();
	EnsureProcedureMetadataSize(async_proc_index);
	m_shared_cell_procedure_flags[async_proc_index] = true;
	m_procedure_capture_counts[async_proc_index] = captured_count;

	Visit(async_expr.m_expr);

	EmitByte(OpCode::ASYNC_RETURN, line);

	std::string full_name = "async_task@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_builder.m_procedure_names.emplace_back(full_name.c_str());

	m_builder.m_current_procedure_index = prev_index;

	if (m_builder.m_current_procedure_index > MAX_FUNCTION_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many functions (max {})", MAX_FUNCTION_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	EmitByte(OpCode::MAKE_CLOSURE, line);
	EmitByte(static_cast<OpCode>(async_proc_index), line);

	NoteCaptureBinding(captured_count, true);
	EmitByte(OpCode::BIND_CAPTURES_SHARED, line);
	EmitByte(static_cast<OpCode>(captured_count), line);

	EmitByte(OpCode::SPAWN_ASYNC, line);
}

void CodeGenerator::operator()(MidoriExpression::Await& await_expr)
{
	int line = await_expr.m_keyword.m_line;
	m_has_async = true;
	EnsureProcedureMetadataSize(m_builder.m_current_procedure_index);
	m_shared_cell_procedure_flags[m_builder.m_current_procedure_index] = true;
	Visit(await_expr.m_expr);
	EmitByte(OpCode::AWAIT_FUTURE, line);
}

void CodeGenerator::EmitNumericConditionalJump(MidoriExpression::ConditionOperandType operand_type, std::unique_ptr<MidoriExpression>& true_branch, std::unique_ptr<MidoriExpression>& else_branch, int line)
{
	int if_jump;
	if (operand_type == MidoriExpression::ConditionOperandType::INTEGER)
	{
		PopByte(line);
		switch (m_builder.m_last_opcode)
		{
		case OpCode::LESS_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_LESS, line);
			break;
		case OpCode::LESS_EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_LESS_EQUAL, line);
			break;
		case OpCode::GREATER_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_GREATER, line);
			break;
		case OpCode::GREATER_EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_GREATER_EQUAL, line);
			break;
		case OpCode::EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_EQUAL, line);
			break;
		case OpCode::NOT_EQUAL_INTEGER:
			if_jump = EmitJump(OpCode::IF_INTEGER_NOT_EQUAL, line);
			break;
		default:
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid opcode for integer ternary condition", line, m_file_name, m_source_lines));
			return;
		}

		Visit(true_branch);

		int else_jump = EmitJump(OpCode::JUMP, line);
		PatchJump(if_jump, line);
		if (else_branch != nullptr)
		{
			Visit(else_branch);
		}
		PatchJump(else_jump, line);
	}
	else
	{
		PopByte(line);
		switch (m_builder.m_last_opcode)
		{
		case OpCode::LESS_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_LESS, line);
			break;
		case OpCode::LESS_EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_LESS_EQUAL, line);
			break;
		case OpCode::GREATER_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_GREATER, line);
			break;
		case OpCode::GREATER_EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_GREATER_EQUAL, line);
			break;
		case OpCode::EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_EQUAL, line);
			break;
		case OpCode::NOT_EQUAL_FLOAT:
			if_jump = EmitJump(OpCode::IF_FLOAT_NOT_EQUAL, line);
			break;
		default:
			AddError(MidoriError::GenerateCodeGeneratorErrorWithContext("Invalid opcode for float ternary condition", line, m_file_name, m_source_lines));
			return;
		}

		Visit(true_branch);

		int else_jump = EmitJump(OpCode::JUMP, line);
		PatchJump(if_jump, line);
		if (else_branch != nullptr)
		{
			Visit(else_branch);
		}
		PatchJump(else_jump, line);
	}
}

bool CodeGenerator::IsGenericType(const std::shared_ptr<MidoriType>& type)
{
	struct GenericTypeVisitor
	{
		CodeGenerator* m_self = nullptr;

		bool operator()(const MidoriType::TypeVariable&) const { return true; }
		bool operator()(const MidoriType::FunctionType& type_variant) const
		{
			bool result = m_self->IsGenericType(type_variant.m_return_type);
			for (const std::shared_ptr<MidoriType>& param_type : type_variant.m_param_types)
			{
				result = result || m_self->IsGenericType(param_type);
			}
			return result;
		}
		bool operator()(const MidoriType::ArrayType& type_variant) const
		{
			return m_self->IsGenericType(type_variant.m_element_type);
		}
		bool operator()(const MidoriType::StructType& type_variant) const
		{
			for (const std::shared_ptr<MidoriType>& member_type : type_variant.m_member_types)
			{
				if (m_self->IsGenericType(member_type))
				{
					return true;
				}
			}
			return false;
		}
		bool operator()(const MidoriType::UnionType& type_variant) const
		{
			for (const std::unordered_map<std::string, MidoriType::UnionType::UnionMemberContext>::value_type& member_pair : type_variant.m_member_info)
			{
				for (const std::shared_ptr<MidoriType>& member_type : member_pair.second.m_member_types)
				{
					if (m_self->IsGenericType(member_type))
					{
						return true;
					}
				}
			}
			return false;
		}

		bool operator()(const MidoriType::UndecidedType&) const { return false; }
		bool operator()(const MidoriType::GenericParam&) const { return false; }
		bool operator()(const MidoriType::FloatType&) const { return false; }
		bool operator()(const MidoriType::IntegerType&) const { return false; }
		bool operator()(const MidoriType::ByteType&) const { return false; }
		bool operator()(const MidoriType::WordType&) const { return false; }
		bool operator()(const MidoriType::TextType&) const { return false; }
		bool operator()(const MidoriType::BoolType&) const { return false; }
		bool operator()(const MidoriType::UnitType&) const { return false; }
		bool operator()(const MidoriType::NeverType&) const { return false; }
		bool operator()(const MidoriType::RangeType&) const { return false; }
		bool operator()(const MidoriType::FutureType&) const { return false; }
		bool operator()(const MidoriType::TupleType&) const { return false; }
		bool operator()(const MidoriType::ClassConstraint&) const { return false; }
	};

	return std::visit(GenericTypeVisitor{ this }, type->m_type);
}

// Helper for type deduction
void CodeGenerator::DeduceGenericTypesRecursive(const std::shared_ptr<MidoriType>& param_type, const std::shared_ptr<MidoriType>& concrete_type, std::unordered_map<std::string, std::shared_ptr<MidoriType>>& map, std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash>& visited)
{
	if (!param_type || !concrete_type)
	{
		return;
	}
	if (param_type.get() == concrete_type.get())
	{
		return;
	}
	if (visited.contains({param_type.get(), concrete_type.get()}))
	{
		return;
	}
	visited.insert({ param_type.get(), concrete_type.get() });

	struct DeduceGenericVisitor
	{
		CodeGenerator* m_self = nullptr;
		const std::shared_ptr<MidoriType>& m_param_type;
		const std::shared_ptr<MidoriType>& m_concrete_type;
		std::unordered_map<std::string, std::shared_ptr<MidoriType>>& m_map;
		std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash>& m_visited;

		void operator()(const MidoriType::GenericParam& p_var) const
		{
			m_map[p_var.m_name] = m_concrete_type;
		}

		void operator()(const MidoriType::TypeVariable&) const
		{
			m_map[m_param_type->ToString()] = m_concrete_type;
		}

		void operator()(const MidoriType::ArrayType& p_var) const
		{
			if (m_concrete_type->IsType<MidoriType::ArrayType>())
			{
				m_self->DeduceGenericTypesRecursive(p_var.m_element_type, m_concrete_type->GetType<MidoriType::ArrayType>().m_element_type, m_map, m_visited);
			}
		}

		void operator()(const MidoriType::StructType& p_var) const
		{
			if (m_concrete_type->IsType<MidoriType::StructType>())
			{
				const MidoriType::StructType& c_struct = m_concrete_type->GetType<MidoriType::StructType>();
				if (p_var.m_member_types.size() == c_struct.m_member_types.size())
				{
					for (size_t i = 0uz; i < p_var.m_member_types.size(); i += 1uz)
					{
						m_self->DeduceGenericTypesRecursive(p_var.m_member_types[i], c_struct.m_member_types[i], m_map, m_visited);
					}
				}
			}
		}

		void operator()(const MidoriType::FunctionType& p_var) const
		{
			if (m_concrete_type->IsType<MidoriType::FunctionType>())
			{
				const MidoriType::FunctionType& c_func = m_concrete_type->GetType<MidoriType::FunctionType>();
				m_self->DeduceGenericTypesRecursive(p_var.m_return_type, c_func.m_return_type, m_map, m_visited);
				if (p_var.m_param_types.size() == c_func.m_param_types.size())
				{
					for (size_t i = 0uz; i < p_var.m_param_types.size(); i += 1uz)
					{
						m_self->DeduceGenericTypesRecursive(p_var.m_param_types[i], c_func.m_param_types[i], m_map, m_visited);
					}
				}
			}
		}

		void operator()(const MidoriType::TupleType& p_var) const
		{
			if (m_concrete_type->IsType<MidoriType::TupleType>())
			{
				const MidoriType::TupleType& c_tuple = m_concrete_type->GetType<MidoriType::TupleType>();
				if (p_var.m_element_types.size() == c_tuple.m_element_types.size())
				{
					for (size_t i = 0uz; i < p_var.m_element_types.size(); i += 1uz)
					{
						m_self->DeduceGenericTypesRecursive(p_var.m_element_types[i], c_tuple.m_element_types[i], m_map, m_visited);
					}
				}
			}
		}

		void operator()(const MidoriType::UnionType& p_var) const
		{
			if (m_concrete_type->IsType<MidoriType::UnionType>())
			{
				const MidoriType::UnionType& c_union = m_concrete_type->GetType<MidoriType::UnionType>();
				for (const auto& [name, ctx] : p_var.m_member_info)
				{
					if (c_union.m_member_info.contains(name))
					{
						const MidoriType::UnionType::UnionMemberContext& c_ctx = c_union.m_member_info.at(name);
						if (ctx.m_member_types.size() == c_ctx.m_member_types.size())
						{
							for (size_t i = 0uz; i < ctx.m_member_types.size(); i += 1uz)
							{
								m_self->DeduceGenericTypesRecursive(ctx.m_member_types[i], c_ctx.m_member_types[i], m_map, m_visited);
							}
						}
					}
				}
			}
		}

		void operator()(const MidoriType::UndecidedType&) const {}
		void operator()(const MidoriType::FloatType&) const {}
		void operator()(const MidoriType::IntegerType&) const {}
		void operator()(const MidoriType::ByteType&) const {}
		void operator()(const MidoriType::WordType&) const {}
		void operator()(const MidoriType::TextType&) const {}
		void operator()(const MidoriType::BoolType&) const {}
		void operator()(const MidoriType::UnitType&) const {}
		void operator()(const MidoriType::NeverType&) const {}
		void operator()(const MidoriType::RangeType&) const {}
		void operator()(const MidoriType::FutureType&) const {}
		void operator()(const MidoriType::ClassConstraint&) const {}
	};

	std::visit(DeduceGenericVisitor{ this, param_type, concrete_type, map, visited }, param_type->m_type);
}

int CodeGenerator::SpecializeGenericFunction(const std::string& base_name, const std::vector<std::shared_ptr<MidoriType>>& concrete_arg_types, int line)
{
	std::vector<std::string> concrete_type_names;
	for (const std::shared_ptr<MidoriType>& arg_type : concrete_arg_types)
	{
		concrete_type_names.push_back(arg_type->ToString());
	}
	FunctionSignature signature{ base_name, concrete_type_names };

	std::unordered_map<FunctionSignature, int, FunctionSignatureHash>::iterator it = m_specialized_functions.find(signature);
	if (it != m_specialized_functions.end())
	{
		return it->second;
	}

	std::unordered_map<std::string, GenericFunctionInfo>::iterator generic_it = m_generic_functions.find(base_name);
	if (generic_it == m_generic_functions.end())
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Generic function '{}' not found", base_name), line, m_file_name, m_source_lines));
		return -1;
	}

	GenericFunctionInfo& generic_info = generic_it->second;
	std::string specialized_name = base_name + "<"s;
	for (size_t i = 0u; i < concrete_type_names.size(); i += 1u)
	{
		if (i > 0u)
		{
			specialized_name += ","s;
		}
		specialized_name += concrete_type_names[i];
	}
	specialized_name += ">"s;

	// Build parameter name -> concrete type map
	TypeEnvironment prev_param_map = m_param_type_map;
	m_param_type_map.clear();

	for (size_t i = 0u; i < generic_info.m_params.size() && i < concrete_arg_types.size(); i += 1u)
	{
		m_param_type_map[generic_info.m_params[i].m_lexeme] = concrete_arg_types[i];
	}

	// Build generic type parameter -> concrete type map
	// Use deep deduction to match generic parameters and TypeVariables nested in arguments
	TypeEnvironment prev_generic_type_map = m_generic_type_substitution;
	m_generic_type_substitution.clear();
	TypeEnvironment& generic_type_map = m_generic_type_substitution;

	std::unordered_set<std::pair<MidoriType*, MidoriType*>, TypePairHash> visited;

	for (size_t i = 0u; i < generic_info.m_param_types.size() && i < concrete_arg_types.size(); i += 1u)
	{
		DeduceGenericTypesRecursive(generic_info.m_param_types[i], concrete_arg_types[i], generic_type_map, visited);
	}

	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>> prev_resolution_map = m_method_resolution_map;
	m_method_resolution_map.clear();

	for (const MidoriType::ClassConstraint& constraint : generic_info.m_constraints)
	{
		TypeclassMethodMap::iterator tc_it = m_class_methods.find(constraint.m_class_name);
		if (tc_it != m_class_methods.end())
		{
			std::vector<std::shared_ptr<MidoriType>> concrete_type_args;
			concrete_type_args.reserve(constraint.m_type_args.size());
			for (const std::shared_ptr<MidoriType>& type_arg : constraint.m_type_args)
			{
				concrete_type_args.emplace_back(SubstituteGenericTypes(type_arg, generic_type_map));
			}

			std::string first_type_name;
			std::string second_type_name;
			if (!concrete_type_args.empty())
			{
				first_type_name = concrete_type_args[0u]->ToString();
			}
			if (concrete_type_args.size() > 1u)
			{
				second_type_name = concrete_type_args[1u]->ToString();
			}

			for (const std::string& method_name : tc_it->second)
			{
				std::string qualified_method_name = constraint.m_class_name + std::string(NameSeparator) + method_name;

				std::string mangled_name_prefix = MidoriType::MangleInstanceMethodName(method_name, constraint.m_class_name, concrete_type_args);
				std::string resolved_method_name = mangled_name_prefix;
				bool instance_found = false;

				TypeclassInstanceMap::iterator instances_it = m_class_instances.find(constraint.m_class_name);
				if (instances_it != m_class_instances.end())
				{
					std::string pattern_with_at = mangled_name_prefix + ModuleSeparator;
					for (const std::string& instance_method : instances_it->second)
					{
						if (instance_method == mangled_name_prefix || instance_method.starts_with(pattern_with_at))
						{
							resolved_method_name = instance_method;
							instance_found = true;
							break;
						}
					}
				}

				ResolvedMethodCandidate candidate;
				candidate.m_first_type_name = first_type_name;
				candidate.m_second_type_name = second_type_name;
				candidate.m_resolved_name = resolved_method_name;
				candidate.m_has_instance = instance_found;

				m_method_resolution_map[qualified_method_name].emplace_back(std::move(candidate));
			}
		}
	}

	size_t prev_index = m_builder.m_current_procedure_index;
	const bool parent_uses_shared_cells = prev_index < m_shared_cell_procedure_flags.size() && m_shared_cell_procedure_flags[prev_index];
	const bool body_contains_async = generic_info.m_body && ExpressionContainsAsyncOrAwait(*generic_info.m_body);
	const bool inherit_shared_cells = parent_uses_shared_cells || body_contains_async;
	const size_t specialized_proc_index = m_builder.m_procedures.size();
	m_builder.m_current_procedure_index = specialized_proc_index;
	m_builder.m_procedures.emplace_back();
	EnsureProcedureMetadataSize(specialized_proc_index);
	m_shared_cell_procedure_flags[specialized_proc_index] = inherit_shared_cells;
	m_procedure_capture_counts[specialized_proc_index] = generic_info.m_captured_count;
	m_specialized_functions[signature] = static_cast<int>(specialized_proc_index);

	Visit(generic_info.m_body);
	EmitByte(OpCode::RETURN, line);

	std::string full_specialized_name = specialized_name + "@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_builder.m_procedure_names.emplace_back(full_specialized_name.c_str());

	m_builder.m_current_procedure_index = prev_index;

	m_param_type_map = std::move(prev_param_map);
	m_method_resolution_map = std::move(prev_resolution_map);
	m_generic_type_substitution = std::move(prev_generic_type_map);
	return static_cast<int>(specialized_proc_index);
}

std::optional<std::string> CodeGenerator::ResolveMethodNameForCall(const std::string& callee_name, const MidoriExpression::Call& call, int line)
{
	std::unordered_map<std::string, std::vector<ResolvedMethodCandidate>>::iterator it = m_method_resolution_map.find(callee_name);
	if (it == m_method_resolution_map.end())
	{
		return std::nullopt;
	}

	const std::vector<ResolvedMethodCandidate>& candidates = it->second;
	if (candidates.empty())
	{
		return std::nullopt;
	}

	if (call.m_arguments.empty())
	{
		if (candidates.size() == 1u && candidates[0u].m_has_instance)
		{
			return candidates[0u].m_resolved_name;
		}

		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': cannot resolve a method call with no arguments.", callee_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	if (call.m_arguments.empty())
	{
		if (candidates.size() == 1u && candidates[0u].m_has_instance)
		{
			return candidates[0u].m_resolved_name;
		}

		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': cannot resolve a method call with no arguments.", callee_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	std::shared_ptr<MidoriType> first_arg_type = GetConcreteTypeForExpression(call.m_arguments[0u]);
	std::string first_arg_type_name = first_arg_type->ToString();
	std::string return_type_name = call.m_type_data->ToString();

	std::vector<const ResolvedMethodCandidate*> matching;
	for (const ResolvedMethodCandidate& candidate : candidates)
	{
		if (candidate.m_first_type_name == first_arg_type_name)
		{
			matching.emplace_back(&candidate);
		}
	}

	if (matching.empty())
	{
		std::string candidates_info;
		for (const ResolvedMethodCandidate& candidate : candidates)
		{
			candidates_info += std::format("\nCandidate: {} (First: '{}', Instance: {})", candidate.m_resolved_name, candidate.m_first_type_name, candidate.m_has_instance);
		}
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': no constraint matches argument type '{}'. Candidates:{}", callee_name, first_arg_type_name, candidates_info), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	const bool return_type_is_concrete =
		!return_type_name.empty() &&
		return_type_name != "Undecided"s &&
		!(return_type_name.size() > 1u && return_type_name[0u] == 'T' && std::isdigit(static_cast<char>(return_type_name[1u])) != 0);

	if (matching.size() > 1u && return_type_is_concrete)
	{
		std::vector<const ResolvedMethodCandidate*> matching_by_return;
		for (const ResolvedMethodCandidate* candidate : matching)
		{
			if (!candidate->m_second_type_name.empty() && candidate->m_second_type_name == return_type_name)
			{
				matching_by_return.emplace_back(candidate);
			}
		}
		if (!matching_by_return.empty())
		{
			matching = std::move(matching_by_return);
		}
	}

	if (matching.size() != 1u)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Ambiguous method '{}': multiple constraints match argument type '{}'. Make constraints more specific.", callee_name, first_arg_type_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	const ResolvedMethodCandidate& selected = *matching[0u];
	if (!selected.m_has_instance)
	{
		std::string suffix;
		if (!selected.m_first_type_name.empty())
		{
			suffix = std::format(" (constraint types: {}", selected.m_first_type_name);
			if (!selected.m_second_type_name.empty())
			{
				suffix.append(", "s).append(selected.m_second_type_name);
			}
			suffix.append(")"s);
		}
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Unresolved method '{}': no matching instance found{}.", callee_name, suffix), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	return selected.m_resolved_name;
}

std::optional<int> CodeGenerator::ResolveResolvedNameGlobalIndex(const std::string& resolved_name, int line)
{
	size_t at_pos = resolved_name.find(ModuleSeparator);
	if (at_pos != std::string::npos)
	{
		std::string symbol_name = resolved_name.substr(0u, at_pos);
		std::string module_name = resolved_name.substr(at_pos + 1u);

		int import_placeholder = GetImportPlaceholder(module_name, symbol_name, line);
		if (import_placeholder < 0)
		{
			return std::nullopt;
		}
		return import_placeholder;
	}

	std::unordered_map<std::string, int>::iterator global_it = m_global_variables.find(resolved_name);
	if (global_it == m_global_variables.end())
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Resolved symbol '{}' not found in globals.", resolved_name), line, m_file_name, m_source_lines));
		return std::nullopt;
	}

	return global_it->second;
}

bool CodeGenerator::EmitResolvedNameGetGlobal(const std::string& resolved_name, int line)
{
	std::optional<int> global_index = ResolveResolvedNameGlobalIndex(resolved_name, line);
	if (!global_index.has_value())
	{
		return false;
	}

	EmitVariable(global_index.value(), OpCode::GET_GLOBAL, line);
	return true;
}

std::shared_ptr<MidoriType> CodeGenerator::GetConcreteTypeForExpression(const std::unique_ptr<MidoriExpression>& expr)
{
	if (!m_param_type_map.empty() && expr->IsExpression<MidoriExpression::NameAccess>())
	{
		const MidoriExpression::NameAccess& bounded_name = expr->GetExpression<MidoriExpression::NameAccess>();
		TypeEnvironment::iterator it = m_param_type_map.find(bounded_name.m_name.m_lexeme);
		if (it != m_param_type_map.end())
		{
			return it->second;
		}
	}

	std::shared_ptr<MidoriType> type = expr->GetType();
	if (!m_generic_type_substitution.empty())
	{
		return SubstituteGenericTypes(type, m_generic_type_substitution);
	}
	return type;
}

std::shared_ptr<MidoriType> CodeGenerator::SubstituteGenericTypes(const std::shared_ptr<MidoriType>& type, const TypeEnvironment& generic_type_map)
{
	using namespace std::string_literals;

	using SubstituteFn = std::function<std::shared_ptr<MidoriType>(const std::shared_ptr<MidoriType>&)>;

	std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>> cache;
	std::unordered_set<const MidoriType*> visiting;

	SubstituteFn substitute;

	struct SubstituteVisitor
	{
		const TypeEnvironment& m_generic_type_map;
		std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>& m_cache;
		SubstituteFn& m_substitute;
		const std::shared_ptr<MidoriType>& m_current;

		std::shared_ptr<MidoriType> operator()(const MidoriType::GenericParam& type_variant) const
		{
			TypeEnvironment::const_iterator it = m_generic_type_map.find(type_variant.m_name);
			if (it != m_generic_type_map.end())
			{
				return it->second;
			}
			return m_current;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::TypeVariable&) const
		{
			TypeEnvironment::const_iterator it = m_generic_type_map.find(m_current->ToString());
			if (it != m_generic_type_map.end())
			{
				return it->second;
			}
			return m_current;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::ArrayType& type_variant) const
		{
			std::shared_ptr<MidoriType> substituted_element = m_substitute(type_variant.m_element_type);
			if (substituted_element != type_variant.m_element_type)
			{
				return std::make_shared<MidoriType>(MidoriType::ArrayType{ substituted_element });
			}
			return m_current;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::TupleType& type_variant) const
		{
			std::vector<std::shared_ptr<MidoriType>> substituted_elements;
			bool changed = false;
			for (const std::shared_ptr<MidoriType>& elem_type : type_variant.m_element_types)
			{
				std::shared_ptr<MidoriType> substituted = m_substitute(elem_type);
				substituted_elements.push_back(substituted);
				if (substituted != elem_type)
				{
					changed = true;
				}
			}
			if (changed)
			{
				return std::make_shared<MidoriType>(MidoriType::TupleType{ std::move(substituted_elements) });
			}
			return m_current;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::FunctionType& type_variant) const
		{
			std::vector<std::shared_ptr<MidoriType>> substituted_params;
			bool changed = false;
			for (const std::shared_ptr<MidoriType>& param_type : type_variant.m_param_types)
			{
				std::shared_ptr<MidoriType> substituted = m_substitute(param_type);
				substituted_params.push_back(substituted);
				if (substituted != param_type)
				{
					changed = true;
				}
			}
			std::shared_ptr<MidoriType> substituted_return = m_substitute(type_variant.m_return_type);
			if (substituted_return != type_variant.m_return_type)
			{
				changed = true;
			}
			if (changed)
			{
				return std::make_shared<MidoriType>
				(
					MidoriType::FunctionType
					{
						.m_param_types = std::move(substituted_params),
						.m_return_type = substituted_return,
						.m_constraints = type_variant.m_constraints,
						.m_is_foreign = type_variant.m_is_foreign
					}
				);
			}
			return m_current;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::StructType& type_variant) const
		{
			std::vector<std::shared_ptr<MidoriType>> empty_member_types;
			std::vector<std::string> member_names_copy = type_variant.m_member_names;
			std::shared_ptr<MidoriType> new_struct = MidoriType::MakeStructType(type_variant.m_name, std::move(empty_member_types), std::move(member_names_copy), {});
			m_cache[m_current.get()] = new_struct;

			std::vector<std::shared_ptr<MidoriType>> substituted_members;
			std::ranges::transform(type_variant.m_member_types, std::back_inserter(substituted_members), m_substitute);
			new_struct->GetType<MidoriType::StructType>().m_member_types = std::move(substituted_members);
			if (!type_variant.m_generic_params.empty() || type_variant.m_is_generic_instantiation)
			{
				new_struct->GetType<MidoriType::StructType>().m_is_generic_instantiation = true;
			}
			return new_struct;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::UnionType& type_variant) const
		{
			std::shared_ptr<MidoriType> new_union = MidoriType::MakeUnionType(type_variant.m_name, {});
			m_cache[m_current.get()] = new_union;
			MidoriType::UnionType& new_union_ref = new_union->GetType<MidoriType::UnionType>();
			if (!type_variant.m_generic_params.empty() || type_variant.m_is_generic_instantiation)
			{
				new_union_ref.m_is_generic_instantiation = true;
			}

			for (const auto& [member_name, member_ctx] : type_variant.m_member_info)
			{
				std::vector<std::shared_ptr<MidoriType>> substituted_members;
				std::ranges::transform(member_ctx.m_member_types, std::back_inserter(substituted_members), m_substitute);
				new_union_ref.m_member_info.emplace(member_name, MidoriType::UnionType::UnionMemberContext{ std::move(substituted_members), member_ctx.m_tag });
			}
			return new_union;
		}

		std::shared_ptr<MidoriType> operator()(const MidoriType::UndecidedType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::FloatType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::IntegerType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::ByteType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::WordType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::TextType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::BoolType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::UnitType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::NeverType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::RangeType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::FutureType&) const { return m_current; }
		std::shared_ptr<MidoriType> operator()(const MidoriType::ClassConstraint&) const { return m_current; }
	};

	substitute = [&generic_type_map, &cache, &visiting, &substitute](const std::shared_ptr<MidoriType>& current) -> std::shared_ptr<MidoriType>
	{
		if (!current)
		{
			return current;
		}

		std::unordered_map<const MidoriType*, std::shared_ptr<MidoriType>>::iterator cache_it = cache.find(current.get());
		if (cache_it != cache.end())
		{
			return cache_it->second;
		}

		if (visiting.contains(current.get()))
		{
			return current;
		}

		visiting.insert(current.get());

		SubstituteVisitor visitor{ generic_type_map, cache, substitute, current };
		std::shared_ptr<MidoriType> result = std::visit(visitor, current->m_type);

		visiting.erase(current.get());
		return result;
	};

	return substitute(type);
}

void CodeGenerator::EmitFunction(const std::vector<Token>& params, std::unique_ptr<MidoriExpression>& body, const std::string& debug_name, int line, int captured_count)
{
	int arity = static_cast<int>(params.size());
	if (arity > MAX_FUNCTION_ARITY)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many arguments (max {})", MAX_FUNCTION_ARITY + 1), line, m_file_name, m_source_lines));
		return;
	}
	if (captured_count > MAX_CAPTURED_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many captured variables (max {})", MAX_CAPTURED_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	size_t prev_index = m_builder.m_current_procedure_index;
	const bool closure_uses_shared_cells = CurrentProcedureUsesSharedCells() || (body && ExpressionContainsAsyncOrAwait(*body));
	const size_t closure_proc_index = m_builder.m_procedures.size();
	m_builder.m_current_procedure_index = closure_proc_index;
	m_builder.m_procedures.emplace_back();
	EnsureProcedureMetadataSize(closure_proc_index);
	m_shared_cell_procedure_flags[closure_proc_index] = closure_uses_shared_cells;
	m_procedure_capture_counts[closure_proc_index] = captured_count;
	Visit(body);

	EmitByte(OpCode::RETURN, line);

	std::string full_name = debug_name + "@"s + (m_module_name.has_value() ? m_module_name.value() : m_file_name);
	m_builder.m_procedure_names.emplace_back(full_name.c_str());

	m_builder.m_current_procedure_index = prev_index;

	if (m_builder.m_current_procedure_index > MAX_FUNCTION_COUNT)
	{
		AddError(MidoriError::GenerateCodeGeneratorErrorWithContext(std::format("Too many functions (max {})", MAX_FUNCTION_COUNT + 1), line, m_file_name, m_source_lines));
		return;
	}

	if (captured_count == 0)
	{
		EmitByte(OpCode::MAKE_FUNCTION, line);
		EmitByte(static_cast<OpCode>(closure_proc_index), line);
	}
	else
	{
		EmitByte(OpCode::MAKE_CLOSURE, line);
		EmitByte(static_cast<OpCode>(closure_proc_index), line);

		NoteCaptureBinding(captured_count, closure_uses_shared_cells);
		EmitByte(closure_uses_shared_cells ? OpCode::BIND_CAPTURES_SHARED : OpCode::BIND_CAPTURES, line);
		EmitByte(static_cast<OpCode>(captured_count), line);
	}
}

std::size_t CodeGenerator::FunctionSignatureHash::operator()(const FunctionSignature& sig) const
{
	std::size_t hash = std::hash<std::string>{}(sig.m_base_name);
	std::ranges::for_each
	(
		sig.m_concrete_types,
		[&hash](const std::string& type)
		{
			hash ^= std::hash<std::string>{}(type)+HASH_OFFSET_BASIS + (hash << HASH_LEFT_SHIFT) + (hash >> HASH_RIGHT_SHIFT);
		}
	);
	return hash;
}
bool CodeGenerator::FunctionSignature::operator==(const FunctionSignature& other) const
{
	return m_base_name == other.m_base_name && m_concrete_types == other.m_concrete_types;
}
std::size_t CodeGenerator::TypePairHash::operator()(const std::pair<MidoriType*, MidoriType*>& pair) const
{
	std::size_t h1 = std::hash<MidoriType*>{}(pair.first);
	std::size_t h2 = std::hash<MidoriType*>{}(pair.second);
	return h1 ^ (h2 << 1);
}

