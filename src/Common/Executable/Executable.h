#pragma once

#include "Common/Value/Value.h"

#include <cinttypes>
#include <string>

enum class OpCode : uint8_t
{
	// Constants and Literals
	LOAD_STRING,
	LOAD_STRING_WIDE,
	INTEGER_CONSTANT,
	FLOAT_CONSTANT,
	BYTE_CONSTANT,
	WORD_CONSTANT,
	OP_UNIT,
	OP_TRUE,
	OP_FALSE,

	// Small integer constants (optimization for common values)
	INT_MINUS_1,
	INT_0,
	INT_1,
	INT_2,
	INT_3,
	INT_4,
	INT_5,
	INT_10,

	// Array Operations
	CREATE_ARRAY,
	GET_ARRAY,
	SET_ARRAY,
	DUP_ARRAY,
	ADD_BACK_ARRAY,
	ADD_FRONT_ARRAY,
	GET_ARRAY_LENGTH,

	// Range Operations
	CREATE_INT_RANGE,
	CREATE_FLOAT_RANGE,
	GET_RANGE_START,
	GET_RANGE_END,
	GET_RANGE_STEP,

	// Atomic type casting
	INT_TO_FLOAT,
	TEXT_TO_FLOAT,
	FLOAT_TO_INT,
	TEXT_TO_INT,
	FLOAT_TO_TEXT,
	INT_TO_TEXT,
	BYTE_TO_INT,
	INT_TO_BYTE,
	BYTE_TO_WORD,
	WORD_TO_BYTE,
	WORD_TO_INT,
	INT_TO_WORD,
	BYTE_TO_FLOAT,
	FLOAT_TO_BYTE,
	WORD_TO_FLOAT,
	FLOAT_TO_WORD,

	// Bit Operations
	LEFT_SHIFT,
	RIGHT_SHIFT,
	LEFT_SHIFT_BYTE,
	RIGHT_SHIFT_BYTE,
	LEFT_SHIFT_WORD,
	RIGHT_SHIFT_WORD,
	BITWISE_AND,
	BITWISE_OR,
	BITWISE_XOR,
	BITWISE_NOT,

	// Arithmetic Operations
	ADD_FLOAT,
	SUBTRACT_FLOAT,
	MULTIPLY_FLOAT,
	DIVIDE_FLOAT,
	MODULO_FLOAT,
	ADD_INTEGER,
	SUBTRACT_INTEGER,
	MULTIPLY_INTEGER,
	DIVIDE_INTEGER,
	MODULO_INTEGER,
	ADD_BYTE,
	SUBTRACT_BYTE,
	MULTIPLY_BYTE,
	DIVIDE_BYTE,
	MODULO_BYTE,
	ADD_WORD,
	SUBTRACT_WORD,
	MULTIPLY_WORD,
	DIVIDE_WORD,
	MODULO_WORD,

	// Concatenations
	CONCAT_ARRAY,
	CONCAT_TEXT,

	// Array Mutations
	APPEND_ARRAY,
	PREPEND_ARRAY,
	EXTEND_ARRAY,

	// Text Mutations
	APPEND_TEXT,
	PREPEND_TEXT,

	// Compound Assignment Operations
	ADD_ASSIGN_INT,
	ADD_ASSIGN_FLOAT,
	SUB_ASSIGN_INT,
	SUB_ASSIGN_FLOAT,
	MUL_ASSIGN_INT,
	MUL_ASSIGN_FLOAT,
	DIV_ASSIGN_INT,
	DIV_ASSIGN_FLOAT,
	MOD_ASSIGN_INT,
	MOD_ASSIGN_FLOAT,
	AND_ASSIGN_INT,
	OR_ASSIGN_INT,
	XOR_ASSIGN_INT,
	LEFT_SHIFT_ASSIGN,
	RIGHT_SHIFT_ASSIGN,

	// Comparison Operations
	EQUAL_FLOAT,
	NOT_EQUAL_FLOAT,
	GREATER_FLOAT,
	GREATER_EQUAL_FLOAT,
	LESS_FLOAT,
	LESS_EQUAL_FLOAT,
	EQUAL_INTEGER,
	NOT_EQUAL_INTEGER,
	GREATER_INTEGER,
	GREATER_EQUAL_INTEGER,
	LESS_INTEGER,
	LESS_EQUAL_INTEGER,
	EQUAL_BYTE,
	NOT_EQUAL_BYTE,
	GREATER_BYTE,
	GREATER_EQUAL_BYTE,
	LESS_BYTE,
	LESS_EQUAL_BYTE,
	EQUAL_WORD,
	NOT_EQUAL_WORD,
	GREATER_WORD,
	GREATER_EQUAL_WORD,
	LESS_WORD,
	LESS_EQUAL_WORD,
	EQUAL_TEXT,

	// Logical Operations
	NOT,

	// UnaryPrefix Operations
	NEGATE_FLOAT,
	NEGATE_INTEGER,

	// Control Flow
	JUMP_IF_FALSE,
	JUMP_IF_TRUE,
	JUMP,
	JUMP_BACK,
	IF_INTEGER_LESS,
	IF_INTEGER_LESS_EQUAL,
	IF_INTEGER_GREATER,
	IF_INTEGER_GREATER_EQUAL,
	IF_INTEGER_EQUAL,
	IF_INTEGER_NOT_EQUAL,
	IF_FLOAT_LESS,
	IF_FLOAT_LESS_EQUAL,
	IF_FLOAT_GREATER,
	IF_FLOAT_GREATER_EQUAL,
	IF_FLOAT_EQUAL,
	IF_FLOAT_NOT_EQUAL,
	BREAK,

	// Match
	LOAD_TAG,
	GET_TAG,
	SET_TAG,
	MATCH_JUMP_TABLE,

	// Callable
	CALL_FOREIGN,
	CALL_FOREIGN_INDEXED,
	CALL,
	CALL_0,
	CALL_1,
	CALL_2,
	CALL_3,
	CALL_PROC,
	CALL_PROC_0,
	CALL_PROC_1,
	CALL_PROC_2,
	CALL_PROC_3,
	CALL_GLOBAL,
	CALL_GLOBAL_WIDE,
	CALL_GLOBAL_SHARED,
	CALL_GLOBAL_SHARED_WIDE,
	TAIL_CALL,
	CONSTRUCT_STRUCT,
	CONSTRUCT_UNION,

	// Closure Operations
	MAKE_CLOSURE,
	BIND_CAPTURES,
	MAKE_FUNCTION,
	DEFINE_GLOBAL,
	GET_GLOBAL,
	SET_GLOBAL,
	GET_LOCAL,
	SET_LOCAL,
	GET_LOCAL_0,
	GET_LOCAL_1,
	GET_LOCAL_2,
	GET_LOCAL_3,
	SET_LOCAL_0,
	SET_LOCAL_1,
	SET_LOCAL_2,
	SET_LOCAL_3,
	GET_LOCAL_SHARED,
	SET_LOCAL_SHARED,
	GET_CELL,
	SET_CELL,
	BIND_CAPTURES_SHARED,
	DEFINE_GLOBAL_SHARED,
	GET_GLOBAL_SHARED,
	SET_GLOBAL_SHARED,
	GET_SHARED_CELL,
	SET_SHARED_CELL,

	// Wide variable operations (for indices > 255)
	DEFINE_GLOBAL_WIDE,
	GET_GLOBAL_WIDE,
	SET_GLOBAL_WIDE,
	GET_LOCAL_WIDE,
	SET_LOCAL_WIDE,
	GET_LOCAL_SHARED_WIDE,
	SET_LOCAL_SHARED_WIDE,
	GET_CELL_WIDE,
	SET_CELL_WIDE,
	DEFINE_GLOBAL_SHARED_WIDE,
	GET_GLOBAL_SHARED_WIDE,
	SET_GLOBAL_SHARED_WIDE,
	GET_SHARED_CELL_WIDE,
	SET_SHARED_CELL_WIDE,

	// Struct Operations
	GET_MEMBER,
	SET_MEMBER,

	// Stack Operations
	POP,
	DUP,
	SWAP,
	POP_LOCAL_SCOPE,
	POP_VALUES,
	POP_BLOCK_SCOPE,
	POP_MATCH_SCOPE,

	// Return
	RETURN,
	HALT,

	// Async
	SPAWN_ASYNC,
	AWAIT_FUTURE,
	ASYNC_RETURN,

	// Placeholder
	PUSH_PLACEHOLDER,
	UPDATE_PLACEHOLDER,
};

enum class ExecutionMode : uint8_t
{
	SyncOnly,
	AsyncEnabled
};

class BytecodeStream
{
public:
	using iterator = std::vector<OpCode>::iterator;
	using const_iterator = std::vector<OpCode>::const_iterator;
	using reverse_iterator = std::vector<OpCode>::reverse_iterator;
	using const_reverse_iterator = std::vector<OpCode>::const_reverse_iterator;

	iterator begin();
	iterator end();
	const_iterator cbegin() const;
	const_iterator cend() const;
	reverse_iterator rbegin();
	reverse_iterator rend();
	const_reverse_iterator crbegin() const;
	const_reverse_iterator crend() const;

private:
	std::vector<OpCode> m_bytecode;
	std::vector<std::pair<int, int>> m_line_info; // Pair of line number and count of consecutive instructions

public:

	OpCode ReadByteCode(int index) const;

	void SetByteCode(int index, OpCode byte);

	void AddByteCode(OpCode byte, int line);

	void PopByteCode(int line);

	int GetByteCodeSize() const;

	bool IsByteCodeEmpty() const;

	int GetLine(int index) const;

	void Append(BytecodeStream&& other);

	const OpCode* operator[](int index) const;
};

class MidoriExecutable
{
public:
	using GlobalNames = std::vector<MidoriText>;
	using Procedures = std::vector<BytecodeStream>;
	using StringPool = std::vector<std::string>;
	std::vector<MidoriText> m_procedure_names;
	std::string m_file_name;

private:
	GlobalNames m_globals;
	Procedures m_procedures;
	StringPool m_string_pool;
	bool m_has_async = false;
	ExecutionMode m_execution_mode = ExecutionMode::SyncOnly;

public:

	int AddGlobalVariable(MidoriText&& name);

	const MidoriText& GetGlobalVariable(int index) const;

	void AttachProcedures(Procedures&& bytecode);

	void AddStringPool(StringPool&& string_pool);

	void AttachProcedureNames(std::vector<MidoriText>&& procedure_names);

	void SetFileName(std::string&& file_name);

	std::string_view GetFileName() const;

	void SetHasAsync(bool has_async);

	bool HasAsync() const;

	void SetExecutionMode(ExecutionMode execution_mode);

	ExecutionMode GetExecutionMode() const;

	int GetLine(int instr_index, int proc_index) const;

	const BytecodeStream& GetBytecodeStream(int proc_index) const;

	OpCode ReadByteCode(int instr_index, int proc_index) const;

	int GetByteCodeSize(int proc_index) const;

	int GetProcedureCount() const;

	int GetGlobalVariableCount() const;

	const StringPool& GetStringPool() const;
};
