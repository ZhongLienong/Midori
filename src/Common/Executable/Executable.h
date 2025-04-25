#pragma once

#include "Common/Value/Value.h"

#include <cinttypes>
#include <string>

enum class OpCode : uint8_t
{
	// Constants and Literals
	LOAD_CONSTANT,
	LOAD_CONSTANT_LONG,
	LOAD_CONSTANT_LONG_LONG,
	LOAD_STRING,
	INTEGER_CONSTANT,
	FLOAT_CONSTANT,
	OP_UNIT,
	OP_TRUE,
	OP_FALSE,

	// Array Operations
	CREATE_ARRAY,
	GET_ARRAY,
	SET_ARRAY,
	DUP_ARRAY,
	ADD_BACK_ARRAY,
	ADD_FRONT_ARRAY,

	// Atomic type casting
	INT_TO_FLOAT,
	TEXT_TO_FLOAT,
	FLOAT_TO_INT,
	TEXT_TO_INT,
	FLOAT_TO_TEXT,
	INT_TO_TEXT,

	// Bit Operations
	LEFT_SHIFT,
	RIGHT_SHIFT,
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

	// Concatenations 
	CONCAT_ARRAY,
	CONCAT_TEXT,

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

	// Switch
	LOAD_TAG,
	SET_TAG,

	// Callable
	CALL_FOREIGN,
	CALL_DEFINED,
	CONSTRUCT_STRUCT,
	CONSTRUCT_UNION,

	// BoundedName Operations
	ALLOCATE_CLOSURE,
	CONSTRUCT_CLOSURE,
	DEFINE_GLOBAL,
	GET_GLOBAL,
	SET_GLOBAL,
	GET_LOCAL,
	SET_LOCAL,
	GET_CELL,
	SET_CELL,

	// Struct Operations
	GET_MEMBER,
	SET_MEMBER,

	// Stack Operations
	POP,
	DUP,
	POP_SCOPE,
	POP_MULTIPLE,

	// Return
	RETURN,
	HALT,

	// Box Operations
	BOX_INT,
	BOX_FLOAT,
	BOX_BOOL,
	BOX_UNIT,
	UNBOX,
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
	using StaticData = std::vector<MidoriValue>;
	using GlobalNames = std::vector<MidoriText>;
	using Procedures = std::vector<BytecodeStream>;
	using StringPool = std::vector<std::string>;

#ifdef DEBUG
	std::vector<MidoriText> m_procedure_names;
#endif

private:
	MidoriTraceable::GarbageCollectionRoots m_constant_roots;
	StaticData m_constants;
	GlobalNames m_globals;
	Procedures m_procedures;
	StringPool m_string_pool;

public:
	const MidoriValue& GetConstant(int index) const;

	int AddConstant(MidoriValue&& value);

	int AddGlobalVariable(MidoriText&& name);

	const MidoriText& GetGlobalVariable(int index) const;

	void AddConstantRoot(MidoriTraceable* root);

	void AttachProcedures(Procedures&& bytecode);

	void AddStringPool(StringPool&& string_pool);

#ifdef DEBUG
	void AttachProcedureNames(std::vector<MidoriText>&& procedure_names);
#endif

	int GetLine(int instr_index, int proc_index) const;

	const BytecodeStream& GetBytecodeStream(int proc_index) const;

	const MidoriTraceable::GarbageCollectionRoots& GetConstantRoots();

	OpCode ReadByteCode(int instr_index, int proc_index) const;

	int GetByteCodeSize(int proc_index) const;

	int GetProcedureCount() const;

	int GetGlobalVariableCount() const;

	const StringPool& GetStringPool() const;
};