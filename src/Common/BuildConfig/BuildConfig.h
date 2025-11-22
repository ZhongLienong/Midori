#pragma once

// Build configuration levels for Midori compiler
//
// MIDORI_BUILD_DEBUG (3):
//   - Full debugging output
//   - AST dumps
//   - Bytecode disassembly
//   - Runtime stack traces
//   - Detailed compilation info
//   - Optimizer statistics
//
// MIDORI_BUILD_DEVELOPMENT (2):
//   - Compilation progress and details
//   - Optimizer statistics
//   - Linker information
//   - NO AST dumps
//   - NO bytecode disassembly
//   - NO runtime stack traces
//
// MIDORI_BUILD_RELEASE (1):
//   - Standard compiler messages only
//   - Error and warning output
//   - Minimal runtime info

// Define build level based on CMake configuration
#if defined(MIDORI_BUILD_DEBUG)
    #define MIDORI_DEBUG_LEVEL 3
#elif defined(MIDORI_BUILD_DEVELOPMENT)
    #define MIDORI_DEBUG_LEVEL 2
#else
    #define MIDORI_DEBUG_LEVEL 1
    #define MIDORI_BUILD_RELEASE
#endif

// Convenience macros for conditional compilation
#define MIDORI_DEBUG_FULL (MIDORI_DEBUG_LEVEL >= 3)
#define MIDORI_DEBUG_INFO (MIDORI_DEBUG_LEVEL >= 2)
#define MIDORI_DEBUG_MINIMAL (MIDORI_DEBUG_LEVEL >= 1)

// Feature flags based on build level
#if MIDORI_DEBUG_FULL
    #define MIDORI_ENABLE_AST_DUMP 1
    #define MIDORI_ENABLE_DISASSEMBLY 1
    #define MIDORI_ENABLE_STACK_TRACE 1
    #define MIDORI_ENABLE_COMPILATION_INFO 1
    #define MIDORI_ENABLE_OPTIMIZER_STATS 1
    #define MIDORI_ENABLE_LINKER_INFO 1
#elif MIDORI_DEBUG_INFO
    #define MIDORI_ENABLE_AST_DUMP 0
    #define MIDORI_ENABLE_DISASSEMBLY 0
    #define MIDORI_ENABLE_STACK_TRACE 0
    #define MIDORI_ENABLE_COMPILATION_INFO 1
    #define MIDORI_ENABLE_OPTIMIZER_STATS 1
    #define MIDORI_ENABLE_LINKER_INFO 1
#else
    #define MIDORI_ENABLE_AST_DUMP 0
    #define MIDORI_ENABLE_DISASSEMBLY 0
    #define MIDORI_ENABLE_STACK_TRACE 0
    #define MIDORI_ENABLE_COMPILATION_INFO 0
    #define MIDORI_ENABLE_OPTIMIZER_STATS 0
    #define MIDORI_ENABLE_LINKER_INFO 0
#endif
