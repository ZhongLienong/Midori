cmake_minimum_required(VERSION 3.24)

# ============================================================================
# Project Configuration
# ============================================================================
project(Midori
    VERSION 1.0.0
    DESCRIPTION "Midori Programming Language"
    LANGUAGES CXX
)

# ============================================================================
# Global Settings
# ============================================================================
# Enforce C++23 standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Define custom build configurations
set(CMAKE_CONFIGURATION_TYPES "Debug;Development;Release" CACHE STRING "" FORCE)

# Output directories for build artifacts
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out)

# ============================================================================
# Platform Detection
# ============================================================================
# Test system endianness
include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)

# ============================================================================
# Source File Collection
# ============================================================================
# Collect common source files
file(GLOB_RECURSE COMMON_HEADERS
    "src/Common/*.h"
    "src/Compiler/*.h"
    "src/Interpreter/*.h"
    "src/Utility/*.h"
)

file(GLOB_RECURSE COMMON_SOURCES
    "src/Common/*.cpp"
    "src/Compiler/*.cpp"
    "src/Interpreter/*.cpp"
    "src/Utility/*.cpp"
)

file(GLOB_RECURSE LIBRARY_SOURCES
    "src/Library/*.cpp"
)

# ============================================================================
# WebAssembly Build Configuration
# ============================================================================
if(EMSCRIPTEN)
    message(STATUS "Configuring for WebAssembly build with Emscripten")

    # Collect WASM-specific files
    file(GLOB_RECURSE WASM_FILES
        "src/WASM/*.h"
        "src/WASM/*.cpp"
    )

    # Create WASM executable (no main() needed)
    add_executable(Midori
        ${COMMON_HEADERS}
        ${COMMON_SOURCES}
        ${LIBRARY_SOURCES}
        ${WASM_FILES}
    )

    # Target-specific compile options
    target_compile_options(Midori PRIVATE
        -fexceptions
        -O3
    )

    # Target-specific compile definitions
    target_compile_definitions(Midori PRIVATE
        $<$<BOOL:${IS_BIG_ENDIAN}>:MIDORI_BIG_ENDIAN>
        $<IF:$<BOOL:${IS_BIG_ENDIAN}>,,MIDORI_LITTLE_ENDIAN>
    )

    # Target include directories
    target_include_directories(Midori PRIVATE
        ${CMAKE_SOURCE_DIR}/src
    )

    # Emscripten linker options
    target_link_options(Midori PRIVATE
        --bind
        -sALLOW_MEMORY_GROWTH=1
        -sMODULARIZE=1
        -sEXPORT_NAME=createMidoriModule
        -sENVIRONMENT=web
        -sSTACK_SIZE=5MB
        -sINITIAL_MEMORY=32MB
        -sMAXIMUM_MEMORY=2GB
        -sALLOW_TABLE_GROWTH=1
        -sNO_DISABLE_EXCEPTION_CATCHING
        -sFORCE_FILESYSTEM=1
        -sEXPORTED_RUNTIME_METHODS=FS,FS_createPath,FS_createDataFile,wasmTable
        -sEXPORTED_FUNCTIONS=_malloc,_free
        --no-entry
    )

    # Set output properties
    set_target_properties(Midori PROPERTIES
        OUTPUT_NAME "midori"
        SUFFIX ".js"
    )

# ============================================================================
# Native Build Configuration (Windows/Linux/macOS)
# ============================================================================
else()
    message(STATUS "Configuring for native build")

    # Collect library headers for static linking
    file(GLOB_RECURSE LIBRARY_HEADERS
        "src/Library/*.h"
    )

    # Create main executable with statically linked FFI functions
    add_executable(Midori
        ${COMMON_HEADERS}
        ${COMMON_SOURCES}
        ${LIBRARY_HEADERS}
        ${LIBRARY_SOURCES}
        "src/Midori.cpp"
    )

    # Target include directories
    target_include_directories(Midori PRIVATE
        ${CMAKE_SOURCE_DIR}/src
    )

    # Target-specific compile definitions
    target_compile_definitions(Midori PRIVATE
        $<$<BOOL:${IS_BIG_ENDIAN}>:MIDORI_BIG_ENDIAN>
        $<IF:$<BOOL:${IS_BIG_ENDIAN}>,,MIDORI_LITTLE_ENDIAN>
        $<$<CONFIG:Debug>:MIDORI_BUILD_DEBUG>
        $<$<CONFIG:Development>:MIDORI_BUILD_DEVELOPMENT>
        $<$<CONFIG:Release>:NDEBUG>
    )

    # ========================================================================
    # MSVC-Specific Configuration
    # ========================================================================
    if(MSVC)
        # Remove default conflicting flags
        foreach(flag_var
            CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG
            CMAKE_CXX_FLAGS_RELEASE CMAKE_CXX_FLAGS_MINSIZEREL
            CMAKE_CXX_FLAGS_RELWITHDEBINFO)
            string(REPLACE "/GR" "" ${flag_var} "${${flag_var}}")
            string(REPLACE "/W3" "" ${flag_var} "${${flag_var}}")
            string(REPLACE "/EHsc" "" ${flag_var} "${${flag_var}}")
        endforeach()

        # Common MSVC flags for Midori executable
        target_compile_options(Midori PRIVATE
            /GR-            # Disable RTTI
            /permissive-    # Standards-conformant mode
            /W4             # Warning level 4
            /EHsc           # Enable async exception handling
            /bigobj         # Allow large object files

            # Debug configuration - best debuggability, no optimization
            $<$<CONFIG:Debug>:/Od>      # No optimization
            $<$<CONFIG:Debug>:/MDd>     # Debug runtime library
            $<$<CONFIG:Debug>:/Zi>      # Full debug info
            $<$<CONFIG:Debug>:/RTC1>    # Runtime checks

            # Development configuration - optimized with debug symbols
            $<$<CONFIG:Development>:/O2>    # Maximize speed
            $<$<CONFIG:Development>:/MD>    # Release runtime library
            $<$<CONFIG:Development>:/Zi>    # Full debug info

            # Release configuration - maximum speed
            $<$<CONFIG:Release>:/O2>    # Maximize speed
            $<$<CONFIG:Release>:/GL>    # Whole program optimization (LTCG)
            $<$<CONFIG:Release>:/Gw>    # Optimize global data
            $<$<CONFIG:Release>:/MT>    # Static runtime library
        )

        # MSVC linker flags
        target_link_options(Midori PRIVATE
            $<$<CONFIG:Development>:/DEBUG>
            $<$<CONFIG:Development>:/OPT:REF>
            $<$<CONFIG:Development>:/OPT:ICF>

            $<$<CONFIG:Release>:/LTCG>
            $<$<CONFIG:Release>:/OPT:REF>
            $<$<CONFIG:Release>:/OPT:ICF>
            $<$<CONFIG:Release>:/RELEASE>
        )
    endif()

endif()
