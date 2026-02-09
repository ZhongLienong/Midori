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

# Define build configurations (multi-config) or default build type (single-config)
if(CMAKE_GENERATOR_MULTI_CONFIG)
    set(CMAKE_CONFIGURATION_TYPES "Debug;Development;Release;Experimental" CACHE STRING "" FORCE)
else()
    if(NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
    endif()
endif()

# Performance tuning options (Experimental is an aggressive, non-portable speed profile)
option(MIDORI_ENABLE_LTO "Enable link-time optimization for Release builds" ON)

if(MSVC)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>" CACHE STRING "Use static MSVC runtime (/MT, /MTd in Debug)" FORCE)
endif()

# Output directories for build artifacts
set(MIDORI_OUTPUT_DIR "${CMAKE_BINARY_DIR}/out")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${MIDORI_OUTPUT_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${MIDORI_OUTPUT_DIR}")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${MIDORI_OUTPUT_DIR}")

# ============================================================================
# External Dependencies
# ============================================================================
include(FetchContent)

FetchContent_Declare(
    toml11
    GIT_REPOSITORY https://github.com/ToruNiina/toml11.git
    GIT_TAG v4.2.0
)
FetchContent_MakeAvailable(toml11)

# ============================================================================
# Platform Detection
# ============================================================================
# Test system endianness
include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)

set(MIDORI_ENDIAN_DEFINES
    $<$<BOOL:${IS_BIG_ENDIAN}>:MIDORI_BIG_ENDIAN>
    $<IF:$<BOOL:${IS_BIG_ENDIAN}>,,MIDORI_LITTLE_ENDIAN>
)

function(midori_configure_target target_name)
    target_include_directories(${target_name} PRIVATE
        ${CMAKE_SOURCE_DIR}/src
    )
    target_link_libraries(${target_name} PRIVATE toml11::toml11)
    target_compile_definitions(${target_name} PRIVATE ${MIDORI_ENDIAN_DEFINES})
endfunction()

# ============================================================================
# Source File Collection
# ============================================================================
# Collect common source files
file(GLOB_RECURSE COMMON_HEADERS CONFIGURE_DEPENDS
    "src/Common/*.h"
    "src/Compiler/*.h"
    "src/Interpreter/*.h"
    "src/Utility/*.h"
)

file(GLOB_RECURSE COMMON_SOURCES CONFIGURE_DEPENDS
    "src/Common/*.cpp"
    "src/Compiler/*.cpp"
    "src/Interpreter/*.cpp"
    "src/Utility/*.cpp"
)

file(GLOB_RECURSE LIBRARY_SOURCES CONFIGURE_DEPENDS
    "src/Library/*.cpp"
)

# ============================================================================
# WebAssembly Build Configuration
# ============================================================================
if(EMSCRIPTEN)
    message(STATUS "Configuring for WebAssembly build with Emscripten")

    option(MIDORI_WASM64 "Build WebAssembly with memory64 (wasm64)" ON)
    set(MIDORI_WASM64_FLAGS)
    if(MIDORI_WASM64)
        list(APPEND MIDORI_WASM64_FLAGS -sMEMORY64=1)
    endif()

    # Collect WASM-specific files
    file(GLOB_RECURSE WASM_FILES CONFIGURE_DEPENDS
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

    midori_configure_target(Midori)

    target_compile_definitions(Midori PRIVATE MIDORI_WASM=1)
    if(MIDORI_WASM64)
        target_compile_definitions(Midori PRIVATE MIDORI_WASM64=1)
    endif()

    # Target-specific compile options
    target_compile_options(Midori PRIVATE
        -fexceptions
        -O3
        -Wno-deprecated-literal-operator
        $<$<BOOL:${MIDORI_ENABLE_LTO}>:-flto>
        ${MIDORI_WASM64_FLAGS}
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
        $<$<BOOL:${MIDORI_ENABLE_LTO}>:-flto>
        ${MIDORI_WASM64_FLAGS}
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
    file(GLOB_RECURSE LIBRARY_HEADERS CONFIGURE_DEPENDS
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
    set(MIDORI_NATIVE_TARGETS Midori)

    # Detect AVX2 compiler flag for Experimental configuration
    set(MIDORI_HAS_AVX2_FLAG FALSE)
    if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
        set(MIDORI_AVX2_FLAG "-mavx2")
    elseif(MSVC)
        set(MIDORI_AVX2_FLAG "/arch:AVX2")
    else()
        set(MIDORI_AVX2_FLAG "-mavx2")
    endif()
    include(CheckCXXCompilerFlag)
    check_cxx_compiler_flag("${MIDORI_AVX2_FLAG}" MIDORI_HAS_AVX2_FLAG)

    set(MIDORI_HAS_AVX2_CONFIG FALSE)
    if(CMAKE_GENERATOR_MULTI_CONFIG)
        list(FIND CMAKE_CONFIGURATION_TYPES "Experimental" MIDORI_AVX2_CONFIG_INDEX)
        if(NOT MIDORI_AVX2_CONFIG_INDEX EQUAL -1)
            set(MIDORI_HAS_AVX2_CONFIG TRUE)
        endif()
    else()
        if(CMAKE_BUILD_TYPE STREQUAL "Experimental")
            set(MIDORI_HAS_AVX2_CONFIG TRUE)
        endif()
    endif()
    if(MIDORI_HAS_AVX2_CONFIG AND NOT MIDORI_HAS_AVX2_FLAG)
        message(WARNING "Experimental config requested but compiler does not support ${MIDORI_AVX2_FLAG}. Building without AVX2.")
    endif()

    foreach(target_name IN LISTS MIDORI_NATIVE_TARGETS)
        midori_configure_target(${target_name})
    endforeach()

    # Link platform-specific dynamic loading libraries
    if(WIN32)
    elseif(UNIX)
        foreach(target_name IN LISTS MIDORI_NATIVE_TARGETS)
            target_link_libraries(${target_name} PRIVATE ${CMAKE_DL_LIBS})
        endforeach()
    endif()

    # Target-specific compile definitions
    foreach(target_name IN LISTS MIDORI_NATIVE_TARGETS)
        target_compile_definitions(${target_name} PRIVATE
            $<$<CONFIG:Debug>:MIDORI_BUILD_DEBUG>
            $<$<CONFIG:Development>:MIDORI_BUILD_DEVELOPMENT>
            $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:NDEBUG>
        )
    endforeach()

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
            string(REPLACE "/RTC1" "" ${flag_var} "${${flag_var}}")
        endforeach()

        # Common MSVC flags for Midori executable
        foreach(target_name IN LISTS MIDORI_NATIVE_TARGETS)
            target_compile_options(${target_name} PRIVATE
            /GR-            # Disable RTTI
            /permissive-    # Standards-conformant mode
            /W4             # Warning level 4
            /EHsc           # Enable exception handling
            /bigobj         # Allow large object files
            /wd5311         # Suppress deprecated string-literal operator warning (from toml11)

            # Debug configuration - best debuggability, no optimization
            $<$<CONFIG:Debug>:/Od>      # No optimization
            $<$<CONFIG:Debug>:/Zi>      # Full debug info

            # Development configuration - optimized with debug symbols
            $<$<CONFIG:Development>:/O2>    # Maximize speed
            $<$<CONFIG:Development>:/Zi>    # Full debug info

            # Release/Experimental configuration - maximum speed
            $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:/O2>    # Maximize speed
            $<$<AND:$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>,$<BOOL:${MIDORI_ENABLE_LTO}>>:/GL>    # Whole program optimization (LTCG)
            $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:/Gw>    # Optimize global data
            $<$<AND:$<CONFIG:Experimental>,$<BOOL:${MIDORI_HAS_AVX2_FLAG}>>:${MIDORI_AVX2_FLAG}>
            $<$<CONFIG:Experimental>:/Gy>   # Function-level linking
            $<$<CONFIG:Experimental>:/GF>   # String pooling
            $<$<CONFIG:Experimental>:/fp:fast>   # Fast-math
            )
        endforeach()

        # MSVC linker flags
        foreach(target_name IN LISTS MIDORI_NATIVE_TARGETS)
            target_link_options(${target_name} PRIVATE
                $<$<CONFIG:Development>:/DEBUG>
                $<$<CONFIG:Development>:/OPT:REF>
                $<$<CONFIG:Development>:/OPT:ICF>

                $<$<AND:$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>,$<BOOL:${MIDORI_ENABLE_LTO}>>:/LTCG>
                $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:/OPT:REF>
                $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:/OPT:ICF>
                $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:/RELEASE>
            )
        endforeach()
    endif()

    if(NOT MSVC)
        foreach(target_name IN LISTS MIDORI_NATIVE_TARGETS)
            target_compile_options(${target_name} PRIVATE
                $<$<CONFIG:Development>:-O2>
                $<$<CONFIG:Development>:-g>
                $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:-O3>
                $<$<AND:$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>,$<BOOL:${MIDORI_ENABLE_LTO}>>:-flto>
                $<$<AND:$<CONFIG:Experimental>,$<BOOL:${MIDORI_HAS_AVX2_FLAG}>>:${MIDORI_AVX2_FLAG}>
                $<$<CONFIG:Experimental>:-fomit-frame-pointer>
                $<$<CONFIG:Experimental>:-funroll-loops>
                $<$<CONFIG:Experimental>:-ffast-math>
            )
            target_link_options(${target_name} PRIVATE
                $<$<AND:$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>,$<BOOL:${MIDORI_ENABLE_LTO}>>:-flto>
            )
        endforeach()
    endif()

endif()
