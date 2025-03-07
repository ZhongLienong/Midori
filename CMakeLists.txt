cmake_minimum_required(VERSION 3.8)

# Project name
project("Midori")

# Enforce C++23
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Test Endianness
include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)
if (IS_BIG_ENDIAN)
    add_definitions(-DMIDORI_BIG_ENDIAN)
else()
    add_definitions(-DMIDORI_LITTLE_ENDIAN)
endif()

# MSVC-specific compiler flags
if (MSVC)
    # Enable fastest optimizations
    add_compile_options(
        /GL          # Whole program optimization
        /Gy          # Enable function-level linking
        /GR-         # Disable RTTI
        /permissive- # Standards-conformant mode
        /WX          # Treat warnings as errors
        /Zi          # Debug symbols
    )

    # Linker flags
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /LTCG /DEBUG /OPT:REF /OPT:ICF")
    set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_SHARED_LINKER_FLAGS_RELEASE} /LTCG /DEBUG /OPT:REF /OPT:ICF")
endif()

# Debug flags
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    add_definitions(-DDEBUG)
    # Disable optimizations and enable debug information for debug builds
    add_compile_options(/Od)
else()
    add_compile_options(/O2 /MT)
endif()

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/src)

# Source files for the main executable
file(GLOB_RECURSE SRC_FILES
    "src/Common/*.h"
    "src/Compiler/*.h"
    "src/Interpreter/*.h"
    "src/Utility/*.h"
    "src/Common/*.cpp"
    "src/Compiler/*.cpp"
    "src/Interpreter/*.cpp"
    "src/Utility/*.cpp"
    "src/Midori.cpp"
)

# Add source to this project's executable.
add_executable(Midori ${SRC_FILES})

# Source files for the standard library DLL
file(GLOB_RECURSE STD_LIB_SRC_FILES
    "src/Library/MidoriPrelude.cpp"
)

# Add standard library as a dynamically linked library
add_library(MidoriStdLib SHARED ${STD_LIB_SRC_FILES})

# Set the output name for Unix-like systems to follow naming convention
if (UNIX)
    set_target_properties(MidoriStdLib PROPERTIES OUTPUT_NAME "libMidoriStdLib")
endif()

# Specify where to put the build artifacts (this makes sure they go into 'out')
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/out)

# Link against any necessary libraries
# target_link_libraries(MidoriStdLib <OtherLibraries>)