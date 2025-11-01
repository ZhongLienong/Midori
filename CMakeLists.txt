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

# Define custom Development configuration
set(CMAKE_CONFIGURATION_TYPES "Debug;Development;Release" CACHE STRING "" FORCE)

# MSVC-specific compiler flags
if (MSVC)
    # Remove default flags that cause conflicts
    string(REPLACE "/GR" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    string(REPLACE "/W3" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    string(REPLACE "/EHsc" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

    # Common flags for all configurations
    add_compile_options(
        /GR-         # Disable RTTI
        /permissive- # Standards-conformant mode
        /W4          # Warning level 4
        /EHa         # Enable async exception handling (allows __try/__except with C++)
    )

    # Debug configuration - best debuggability, no optimization
    add_compile_options(
        $<$<CONFIG:Debug>:/Od>      # No optimization
        $<$<CONFIG:Debug>:/MDd>     # Debug runtime library
        $<$<CONFIG:Debug>:/Zi>      # Full debug info
        $<$<CONFIG:Debug>:/RTC1>    # Runtime checks (stack frames, uninitialized variables)
        $<$<CONFIG:Debug>:/Ob0>     # Disable inlining
        $<$<CONFIG:Debug>:/DDEBUG>  # DEBUG macro
    )

    # Development configuration - optimized with debug symbols (like Unreal's Development)
    add_compile_options(
        $<$<CONFIG:Development>:/O2>    # Maximize speed
        $<$<CONFIG:Development>:/MD>    # Release runtime library
        $<$<CONFIG:Development>:/Zi>    # Full debug info
        $<$<CONFIG:Development>:/Oi>    # Intrinsic functions
        $<$<CONFIG:Development>:/Gy>    # Function-level linking
        $<$<CONFIG:Development>:/DDEVELOPMENT>  # DEVELOPMENT macro
    )

    # Release configuration - maximum speed for modern x64 CPUs (12th gen Intel, Zen 4+)
    add_compile_options(
        $<$<CONFIG:Release>:/O2>        # Maximize speed
        $<$<CONFIG:Release>:/Ob3>       # Aggressive inlining (beyond /Ob2)
        $<$<CONFIG:Release>:/Ot>        # Favor fast code over small code
        $<$<CONFIG:Release>:/Oi>        # Intrinsic functions
        $<$<CONFIG:Release>:/GL>        # Whole program optimization (LTCG)
        $<$<CONFIG:Release>:/Gy>        # Function-level linking
        $<$<CONFIG:Release>:/Gw>        # Optimize global data
        $<$<CONFIG:Release>:/arch:AVX2> # Use AVX2 instructions
        $<$<CONFIG:Release>:/Qpar>      # Auto-parallelization
        $<$<CONFIG:Release>:/MT>        # Static runtime library
        $<$<CONFIG:Release>:/DNDEBUG>   # NDEBUG macro
    )

    # Development linker flags
    set(CMAKE_EXE_LINKER_FLAGS_DEVELOPMENT "${CMAKE_EXE_LINKER_FLAGS_DEVELOPMENT} /DEBUG /OPT:REF /OPT:ICF")
    set(CMAKE_SHARED_LINKER_FLAGS_DEVELOPMENT "${CMAKE_SHARED_LINKER_FLAGS_DEVELOPMENT} /DEBUG /OPT:REF /OPT:ICF")

    # Release linker flags - maximum optimization
    set(CMAKE_EXE_LINKER_FLAGS_RELEASE "${CMAKE_EXE_LINKER_FLAGS_RELEASE} /LTCG /OPT:REF /OPT:ICF /RELEASE")
    set(CMAKE_SHARED_LINKER_FLAGS_RELEASE "${CMAKE_SHARED_LINKER_FLAGS_RELEASE} /LTCG /OPT:REF /OPT:ICF /RELEASE")
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