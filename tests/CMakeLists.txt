file(GLOB_RECURSE MIDORI_UNIT_TEST_SOURCES CONFIGURE_DEPENDS
    "unit/*.cpp"
)

file(GLOB_RECURSE MIDORI_TEST_SUPPORT_SOURCES CONFIGURE_DEPENDS
    "support/*.cpp"
)

file(GLOB_RECURSE MIDORI_TEST_SUPPORT_HEADERS CONFIGURE_DEPENDS
    "support/*.h"
)

add_library(MidoriTestSupport STATIC
    ${MIDORI_TEST_SUPPORT_SOURCES}
    ${MIDORI_TEST_SUPPORT_HEADERS}
)

target_include_directories(MidoriTestSupport PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(MidoriTestSupport PUBLIC
    MidoriCore
)

add_executable(MidoriUnitTests
    ${MIDORI_UNIT_TEST_SOURCES}
)

target_include_directories(MidoriUnitTests PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# The root CMakeLists.txt only defines MIDORI_BUILD_DEBUG/MIDORI_BUILD_DEVELOPMENT
# (BuildConfig.h's MIDORI_DEBUG_LEVEL, which in turn gates
# MIDORI_ENABLE_OPTIMIZER_STATS and friends) for MidoriCore and Midori -
# add_subdirectory(tests) runs after that foreach, so these two targets never
# received it and always compiled at MIDORI_DEBUG_LEVEL 1. That is harmless
# for headers whose declarations do not change shape under these flags, but
# OptimizerManager::Optimize's parameter list is guarded by
# MIDORI_ENABLE_OPTIMIZER_STATS, so a test target built without this define
# calls a different overload than the one MidoriCore's object files provide -
# an unresolved external symbol at link time, not a compile error, the first
# time a test actually calls it. Mirror the same per-config defines here so
# every target that links MidoriCore sees BuildConfig.h identically.
foreach(midori_test_target IN ITEMS MidoriTestSupport MidoriUnitTests)
    target_compile_definitions(${midori_test_target} PRIVATE
        $<$<CONFIG:Debug>:MIDORI_BUILD_DEBUG>
        $<$<CONFIG:Development>:MIDORI_BUILD_DEVELOPMENT>
        $<$<OR:$<CONFIG:Release>,$<CONFIG:Experimental>>:NDEBUG>
    )
endforeach()

target_link_libraries(MidoriUnitTests PRIVATE
    Catch2::Catch2WithMain
    MidoriTestSupport
)

if(MSVC)
    target_compile_options(MidoriTestSupport PRIVATE /EHsc)
    target_compile_options(MidoriUnitTests PRIVATE /EHsc)
endif()

include(${catch2_SOURCE_DIR}/extras/Catch.cmake)
catch_discover_tests(MidoriUnitTests)
