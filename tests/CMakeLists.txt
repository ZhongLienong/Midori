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
