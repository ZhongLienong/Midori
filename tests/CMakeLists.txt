file(GLOB_RECURSE MIDORI_UNIT_TEST_SOURCES CONFIGURE_DEPENDS
    "unit/*.cpp"
)

add_executable(MidoriUnitTests
    ${MIDORI_UNIT_TEST_SOURCES}
)

target_link_libraries(MidoriUnitTests PRIVATE
    Catch2::Catch2WithMain
    MidoriCore
)

if(MSVC)
    target_compile_options(MidoriUnitTests PRIVATE /EHsc)
endif()

include(${catch2_SOURCE_DIR}/extras/Catch.cmake)
catch_discover_tests(MidoriUnitTests)
