cmake_minimum_required(VERSION 3.30)
project(test LANGUAGES C)

add_executable(test "test.c")

set_target_properties(test PROPERTIES
    C_STANDARD 99
    C_STANDARD_REQUIRED TRUE
    C_EXTENSIONS DISABLED
    EXPORT_COMPILE_COMMANDS TRUE
)

target_compile_options(test PRIVATE -g -Wall -pedantic -march=native)
