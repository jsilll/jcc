cmake_minimum_required(VERSION 3.10)

project(Compiler VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)
set(CMAKE_C_EXTENSIONS OFF)

add_executable(cc src/main.c src/arena.c)

if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
    target_compile_options(cc PRIVATE -Wall -Wextra -pedantic)
elseif (CMAKE_C_COMPILER_ID STREQUAL "Clang")
    target_compile_options(cc PRIVATE -Wall -Wextra -pedantic)
elseif (CMAKE_C_COMPILER_ID STREQUAL "MSVC")
    target_compile_options(cc PRIVATE /W4 /permissive-)
endif()