cmake_minimum_required(VERSION 4.0)
project(sample C)
add_executable(sample main.c)

include(CTest)
add_test(NAME sample_runs COMMAND sample)

include(CPack)
