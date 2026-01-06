cmake_minimum_required(VERSION 3.10)
project(minimal-cling-parser)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

#######################
# Rely on llvm-config #
#######################

set(CONFIG_OUTPUT)
find_program(LLVM_CONFIG "llvm-config")
if(LLVM_CONFIG)
    message(STATUS "Found LLVM_CONFIG as ${LLVM_CONFIG}")
    set(CONFIG_COMMAND ${LLVM_CONFIG}
        "--assertion-mode"
        "--bindir"
        "--libdir"
        "--includedir"
        "--prefix")
    execute_process(COMMAND ${CONFIG_COMMAND}
                    RESULT_VARIABLE HAD_ERROR
                    OUTPUT_VARIABLE CONFIG_OUTPUT)
    if(NOT HAD_ERROR)
        string(REGEX REPLACE
               "[ \t]*[\r\n]+[ \t]*" ";"
               CONFIG_OUTPUT ${CONFIG_OUTPUT})
    else()
        string(REPLACE ";" " " CONFIG_COMMAND_STR "${CONFIG_COMMAND}")
        message(STATUS "${CONFIG_COMMAND_STR}")
        message(FATAL_ERROR "llvm-config failed with status ${HAD_ERROR}")
    endif()
else()
    message(FATAL_ERROR "llvm-config not found -- ${LLVM_CONFIG}")
endif()

list(GET CONFIG_OUTPUT 0 ENABLE_ASSERTIONS)
list(GET CONFIG_OUTPUT 1 TOOLS_BINARY_DIR)
list(GET CONFIG_OUTPUT 2 LIBRARY_DIR)
list(GET CONFIG_OUTPUT 3 INCLUDE_DIR)
list(GET CONFIG_OUTPUT 4 LLVM_OBJ_ROOT)

if(NOT MSVC_IDE)
    set(LLVM_ENABLE_ASSERTIONS ${ENABLE_ASSERTIONS} CACHE BOOL "Enable assertions")
    mark_as_advanced(LLVM_ENABLE_ASSERTIONS)
endif()

set(LLVM_TOOLS_BINARY_DIR ${TOOLS_BINARY_DIR} CACHE PATH "Path to llvm/bin")
set(LLVM_LIBRARY_DIR ${LIBRARY_DIR} CACHE PATH "Path to llvm/lib")
set(LLVM_MAIN_INCLUDE_DIR ${INCLUDE_DIR} CACHE PATH "Path to llvm/include")
set(LLVM_BINARY_DIR ${LLVM_OBJ_ROOT} CACHE PATH "Path to LLVM build tree")

include_directories(${LLVM_MAIN_INCLUDE_DIR})
link_directories(${LLVM_LIBRARY_DIR})
add_definitions(-DLLVM_DIR="${LLVM_BINARY_DIR}")

################
# Dependencies #
################

find_package(Clang REQUIRED)
find_package(Cling REQUIRED)

#########
# flags #
#########

include(CheckCXXCompilerFlag)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING
        "Choose the build type, e.g. Release or Debug." FORCE)
endif()

if(MSVC)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4251 /wd4141")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd4018 /wd4267 /wd4715 /wd4146 /wd4129")
endif()

if (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID MATCHES "GNU" OR CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wunused-parameter -Wextra -Wreorder")

    CHECK_CXX_COMPILER_FLAG("-std=c++17" HAS_CPP17_FLAG)

    if (HAS_CPP17_FLAG)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
    else()
        message(FATAL_ERROR "Unsupported compiler -- requires C++17 support!")
    endif()
endif()

###################
# Target and link #
###################

# Build minimal parser
add_executable(cling-parser src/main.cpp)
target_link_libraries(cling-parser
    clingInterpreter
    clingMetaProcessor
    clingUtils
)

install(TARGETS cling-parser DESTINATION bin)
