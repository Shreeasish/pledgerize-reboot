cmake_minimum_required(VERSION 2.8)
project(llvm-dataflow-analysis)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
list(APPEND CMAKE_MODULE_PATH "${LLVM_DIR}")

set(PACKAGE_NAME llvm-dataflow-analysis)
set(PACKAGE_VERSION 0.1)
set(PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
set(PACKAGE_BUGREPORT "wsumner@sfu.ca")

set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17 -fno-rtti -fsanitize=address,undefined -O1 -fno-omit-frame-pointer -Wall -Wtautological-unsigned-zero-compare")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

############## LLVM CONFIGURATION #################

# LLVM_DIR must be set to the prefix of /share/llvm/cmake via commandline
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# We incorporate the CMake features provided by LLVM:
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

option(LLVM_ENABLE_CXX1Z  "Enable C++17" ON)
option(LLVM_INCLUDE_TOOLS "Generate build targets for the LLVM tools." ON)
option(LLVM_BUILD_TOOLS
  "Build the LLVM tools. If OFF, just generate build targets." ON
)

message("LLVM STATUS:
  Definitions ${LLVM_DEFINITIONS}
  Includes    ${LLVM_INCLUDE_DIRS}
  Libraries   ${LLVM_LIBRARY_DIRS}
  Targets     ${LLVM_TARGETS_TO_BUILD}"
)

# Now set the LLVM header and library paths:
include_directories(${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
add_definitions(${LLVM_DEFINITIONS})

############### SVF-CONFIGURATION ##################

if (DEFINED ENV{SVF_HOME})
    message(STATUS "SVF_HOME: $ENV{SVF_HOME}")
    set(SVF_HEADER "$ENV{SVF_HOME}/include")
    set(SVF_LIB "$ENV{SVF_HOME}/Release-build/lib")
    if (EXISTS "${SVF_HEADER}" AND EXISTS "${SVF_LIB}")
        message(STATUS "SVF_HEADER: ${SVF_HEADER}")
        message(STATUS "SVF_LIB: ${SVF_LIB}")
    else()
        message(FATAL_ERROR "${SVF_LIB} or ${SVF_HEADER} not exists")
    endif()
    include_directories(${CMAKE_SOURCE_DIR}/include ${SVF_HEADER})
    link_directories(${LIBRARY_OUTPUT_PATH} ${SVF_LIB} ${SVF_LIB}/CUDD)
else()
    message(FATAL_ERROR "Please point SVF_HOME to your SVF source folder")
endif()

############## FINAL PROJECT CONFIG #################

# And the project header and library paths
#include_directories(${CMAKE_SOURCE_DIR}/include)
#link_directories(${LIBRARY_OUTPUT_PATH})
set(CMAKE_TEMP_LIBRARY_PATH "${PROJECT_BINARY_DIR}/lib")
# TODO: Add install path to the list....

add_subdirectory(tools)
