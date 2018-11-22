cmake_minimum_required(VERSION 3.9)

# for Boost
find_package(Boost COMPONENTS REQUIRED)

# for LLVM
find_package(LLVM REQUIRED CONFIG)

# for threading library
find_package(Threads REQUIRED)

# for google test framework
find_package(GTest REQUIRED)

include_directories(${LLVM_INCLUDE_DIRS} ${Boost_INCLUDE_DIR} ${GTest_INCLUDE_DIR} "../../src")
add_definitions(${LLVM_DEFINITIONS})

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config --cxxflags
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_CXXFLAGS
)

execute_process(
    COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config --ldflags
    COMMAND tr -d \n
    OUTPUT_VARIABLE LLVM_LDFLAGS
)

# print status
message(STATUS "Build type: -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}")
message(STATUS "Google test library: -DGTEST_LIBRARY=${GTEST_LIBRARY}")
message(STATUS "Google test main library: -DGTEST_MAIN_LIBRARY=${GTEST_MAIN_LIBRARY}")
message(STATUS "Using LLVMConfig.cmake in: -DLLVM_DIR=${LLVM_DIR}")
message(STATUS "LLVM version: ${LLVM_PACKAGE_VERSION}")
message(STATUS "LLVM C++ compiler flags: ${LLVM_CXXFLAGS}")
message(STATUS "LLVM linker flags: ${LLVM_LDFLAGS}")

set(CMAKE_CXX_FLAGS "-Wno-gnu-zero-variadic-macro-arguments -fno-rtti -std=c++14 -fPIC ${LLVM_CXXFLAGS}")

if(CMAKE_THREAD_LIBS_INIT)
    set(LIBS ${CMAKE_THREAD_LIBS_INIT} LLVM)
else()
    set(LIBS LLVM)
endif()

add_executable(test test.cpp)
target_link_libraries(test ${LIBS} ${GTEST_LIBRARY} ${GTEST_MAIN_LIBRARY})