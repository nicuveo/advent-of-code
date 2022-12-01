# project

project(aoc)
cmake_minimum_required(VERSION 3.10)

# compilation

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif(NOT CMAKE_BUILD_TYPE)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic")

include_directories(src)
include_directories(lib)

# shared code

file(GLOB_RECURSE LIB_FILES lib/*.cc)
add_library(aoc ${LIB_FILES})

# days

file(GLOB DAYS src/day*.cc)
foreach(DAYF ${DAYS})
  get_filename_component(DAY ${DAYF} NAME_WE)
  add_executable(${DAY} ${DAYF})
  add_dependencies(${DAY} aoc)
  target_link_libraries(${DAY} aoc)
endforeach()
