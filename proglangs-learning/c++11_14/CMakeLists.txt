cmake_minimum_required(VERSION 3.1.0)

set(CMAKE_CXX_EXTENSIONS off)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")

file( GLOB_RECURSE SOURCES *.cc )
#message("*.cc ${SOURCES}")

foreach(cpp_file ${SOURCES})
  #message("cpp_file ${cpp_file}")
  get_filename_component(bin_file ${cpp_file} NAME_WE)
  #message("bin_file ${bin_file}")

  add_executable(${bin_file} ${cpp_file})
  set_property(TARGET ${bin_file} PROPERTY CXX_STANDARD 14)
endforeach(cpp_file)
