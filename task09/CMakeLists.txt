cmake_minimum_required(VERSION 3.1)
project(task09)

set(THREADS_PREFER_PTHREAD_FLAG TRUE)
find_package(Threads REQUIRED)

add_library(doctest INTERFACE)
target_include_directories(doctest INTERFACE doctest)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED on)

if ( CMAKE_COMPILER_IS_GNUCC )
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra -Werror")
endif()
if ( MSVC )
    set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} /W4 /WX")
endif()

include_directories(src)

add_executable(queue_test src/queue_test.cpp src/queue.cpp)
target_link_libraries(queue_test doctest Threads::Threads)

add_executable(tsqueue_test src/tsqueue_test.cpp src/tsqueue.cpp src/queue.cpp)
target_link_libraries(tsqueue_test doctest Threads::Threads)
