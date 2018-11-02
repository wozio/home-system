cmake_minimum_required(VERSION 3.7)

enable_testing()

project(home-system)

# output paths
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# common include directories
include_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/rapidjson/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/utils/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/com/include)

# common compiler flags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D__FILENAME__='\"$(subst ${CMAKE_SOURCE_DIR}/,,$(abspath $<))\"'")

add_definitions(-DBOOST_ALL_DYN_LINK)

# YAMI4 dependency
file(MAKE_DIRECTORY ${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/lib)

add_custom_target(yami4core
                  COMMAND make
                  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/src/core/)

add_custom_target(yami4cpp
                  COMMAND make
                  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/src/cpp/)
add_dependencies(yami4cpp yami4core)

set(YAMI4DEPS yami4cpp)
link_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/lib)

# common link libraries
set(LIBS
    boost_system
    boost_filesystem
    boost_program_options
    boost_thread
    boost_log_setup
    boost_log
    com
    utils
    pthread
    PocoNet
    yamicpp
    yamicore
    )

add_subdirectory(${CMAKE_SOURCE_DIR}/libs/utils)
add_subdirectory(${CMAKE_SOURCE_DIR}/libs/com)
add_subdirectory(${CMAKE_SOURCE_DIR}/libs/io)
#add_subdirectory(${CMAKE_SOURCE_DIR}/io-1wire)
add_subdirectory(${CMAKE_SOURCE_DIR}/io-1wire-sysfs)
#add_subdirectory(${CMAKE_SOURCE_DIR}/io-relay-board)
add_subdirectory(${CMAKE_SOURCE_DIR}/io-control)
add_subdirectory(${CMAKE_SOURCE_DIR}/io-gpio)

# 3rd party libraries
add_subdirectory(${CMAKE_SOURCE_DIR}/libs/3rdparty/googletest)
