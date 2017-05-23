cmake_minimum_required(VERSION 3.7)

enable_testing()

project(home-system)

# common include directories
include_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/rapidjson/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/utils/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/com/include)

# common compiler flags
SET(GXX_COMPILE_FLAGS "-Wno-deprecated-declarations")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${GXX_COMPILE_FLAGS}")

add_definitions(-DBOOST_ALL_DYN_LINK)

# common link directories
link_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/yami4/lib)

# common link libraries
set(LIBS
    boost_system
    boost_filesystem
    boost_program_options
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
add_subdirectory(${CMAKE_SOURCE_DIR}/io-1wire)
add_subdirectory(${CMAKE_SOURCE_DIR}/io-relay-board)
add_subdirectory(${CMAKE_SOURCE_DIR}/io-control)

# 3rd party libraries
add_subdirectory(${CMAKE_SOURCE_DIR}/libs/3rdparty/googletest)
