cmake_minimum_required(VERSION 3.7)

project(home-system)

# common include directories
include_directories(${CMAKE_SOURCE_DIR}/common/easyloggingpp/src)
include_directories(${CMAKE_SOURCE_DIR}/common/yami4/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/utils/include)
include_directories(${CMAKE_SOURCE_DIR}/libs/com/include)

# common compiler flags
SET(GXX_COMPILE_FLAGS "-Wno-deprecated-declarations")

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${GXX_COMPILE_FLAGS}")

# common link directories
link_directories(${CMAKE_SOURCE_DIR}/common/yami4/lib)

# common link libraries
set(LIBS
    boost_system
    boost_filesystem
    boost_program_options
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

