include_directories(include/utils)
file(GLOB_RECURSE SRCS src/*.cpp)
add_library(utils STATIC ${SRCS} ${CMAKE_SOURCE_DIR}/libs/3rdparty/easyloggingpp/src/easylogging++.cc)