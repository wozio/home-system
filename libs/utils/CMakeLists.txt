include_directories(include/utils)
file(GLOB_RECURSE SRCS src/*.cpp)
add_library(utils STATIC ${SRCS})