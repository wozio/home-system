include_directories(include/com)
file(GLOB_RECURSE SRCS src/*.cpp)
add_library(com STATIC ${SRCS})