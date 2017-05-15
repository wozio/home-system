include_directories(include/io)
file(GLOB_RECURSE SRCS src/*.cpp)
add_library(io STATIC ${SRCS})