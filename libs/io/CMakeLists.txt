include_directories(include)
file(GLOB_RECURSE SRCS src/*.cpp)
add_library(io STATIC ${SRCS})