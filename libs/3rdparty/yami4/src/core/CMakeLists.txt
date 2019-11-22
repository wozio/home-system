include_directories(posix)
file(GLOB SRCS posix/*.cpp)
file(GLOB SRCS *.cpp)
add_library(yamicore STATIC ${SRCS})