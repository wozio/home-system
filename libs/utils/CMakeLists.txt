include_directories(include/utils)
file(GLOB_RECURSE SRCS src/*.cpp)
add_definitions(-DBOOST_FILESYSTEM_NO_DEPRECATED)
add_library(utils STATIC ${SRCS})