include_directories(include/io)

# library
file(GLOB_RECURSE SRCS src/*.cpp)
add_library(io STATIC ${SRCS})

# unit test
include_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/googletest/googletest/include)
add_executable(test_io test/io_test.cpp)
target_link_libraries(test_io ${LIBS} io gtest)
add_test(NAME ioUnitTest COMMAND test_io)