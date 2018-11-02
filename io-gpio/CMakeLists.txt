
include_directories(${CMAKE_SOURCE_DIR}/libs/io/include)

file(GLOB_RECURSE SRCS src/*.cpp)

set(PROGRAM io-gpio)

add_executable(${PROGRAM} ${SRCS})

target_link_libraries(${PROGRAM} ${LIBS} io)