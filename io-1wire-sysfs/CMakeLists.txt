include_directories(${CMAKE_SOURCE_DIR}/libs/io/include)

set(PROGRAM io-1wire-sysfs)

file(GLOB_RECURSE SRCS src/*.cpp)

add_executable(${PROGRAM} ${SRCS}
            )

TARGET_LINK_LIBRARIES(${PROGRAM} ${LIBS} io)