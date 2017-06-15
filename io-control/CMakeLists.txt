include_directories(${CMAKE_SOURCE_DIR}/libs/io/include)

set(PROGRAM io-control)

file(GLOB_RECURSE SRCS src/*.cpp)

add_executable(${PROGRAM}
                ${SRCS}
            )

target_link_libraries(${PROGRAM} ${LIBS} io)

# configuration file copy
configure_file(io-control.conf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/io-control.conf COPYONLY)