include_directories(${CMAKE_SOURCE_DIR}/libs/io/include)

set(PROGRAM io-control)

file(GLOB_RECURSE SRCS src/*.cpp)

add_executable(${PROGRAM}
                ${SRCS}
            )

target_link_libraries(${PROGRAM} ${LIBS} io lua5.3-c++)

# configuration file copy
configure_file(io-control.conf ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/io-control.conf COPYONLY)
configure_file(example_rule.lua ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/example_rule.lua COPYONLY)
configure_file(io-control.lua ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/io-control.lua COPYONLY)