
set(PROGRAM io-control)

file(GLOB_RECURSE SRCS src/*.cpp)

add_executable(${PROGRAM}
                src/stdafx.h
                ${SRCS}
            )

TARGET_LINK_LIBRARIES(${PROGRAM} ${LIBS})