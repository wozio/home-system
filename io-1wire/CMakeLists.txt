
include_directories(${CMAKE_SOURCE_DIR}/libs/io/include)
include_directories(src/1wire/common)

set(PROGRAM io-1wire)

file(GLOB_RECURSE SRCS src/*.cpp)

add_executable(${PROGRAM} ${SRCS}
                src/1wire/common/crcutil.c
                src/1wire/common/findtype.c
                src/1wire/common/ioutil.c
                src/1wire/common/owerr.c
                src/1wire/common/temp10.c
                src/1wire/lib/other/libUSB/libusbds2490.c
                src/1wire/lib/other/libUSB/libusblnk.c
                src/1wire/lib/other/libUSB/libusbnet.c
                src/1wire/lib/other/libUSB/libusbses.c
                src/1wire/lib/other/libUSB/libusbtran.c
            )

TARGET_LINK_LIBRARIES(${PROGRAM} ${LIBS} usb io)