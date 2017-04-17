
include_directories(libio/include)
include_directories(src/1wire/common)

file(GLOB_RECURSE SRCS src/*.cpp)

add_executable(input-output ${SRCS}
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
                libio/src/io_service.cpp
                libio/src/io_device.cpp
            )

TARGET_LINK_LIBRARIES(input-output ${LIBS} usb)