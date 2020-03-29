include_directories(${CMAKE_SOURCE_DIR}/libs/3rdparty/googletest/googletest/include)
add_executable(systemTest system_test.cpp)
target_link_libraries(systemTest ${LIBS} gtest)
add_test(NAME systemTest COMMAND systemTest)

#add_dependencies(systemTest io-control)