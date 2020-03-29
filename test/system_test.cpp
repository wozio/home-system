#include "gtest/gtest.h"

namespace
{

TEST(systemTest, DeviceValueChange)
{
    EXPECT_EQ(0, 0);
}

}

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
