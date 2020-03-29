#include "gtest/gtest.h"
#include <boost/process/child.hpp>
#include <boost/process/io.hpp>
#include <boost/process/start_dir.hpp>

namespace bp = boost::process;

namespace
{

TEST(IoControlTest, DeviceValueChange)
{
  bp::opstream in;
  bp::ipstream out;
  bp::child c(bp::start_dir("./"), "io-control", bp::std_out > out, bp::std_in < in);

  in << "quit" << std::endl;

  while (c.running())
  {
    
  }

  c.wait(); //wait for the process to exit
  int result = c.exit_code();

  EXPECT_EQ(0, result);
}

}

int main(int argc, char **argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
