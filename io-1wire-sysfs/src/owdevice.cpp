#include "owdevice.h"

owdevice::owdevice(uint64_t serial_num, boost::filesystem::path dev_path)
: serial_num_(serial_num),
  dev_path_(dev_path)
{
}

owdevice::~owdevice()
{
}
