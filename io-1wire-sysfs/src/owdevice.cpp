#include "owdevice.h"

owdevice::owdevice(uint64_t serial_num, const std::string& dev_path)
: serial_num_(serial_num),
  dev_path_(dev_path)
{
}

owdevice::~owdevice()
{
}
