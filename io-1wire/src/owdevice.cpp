#include "owdevice.h"

owdevice::owdevice(int port_num, uint64_t serial_num)
: 
  port_num_(port_num),
  serial_num_(serial_num)
{
}

owdevice::~owdevice()
{
}
