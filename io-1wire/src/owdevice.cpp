#include "owdevice.h"

namespace home_system
{

owdevice::owdevice(io_service& ios, int port_num, uint64_t serial_num, io_type_t type)
: io_device(ios, serial_num, type), // serial num may be set as id
  port_num_(port_num),
  serial_num_(serial_num)
{
}

owdevice::~owdevice()
{
}

}
