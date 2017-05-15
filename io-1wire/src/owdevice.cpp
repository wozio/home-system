#include "owdevice.h"

owdevice::owdevice(int port_num, uint64_t serial_num, home_system::io::io_data_type_t data_type, const std::string& type)
: home_system::io::device(serial_num, data_type, type), // serial num may be set as id
  port_num_(port_num),
  serial_num_(serial_num)
{
}

owdevice::~owdevice()
{
}
