#pragma once

#include "io/io_device.h"

namespace home_system
{

class owdevice
: public io_device
{
public:
  owdevice(io_service& ios, int port_num, uint64_t serial_num, io_type_t type);
  ~owdevice();

  // called every second
  virtual void process() = 0;

protected:
  int port_num_;
  uint64_t serial_num_;

private:
};

}
