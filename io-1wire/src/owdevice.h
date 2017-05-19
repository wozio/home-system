#pragma once

#include "io/device.h"

class owdevice
: public home_system::io::device
{
public:
  owdevice(int port_num, uint64_t serial_num, home_system::io::io_data_type_t data_type, const std::string& type);
  ~owdevice();

  // called every second
  virtual void process() = 0;

protected:
  int port_num_;
  uint64_t serial_num_;

private:
};
