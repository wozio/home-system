#pragma once

#include "owdevice.h"
#include <cstdint>

namespace home_system
{
class temp
: public owdevice
{
public:
  temp(io_service& ios, int port_num, uint64_t serial_num);
  ~temp();
  void process();
  
private:
  int process_cnt_;

  void send_convert();
  bool read_temp();
};

}
