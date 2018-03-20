#pragma once

#include "owdevice.h"
#include "io/device_float.h"

class temp
: public owdevice,
  public home_system::io::device_float
{
public:
  temp(int port_num, uint64_t serial_num);
  ~temp();
  void process();
  
private:
  int process_cnt_;

  void send_convert();
  bool read_temp();
};
