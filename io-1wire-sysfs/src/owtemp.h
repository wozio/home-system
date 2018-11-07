#pragma once

#include "owdevice.h"
#include "io/device_float.h"

class temp
: public owdevice,
  public home_system::io::device_float
{
public:
  temp(uint64_t serial_num, const std::string& dev_path);
  ~temp();
  void process();
  
private:
  int process_cnt_;

  void read_temp();
};
