#pragma once

#include <cstdint>

class owdevice
{
public:
  owdevice(int port_num, uint64_t serial_num);
  ~owdevice();

  // called every second
  virtual void process() = 0;

protected:
  int port_num_;
  uint64_t serial_num_;

private:
};
