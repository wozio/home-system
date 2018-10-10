#pragma once

#include "io/device_int.h"

enum class gpio_mode
{
  output,
  input
};

class gpio
: public home_system::io::device_int
{
public:
  gpio(int port, gpio_mode mode);
  ~gpio();

protected:
  void exec_value_change();

private:
  int port_;
  gpio_mode mode_;
  int fd_;
};
