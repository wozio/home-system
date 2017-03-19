#pragma once

#include "device.h"
#include <cstdint>

namespace home_system
{
namespace input_output
{
namespace ow
{

class temp
: public device
{
public:
  temp(int portnum, uint64_t serial_num, std::function<void(uint64_t)> state_change_callback);
  void send_convert();
  bool read_temp();

  void process();
  
private:
};

}
}
}
