#ifndef OWTEMP_H
#define	OWTEMP_H

#include <cstdint>

namespace home_system
{
namespace input_output
{
namespace ow
{

class temp
{
public:
  temp(int portnum, uint64_t serial_num, std::function<void(uint64_t)> state_change_callback);
  void send_convert();
  bool read_temp();
  
  uint64_t id();
  float get_value();
  long long get_time();

private:
  int portnum_;
  float value_;
  long long time_;
  uint64_t serial_num_;
  std::function<void(uint64_t)> state_change_callback_;
};

}
}
}

#endif	/* OWTEMP_H */

