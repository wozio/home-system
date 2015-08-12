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
  temp(int portnum, uint64_t serial_num);
  
  void send_convert();
  bool read_temp();
  
  uint64_t id();
  void get_value(double& value);

private:
  int portnum_;
  uint64_t serial_num_;
  float value_;
};

}
}
}

#endif	/* OWTEMP_H */

