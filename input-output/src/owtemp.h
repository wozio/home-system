#ifndef OWTEMP_H
#define	OWTEMP_H

#include <boost/circular_buffer.hpp>
#include <vector>

typedef unsigned char uchar;

namespace home_system
{
namespace input_output
{
namespace ow
{

class temp
{
public:
  temp(int portnum, uchar* serial_num);
  void send_convert();
  void read_temp();
  void get_history(std::vector<double>& history);

private:
  int portnum_;
  std::vector<uchar> serial_num_;
  
  struct history_entry
  {
    time_t time_;
    float value_;
  };
  boost::circular_buffer<history_entry> history_;
};

}
}
}

#endif	/* OWTEMP_H */

