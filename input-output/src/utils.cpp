#include "utils.h"
#include <sstream>

using namespace std;

namespace home_system
{
namespace input_output
{
namespace ow
{

std::string serial_num_to_string(const std::vector<uchar>& serial_num)
{
  return serial_num_to_string(&serial_num[0]);
}

std::string serial_num_to_string(const uchar* serial_num)
{
  ostringstream o;
  for(int i = 7; i >= 0; i--)
  {
    o.width(2);
    o.fill('0');
    o << hex << uppercase << (int)serial_num[i];
  }
  return o.str();
}

}
}
}

