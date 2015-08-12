#include "utils.h"
#include <sstream>

using namespace std;

namespace home_system
{
namespace input_output
{
namespace ow
{

std::string serial_num_to_string(const uint64_t serial_num)
{
  ostringstream o;
  o.width(16);
  o.fill('0');
  o << hex << uppercase << serial_num;
  return o.str();
}

}
}
}

