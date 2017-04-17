#ifndef UTILS_H
#define	UTILS_H

#include <string>
#include <vector>
#include <sstream>

typedef unsigned char uchar;

namespace home_system
{
template<class T>
std::string serial_num_to_string(const T serial_num)
{
  std::ostringstream o;
  o.width(sizeof(T) * 2);
  o.fill('0');
  o << std::hex << std::uppercase << serial_num;
  return o.str();
}

}

#endif	/* UTILS_H */

