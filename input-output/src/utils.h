#ifndef UTILS_H
#define	UTILS_H

#include <string>
#include <vector>

typedef unsigned char uchar;

namespace home_system
{
namespace input_output
{
namespace ow
{

std::string serial_num_to_string(const std::vector<uchar>& serial_num);
std::string serial_num_to_string(const uchar* serial_num);

}
}
}

#endif	/* UTILS_H */

