#pragma once

#include "device.h"

namespace home_system
{
namespace io
{

class device_int
: public device
{
public:
  device_int(io_id_t id, const std::string& type);

  void set_value(long long v);
  long long get_value();
  void set_wanted_value(long long v);
  long long get_wanted_value();

  void extract_value(const yami::parameters& params);
  void extract_wanted_value(const yami::parameters& params);
  void write_value(yami::parameters& params);

private:
  long long value_;
  long long wanted_value_;
};

}
}