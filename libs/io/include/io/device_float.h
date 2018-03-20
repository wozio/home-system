#pragma once

#include "device.h"

namespace home_system
{
namespace io
{

class device_float
: public device
{
public:
  device_float(io_id_t id, const std::string& type);

  void set_value(double v);
  double get_value();
  void set_wanted_value(double v);
  double get_wanted_value();

  void extract_value(const yami::parameters& params);
  void extract_wanted_value(const yami::parameters& params);
  void write_value(yami::parameters& params);

private:
  double value_;
  double wanted_value_;
};

}
}