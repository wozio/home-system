#include "device_float.h"
#include "utils/logger.h"

using namespace std;

namespace home_system
{
namespace io
{

device_float::device_float(io_id_t id, const std::string &type)
: device(id, home_system::io::io_data_type_t::double_float, type)
{
}

double device_float::get_value()
{
    if (get_state() != io_state_t::ok)
    {
        throw runtime_error("Attempt to read value while state not ok");
    }
    return value_;
}

void device_float::set_wanted_value(double v)
{
  if (wanted_value_)
  {
    wanted_value_ = v;
    on_wanted_value_change(get_id());
  }
}

double device_float::get_wanted_value()
{
    return wanted_value_;
}

void device_float::set_value(double v)
{
  if (value_ != v)
  {
    value_ = v;
    on_value_change(get_id());
  }
}

void device_float::extract_value(const yami::parameters& params)
{
  try
  {
    yami::parameter_type t = params.type("value");
    LOG(DEBUG) << "value entry type: " << t;
    double v = params.get_double_float("value");
    set_value(v);
  }
  catch (const std::exception& e)
  {
    LOG(ERROR) << "EXCEPTION: " << e.what();
  }
}

void device_float::extract_wanted_value(const yami::parameters& params)
{
  try
  {
    auto v = params.get_double_float("value");
    if (v != wanted_value_)
    {
      wanted_value_ = v;
      on_wanted_value_change(get_id());
    }
  }
  catch (const std::exception& e)
  {
    LOG(ERROR) << "EXCEPTION: " << e.what();
  }
}

void device_float::write_value(yami::parameters& params)
{
  params.set_double_float("value", value_);
}

}
}
