#include "device_int.h"
#include "utils/logger.h"

using namespace std;

namespace home_system
{
namespace io
{

device_int::device_int(io_id_t id, const std::string &type)
: device(id, home_system::io::io_data_type_t::integer, type)
{
}

long long device_int::get_value()
{
    if (get_state() != io_state_t::ok)
    {
        throw runtime_error("Attempt to read value while state not ok");
    }
    return value_;
}

void device_int::set_wanted_value(long long v)
{
  if (wanted_value_)
  {
    wanted_value_ = v;
    on_wanted_value_change(get_id());
  }
}

long long device_int::get_wanted_value()
{
    return wanted_value_;
}

void device_int::set_value(long long v)
{
  if (value_ != v)
  {
    value_ = v;
    on_value_change(get_id());
  }
}

void device_int::extract_value(const yami::parameters& params)
{
  try
  {
    auto v = params.get_long_long("value");
    set_value(v);
  }
  catch (const std::exception& e)
  {
    LOG(ERROR) << "EXCEPTION: " << e.what();
  }
}

void device_int::extract_wanted_value(const yami::parameters& params)
{
  try
  {
    auto v = params.get_long_long("value");
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

void device_int::write_value(yami::parameters& params)
{
  params.set_long_long("value", value_);
}

}
}
