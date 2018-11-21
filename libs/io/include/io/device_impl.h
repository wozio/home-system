#pragma once

#include "device.h"
#include "utils/logger.h"
#include <stdexcept>

namespace home_system
{
namespace io
{

void extract_value_impl(long long& value, const yami::parameters& params, const char* name);
void write_value_impl(long long& value, yami::parameters& params, const char* name);
void extract_value_impl(double& value, const yami::parameters& params, const char* name);
void write_value_impl(double& value, yami::parameters& params, const char* name);

template<typename T>
class device_impl
: public device
{
public:
  device_impl(io_id_t id, const std::string& type);
  
  void set_value(T v)
  {
    if (value_ != v)
    {
      LOG(TRACE) << "IO [" << get_id() << "]: set to value=" << v;
      value_ = v;
      on_value_change(get_id());
      if (value_ != wanted_value_)
      {
        exec_value_change();
      }
    }
  }

  T get_value()
  {
    if (get_state() != io_state_t::ok)
    {
      throw std::runtime_error("Attempt to read value while state not ok");
    }
    return value_;
  }

  void set_wanted_value(T v)
  {
    if (wanted_value_ != v)
    {
      LOG(TRACE) << "IO: " << get_id() << " set to wanted_value=" << v;
      wanted_value_ = v;
      on_wanted_value_change(get_id());
    }
  }

  T get_wanted_value()
  {
    return wanted_value_;
  }

  void extract_value(const yami::parameters& params)
  {
    try
    {
      T v;
      extract_value_impl(v, params, "value");
      set_value(v);
    }
    catch (const std::exception& e)
    {
      LOG(ERROR) << "EXCEPTION: " << e.what();
    }
  }

  void extract_wanted_value(const yami::parameters& params)
  {
    try
    {
      T v;
      extract_value_impl(v, params, "wanted_value");

      if (v != wanted_value_)
      {
        wanted_value_ = v;
        exec_value_change();
        on_wanted_value_change(get_id());
      }
    }
    catch (const std::exception& e)
    {
      LOG(ERROR) << "EXCEPTION: " << e.what();
    }
  }

  void write_value(yami::parameters& params)
  {
    write_value_impl(value_, params, "value");
  }

private:
  T value_;
  T wanted_value_;
};

}
}