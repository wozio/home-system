#include "device_impl.h"

namespace home_system
{
namespace io
{

template<> device_impl<long long>::device_impl(io_id_t id, const std::string& type)
: device(id, io_data_type_t::integer, type),
  value_(0),
  wanted_value_(0)
{
}

template<> device_impl<double>::device_impl(io_id_t id, const std::string& type)
: device(id, io_data_type_t::double_float, type),
  value_(0.0),
  wanted_value_(0.0)
{
}

void extract_value_impl(long long& value, const yami::parameters& params, const char* name)
{
  value = params.get_long_long(name);
}
void write_value_impl(long long& value, yami::parameters& params, const char* name)
{
  params.set_long_long(name, value);
}
void extract_value_impl(double& value, const yami::parameters& params, const char* name)
{
  value = params.get_double_float(name);
}
void write_value_impl(double& value, yami::parameters& params, const char* name)
{
  params.set_double_float(name, value);
}

}
}