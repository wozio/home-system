#include "logger.h"
#include "io/device.h"
#include "io/service.h"

using namespace std;

namespace home_system
{

device::device(io_id_t id, io_data_type_t data_type, const std::string& type)
: id_(id),
  data_type_(data_type),
  type_(type),
  state_(io_state_t::unknown)
{
}

device::~device()
{
}

io_id_t device::get_id()
{
  return id_;
}

io_data_type_t device::get_data_type()
{
    return data_type_;
}

std::string device::get_type()
{
    return type_;
}

io_state_t device::get_state()
{
    return state_;
}

void device::set_state(io_state_t state)
{
    state_ = state;
    on_state_change(id_);
}

boost::any& device::get_value()
{
    if (state_ != io_state_t::ok)
    {
        throw runtime_error("Attempt to read value while state not ok");
    }
    return value_;
}

void device::set_wanted_value(const boost::any& v)
{
    wanted_value_ = v;
}

void device::set_value(boost::any& value)
{
    value_ = value;
    on_state_change(id_);
}

}
