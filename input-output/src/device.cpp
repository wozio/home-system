#include "logger.h"
#include "device.h"
#include "utils.h"

using namespace std;

namespace home_system
{

device::device(int portnum, uint64_t serial_num)
: portnum_(portnum),
  serial_num_(serial_num),
  state_(unknown)
{
  LOG(DEBUG) << "Created 1-wire device: " << serial_num_to_string(serial_num_);
}

uint64_t device::get_id()
{
  return serial_num_;
}

state_t device::get_state()
{
    return state_;
}

void device::set_state(state_t state)
{
    state_ = state;
}

boost::any device::get_value()
{
    if (state_ != ok)
    {
        throw runtime_error("Attempt to read value while state not ok ");
    }
    return value_;
}

void device::set_value(boost::any& value)
{
    value_ = value;
}

}
