#include "device.h"
#include "service.h"

using namespace std;

namespace home_system
{
namespace io
{

device::device(io_id_t id, io_data_type_t data_type, const std::string &type)
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

void device::set_state(long long state)
{
  state_ = static_cast<io_state_t>(state);
  on_state_change(id_);
}

const char* io_state_to_string(io_state_t s)
{
    switch (s)
    {
    case io_state_t::unknown:
        return "unknown";
    case io_state_t::ok:
        return "ok";
    case io_state_t::faulty:
        return "faulty";
    }
}

}
}
