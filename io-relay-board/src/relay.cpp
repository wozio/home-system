#include "relay.h"

namespace home_system
{

relay::relay(home_system::io::io_id_t id, boost::asio::serial_port& serial_port)
: home_system::io::device(id, io_data_type_t::integer, "binary_switch"),
  serial_port_(serial_port)
{

}

relay::~relay()
{

}

void relay::check_value(int value)
{
    boost::any v = value;
    set_value(v);
}

void relay::set_wanted_value(const boost::any& v)
{

}

}