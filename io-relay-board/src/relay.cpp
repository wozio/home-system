#include "relay.h"

relay::relay(home_system::io::io_id_t id)
: home_system::io::device(id, home_system::io::io_data_type_t::integer, "binary_switch")
{

}

relay::~relay()
{

}

void relay::set_value(int value)
{
    boost::any v = value;
    home_system::io::device::set_value(v);
}

int relay::get_value()
{
    return boost::any_cast<int>(home_system::io::device::get_value());
}

int relay::get_wanted_value()
{
    return boost::any_cast<int>(home_system::io::device::get_wanted_value());
}
