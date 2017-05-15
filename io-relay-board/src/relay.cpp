#include "relay.h"

relay::relay(home_system::io::io_id_t id)
: home_system::io::device(id, io_data_type_t::integer, "binary_switch")
{

}

relay::~relay()
{

}

void relay::set_value(int value)
{
    boost::any v = value;
    set_value(v);
}

void relay::set_wanted_value(const boost::any& v)
{

}
