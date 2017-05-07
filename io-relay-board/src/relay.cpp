#include "relay.h"

namespace home_system
{

relay::relay(io_service& ios, io_id_t id)
: io_device(ios, id)
{

}

relay::~relay()
{

}

void relay::check_value(int value)
{

}

}