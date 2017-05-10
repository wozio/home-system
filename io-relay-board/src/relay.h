#pragma once

#include "io/io_device.h"
#include "timer.h"

namespace home_system
{

class relay : public io_device
{
public:
    relay(io_id_t id, boost::asio::serial_port& serial_port);
    ~relay();
l
    void set_wanted_value(const boost::any& v);

    void check_value(int value);
    int get_value();
    int get_wanted_value();
    

private:
};

}