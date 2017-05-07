#pragma once

#include "io/io_device.h"

namespace home_system
{

class relay : public io_device
{
public:
    relay(io_service& ios, io_id_t id);
    ~relay();

    void check_value(int value);

private:

};

}