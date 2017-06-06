#pragma once

#include "io.h"
#include "io/device_types.h"
#include "com/service.h"
#include <map>
#include <memory>

class ios;

typedef std::unique_ptr<ios> ios_t;

/// Owner of all IO devices objects and service for
/// subscribing for notifications from IO device drivers
/// which are passed to proper objects.
/// Done this way to avoid service in each IO device object.
/// Entry point for subscriptions from clients
class ios
: public home_system::com::service
{
public:
    static ios_t create()
    {
        return ios_t(new ios());
    }
    ios();
    ~ios();

    io_t get(const std::string& name);
private:

    std::map<std::string, io_t> io_devices_;

    void on_msg(yami::incoming_message &im);
};
