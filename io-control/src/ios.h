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

    void kickoff();
private:
    // IO devices keyed by its local name
    std::map<std::string, io_t> io_devices_;

    // IO devices keyed by
    // service name and remote id
    typedef std::map<std::string, std::map<long long, io_t>> io_devices_by_service_t;
    io_devices_by_service_t io_devices_by_service_;

    void on_msg(yami::incoming_message &im);
};
