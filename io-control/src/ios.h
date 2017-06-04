#pragma once

#include "io.h"
#include "io/device_types.h"
#include <map>
#include <memory>

class ios;

typedef std::unique_ptr<ios> ios_t;

class ios
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

    std::map<home_system::io::io_id_t, io_t> io_devices_;
};
