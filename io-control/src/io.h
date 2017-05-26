#pragma once

#include "io/device_types.h"

class io;

typedef std::shared_ptr<io> io_t;

class io
{
public:
    static io_t create(home_system::io::io_data_type_t data_type,
        const std::string& type,
        home_system::io::io_mode_t mode,
        const std::string& name,
        const std::string& service,
        long long id);

    io(const std::string& type, const std::string& name, const std::string& service, long long id);
    virtual ~io();

    // in params 'value' field in proper type must be present
    virtual void on_value_state_change(const yami::parameters& params) = 0;

    // 'value' field in proper type will be written into params
    virtual void write_value_state(yami::parameters& params) = 0;

    
private:

    const std::string type_;
    const std::string name_;
    const std::string service_;
    const long long id_;
    home_system::io::io_state_t state_;
};
