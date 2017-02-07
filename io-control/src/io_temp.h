#pragma once
#include "io.h"

namespace home_system
{
class io_temp : public io
{
public:
    io_temp(const std::string& type, const std::string& name, const std::string& service, long long id);
    ~io_temp();

    void on_value_state_change(const yami::parameters& params);
    void write_value_state(yami::parameters& params);
private:
};

}