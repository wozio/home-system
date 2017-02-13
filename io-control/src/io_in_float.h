#pragma once
#include "io.h"

namespace home_system
{
class io_in_float
    : public io
{
public:
    io_in_float(const std::string& type, const std::string& name, const std::string& service, long long id);
    ~io_in_float();

    void on_value_state_change(const yami::parameters& params);
    void write_value_state(yami::parameters& params);
private:
    double value_;
};

}