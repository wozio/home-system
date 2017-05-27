#include "io_in_float.h"

io_in_float::io_in_float(const std::string &type, const std::string &name, const std::string &service, long long id)
    : io(type, name, service, id),
      value_(0.0)
{
}

io_in_float::~io_in_float()
{
}

void io_in_float::on_value_state_change(const yami::parameters &params)
{
    value_ = params.get_double_float("value");
    io::on_value_state_change(params);
}

void io_in_float::write_value_state(yami::parameters &params)
{
    params.set_double_float("value", value_);
    io::write_value_state(params);
}
