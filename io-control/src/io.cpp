#include "io.h"
#include "utils/logger.h"

using namespace home_system::io;

io::io(home_system::io::io_data_type_t data_type,
    const std::string &type,
    home_system::io::io_mode_t mode,
    const std::string &name,
    const std::string &service,
    home_system::io::io_id_t id)
    : data_type_(data_type),
      type_(type),
      name_(name),
      service_(service),
      id_(id),
      state_(io_state_t::unknown)
{
}

io::~io()
{
}

void io::on_value_state_change(const yami::parameters &params)
{
    state_ = static_cast<io_state_t>(params.get_long_long("state"));

    switch (data_type_)
    {
        case home_system::io::io_data_type_t::integer:
        {
            auto nv = params.get_long_long("value");
            check_value(nv);
            break;
        }
        case home_system::io::io_data_type_t::double_float:
        {
            auto nv = params.get_double_float("value");
            check_value(nv);
            break;
        }
    }
}

void io::write_value_state(yami::parameters &params)
{
    params.set_long_long("state", static_cast<long long>(state_));
}
