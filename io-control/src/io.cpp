#include "io.h"

using namespace std;
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

const std::string& io::get_name()
{
    return name_;
}

const home_system::io::io_data_type_t io::get_data_type()
{
    return data_type_;
}

home_system::io::io_state_t io::get_state()
{
    return state_;
}

void io::set_state(home_system::io::io_state_t s)
{
    if (state_ != s)
    {
        LOG(DEBUG) << "IO \"" << name_ << "\" state changed: " << io_state_to_string(state_) << " >> " << io_state_to_string(s);
        state_ = s;
    }
    on_value_state_change(shared_from_this());
}

boost::any io::get_value()
{
    return value_;
}

void io::extract_value_state(const yami::parameters &params)
{
    // state extract
    auto s = static_cast<io_state_t>(params.get_long_long("state"));
    
    // value extract
    if (s == io_state_t::ok)
    {
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

    set_state(s);
}

void io::write_value_state(yami::parameters &params)
{
    params.set_long_long("state", static_cast<long long>(state_));

    switch (data_type_)
    {
        case home_system::io::io_data_type_t::integer:
        {
            auto v = boost::any_cast<long long>(value_);
            params.set_long_long("value", v);
            break;
        }
        case home_system::io::io_data_type_t::double_float:
        {
            auto v = boost::any_cast<double>(value_);
            params.set_double_float("value", v);
            break;
        }
    }
}
