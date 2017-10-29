#pragma once

#include "io/device_types.h"
#include "utils/logger.h"
#include <yami4-cpp/parameters.h>
#include <boost/signals2.hpp>

class io;

typedef std::shared_ptr<io> io_t;

class io
    : public std::enable_shared_from_this<io>
{
  public:
    io(
        home_system::io::io_data_type_t data_type,
        const std::string &type,
        home_system::io::io_mode_t mode,
        const std::string &name);

    virtual ~io();

    const std::string& get_name();

    const home_system::io::io_data_type_t get_data_type();

    home_system::io::io_state_t get_state();

    void set_state(home_system::io::io_state_t s);

    boost::any get_value();
    void set_wanted_value(boost::any v);

    // 'value' field in proper type will be written into params
    void write_value_state(yami::parameters &params);

    boost::signals2::signal<void (io_t)> on_state_change;
    boost::signals2::signal<void (io_t)> on_value_change;

protected:

    template <typename T>
    void check_value(T& nv)
    {
        T ov = boost::any_cast<T>(value_);
        if (nv != ov)
        {
            LOG(DEBUG) << "IO \"" << name_ << "\" value changed to: " << nv;
            value_ = nv;
            if (state_ == home_system::io::io_state_t::ok)
            {
                on_value_change(shared_from_this());
            }
        }
    }
    void check_value(boost::any nv)
    {
        switch (data_type_)
        {
            case home_system::io::io_data_type_t::integer:
            {
                auto v = boost::any_cast<long long>(nv);
                check_value(v);
                break;
            }
            case home_system::io::io_data_type_t::double_float:
            {
                auto v = boost::any_cast<double>(nv);
                check_value(v);
                break;
            }
        }
    }
    home_system::io::io_data_type_t data_type_;
  private:
    std::string name_;
    
    const std::string type_;
    home_system::io::io_mode_t mode_;
    
    home_system::io::io_state_t state_;
    boost::any value_;
    boost::any wanted_value_;

    
};
