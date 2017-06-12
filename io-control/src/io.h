#pragma once

#include "io/device_types.h"
#include "utils/logger.h"
#include <yami4-cpp/parameters.h>
#include <boost/any.hpp>
#include <boost/signals2.hpp>
#include <memory>

class io;

typedef std::shared_ptr<io> io_t;

class io
{
  public:
    io(home_system::io::io_data_type_t data_type,
       const std::string &type,
       home_system::io::io_mode_t mode,
       const std::string &name,
       const std::string &service,
       home_system::io::io_id_t id);

    ~io();

    const std::string& get_name();
    home_system::io::io_state_t get_state();
    void set_state(home_system::io::io_state_t s);

    // in params 'value' field in proper type must be present
    void on_value_state_change(const yami::parameters &params);

    // 'value' field in proper type will be written into params
    void write_value_state(yami::parameters &params);

    // called every time when value is changed
    boost::signals2::signal<void (home_system::io::io_id_t)> on_value_change;
    // called every time when state is changed
    boost::signals2::signal<void (home_system::io::io_id_t)> on_state_change;

  private:
    home_system::io::io_data_type_t data_type_;
    const std::string type_;
    home_system::io::io_mode_t mode_;
    const std::string name_;
    const std::string service_;
    const home_system::io::io_id_t id_;
    home_system::io::io_state_t state_;
    boost::any value_;

    template <typename T>
    void check_value(T& nv)
    {
        T ov = boost::any_cast<T>(value_);
        if (nv != ov)
        {
            LOG(DEBUG) << "IO \"" << name_ << "\" value changed to: " << nv;
            value_ = nv;
            on_value_change(id_);
        }
    }
};
