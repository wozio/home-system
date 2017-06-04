#pragma once

#include "io/device_types.h"
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
       long long id);

    ~io();

    // in params 'value' field in proper type must be present
    void on_value_state_change(const yami::parameters &params);

    // 'value' field in proper type will be written into params
    void write_value_state(yami::parameters &params);

    // called every time when value is changed
    boost::signals2::signal<void ()> on_value_change;

  private:
    home_system::io::io_data_type_t data_type_;
    const std::string type_;
    home_system::io::io_mode_t mode_;
    const std::string name_;
    const std::string service_;
    const long long id_;
    home_system::io::io_state_t state_;
    boost::any value_;
};
