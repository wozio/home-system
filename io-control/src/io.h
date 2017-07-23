#pragma once

#include "io_base.h"
#include "io/device_types.h"
#include "utils/logger.h"
#include <yami4-cpp/parameters.h>

class io;

typedef std::shared_ptr<io> io_t;

class io
    : public std::enable_shared_from_this<io>,
      public io_base
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

    const home_system::io::io_data_type_t get_data_type();

    home_system::io::io_state_t get_state();
    void set_state(home_system::io::io_state_t s);

    boost::any get_value();

    template<typename T>
    void set_wanted_value(T v)
    {
        wanted_value_ = v;
    }

    // in params 'value' field in proper type must be present
    void extract_value_state(const yami::parameters &params);

    // 'value' field in proper type will be written into params
    void write_value_state(yami::parameters &params);

    

  private:
    home_system::io::io_data_type_t data_type_;
    const std::string type_;
    home_system::io::io_mode_t mode_;
    
    const std::string service_;
    const home_system::io::io_id_t id_;
    home_system::io::io_state_t state_;
    boost::any value_;
    boost::any wanted_value_;

    template <typename T>
    void check_value(T& nv)
    {
        T ov = boost::any_cast<T>(value_);
        if (nv != ov)
        {
            LOG(DEBUG) << "IO \"" << name_ << "\" value changed to: " << nv;
            value_ = nv;
        }
    }
};
