#pragma once

#include "io.h"

class io_remote
    : public io
{
  public:
    io_remote(home_system::io::io_data_type_t data_type,
       const std::string &type,
       home_system::io::io_mode_t mode,
       const std::string &name,
       const std::string &service,
       home_system::io::io_id_t id);

    ~io_remote();

    // in params 'value' field in proper type must be present
    void extract_value_state(const yami::parameters &params);

  private:
    const std::string service_;
    const home_system::io::io_id_t id_;
};
