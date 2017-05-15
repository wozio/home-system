#pragma once

#include "io/device.h"
#include "utils/timer.h"

class relay : public home_system::io::device
{
  public:
    relay(home_system::io::io_id_t id, boost::asio::serial_port &serial_port);
    ~relay();
    
    void set_wanted_value(const boost::any &v);

    void check_value(int value);
    int get_value();
    int get_wanted_value();

  private:
};
