#pragma once

#include "io/device.h"

class relay : public home_system::io::device
{
  public:
    relay(home_system::io::io_id_t id);
    ~relay();

    void set_value(int value);

    int get_value();
    int get_wanted_value();

  private:
};
