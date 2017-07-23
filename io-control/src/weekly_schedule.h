#pragma once

#include "utils/timer.h"

class weekly_schedule
{
public:
    weekly_schedule();
private:
    home_system::utils::timer timer_;
    void set_timer();
    void on_timer();
};
