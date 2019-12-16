#pragma once

#include "io/device.h"
#include "utils/timer.h"
#include "schedule.h"

template <typename IO_T, typename DATA_T>
class interval_schedule
: public schedule
{
public:
  interval_schedule(IO_T io, int interval, DATA_T value_1, DATA_T value_2)
  : io_(io)
  {
    interval_ = interval;
    value_1_ = value_1;
    value_2_ = value_2;
    current_value_ = value_2_;
  }
  
  void kickoff()
  {
    // this will write first value
    on_timer();
    // when state changes to OK, IO sends current value
    io_->set_state(home_system::io::io_state_t::ok);
  }
private:
  IO_T io_;
  home_system::utils::timer timer_;
  int interval_;
  DATA_T value_1_, value_2_, current_value_;
  
  void on_timer()
  {
    timer_.set_from_now(interval_, [this] (){
      on_timer();
    });

    current_value_ == value_1_ ? current_value_ = value_2_ : current_value_ = value_1_;
    io_->set_value(current_value_);
  }
};
