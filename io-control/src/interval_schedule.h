#pragma once

#include "io/device.h"
#include "utils/timer.h"
#include "schedule.h"
#include <vector>

template <typename IO_T, typename DATA_T>
class interval_schedule
: public schedule
{
public:
  interval_schedule(IO_T io, int interval)
  : io_(io),
    schedule(io),
    interval_(interval),
    next_value_index_(0)
  {
  }

  void add_value(DATA_T value)
  {
    values_.push_back(value);
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
  int next_value_index_;
  std::vector<DATA_T> values_;
  
  void on_timer()
  {
    if (values_.size() > 0)
    {
      timer_.set_from_now(interval_, [this] (){
        on_timer();
      });

      io_->set_value(values_[next_value_index_]);

      if (next_value_index_ == values_.size() - 1)
      {
        next_value_index_ = 0;
      }
      else
      {
        next_value_index_++;
      }
      
    }
  }
};
