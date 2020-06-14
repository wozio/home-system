#pragma once

#include "io/device.h"
#include "utils/timer.h"
#include "schedule.h"

template <typename IO_T, typename DATA_T>
class weekly_schedule
: public schedule
{
public:
  weekly_schedule(IO_T io)
  : io_(io),
    schedule(io)
  {
  }

  void add_trigger(int day, int hour, int minute, int second, DATA_T value)
  {
    int time = hour * 3600 + minute * 60 + second;
    triggers_[day * 24 * 3600 + time] = value;
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
  std::map<int, DATA_T> triggers_; // keyed by time of week in seconds
  
  void on_timer()
  {
    static int previous_time = -1;
    timer_.set_from_now(100, [this] (){
      on_timer();
    });

    if (triggers_.size() > 0)
    {
      // first find out what is the time now from begining of the week
      time_t rawtime;
      time(&rawtime);
      tm* ti = localtime(&rawtime);
      int ct = ti->tm_wday * 24 * 3600 + ti->tm_hour * 3600 + ti->tm_min * 60 + ti->tm_sec;

      auto t = triggers_.upper_bound(ct);
      if (t == triggers_.begin())
      {
        t = triggers_.end();
      }
      t--;
      if (ct != previous_time)
      {
        previous_time = ct;
        io_->set_value(t->second);
      }
    }
  }
};
