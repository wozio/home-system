#pragma once

#include "io/device.h"
#include "utils/timer.h"
#include "schedule.h"

template <typename IO_T, typename DATA_T>
class weekly_schedule
: public schedule
{
public:
  weekly_schedule(std::shared_ptr<IO_T> io, std::map<std::string, DATA_T> &triggers)
  : io_(io)
  {
    for (auto& trigger : triggers)
    {
      int hh, mm, ss, day;
      sscanf(trigger.first.c_str(), "%d:%d:%d:%d",
        &day, &hh, &mm, &ss);
      if (day < 0 || day > 6 || hh < 0 || hh > 23 || mm < 0 || mm > 59 || ss < 0 || ss > 59)
      {
        throw std::runtime_error("Incorrect time format");
      }
      int time = hh * 3600 + mm * 60 + ss;
      triggers_[day * 24 * 3600 + time] = trigger.second;
    }
  }
  void kickoff()
  {
    // this will write first value
    on_timer();
    // when state changes to OK, IO sends current value
    io_->set_state(home_system::io::io_state_t::ok);
  }
private:
  std::shared_ptr<IO_T> io_;
  home_system::utils::timer timer_;
  std::map<int, DATA_T> triggers_;
  
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
