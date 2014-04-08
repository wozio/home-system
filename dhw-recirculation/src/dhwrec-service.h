#ifndef DHWREC_SERVICE_H
#define	DHWREC_SERVICE_H

#include "service.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <set>
#include <iostream>

namespace home_system
{
namespace comfort
{

struct timeout
{
  timeout(int dow, boost::posix_time::time_duration tod, bool s);
  timeout(const boost::posix_time::ptime& t);
  bool operator<(const timeout& a) const
  {
    if (day_of_week_ < a.day_of_week_)
      return true;
    else if (day_of_week_ == a.day_of_week_)
    {
      if (time_of_day_ < a.time_of_day_)
        return true;
    }
    return false;
  }
  bool operator>(const timeout& a) const
  {
    if (day_of_week_ > a.day_of_week_)
      return true;
    else if (day_of_week_ == a.day_of_week_)
    {
      if (time_of_day_ > a.time_of_day_)
        return true;
    }
    return false;
  }
  bool is_timed_out(const boost::posix_time::ptime& a) const
  {
    if (day_of_week_ == a.date().day_of_week().as_number())
    {
      if (time_of_day_ <= a.time_of_day())
      {
        return true;
      }
    }
    return false;
  }
  
  int day_of_week_;
  boost::posix_time::time_duration time_of_day_;
  bool state_; // state after timeout
};

class dhwrec_service
: public home_system::service
{
public:
  dhwrec_service(const std::string& output_service, int output);
  ~dhwrec_service();
  void on_msg(yami::incoming_message& im);
  void on_remote_service_availability(const std::string& name,
    bool available);
  
private:
  void start();
  void stop();
  
  void start_timers();
  void stop_timers();
  
  std::string output_service_;
  int output_, output_state_;
  
  boost::mutex state_mutex;
  
  boost::asio::deadline_timer dt_, interval_dt_;
  bool continue_, running_, continue_interval_;
  
  std::set<timeout> tots_;
  std::set<timeout>::iterator tot_;
  
  boost::posix_time::ptime last_rinse_;
  
  void on_timeout(const boost::system::error_code& error);
  void on_int_timeout(const boost::system::error_code& error);
  
  void send_to_output(bool state);
};

}
}

#endif	/* DHWREC_SERVICE_H */

