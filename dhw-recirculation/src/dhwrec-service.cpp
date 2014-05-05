#include "dhwrec-service.h"
#include "logger.h"
#include "yamicontainer.h"
#include "discovery.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread.hpp>
#include <boost/bind.hpp>
#include <iostream>

using namespace boost;
using namespace boost::asio;
using namespace boost::posix_time;
using namespace std;

namespace home_system
{
namespace comfort
{

timeout::timeout(int dow, boost::posix_time::time_duration tod, bool s)
: day_of_week_(dow),
  time_of_day_(tod),
  state_(s)
{
}

timeout::timeout(const boost::posix_time::ptime& t)
: day_of_week_(t.date().day_of_week().as_number()),
  time_of_day_(t.time_of_day())
{
}

dhwrec_service::dhwrec_service(const std::string& output_service, int output)
: service("dhwrec"),
  output_service_(output_service),
  output_(output),
  output_state_(-1),
  dt_(ios_.io_service()),
  interval_dt_(ios_.io_service()),
  running_(false)
{
  LOGINFO("Output service: " << output_service << " output: " << output);
  
  DISCOVERY.subscribe(this);
}

dhwrec_service::~dhwrec_service()
{
  DISCOVERY.unsubscribe(this);
  stop_timers();
}

void dhwrec_service::on_msg(yami::incoming_message& im)
{
  if (im.get_message_name() == "set_state")
  {
    if (output_state_ >= -1)
    {
      if (im.get_parameters().get_boolean("state"))
      {
        start();
      }
      else
      {
        stop();
      }
    }
    
    im.reply();
  }
  else if (im.get_message_name() == "get_state")
  {
    yami::parameters param;
    param.set_boolean("state", running_);
    im.reply(param);
  }
  else if (im.get_message_name() == "output_state_change")
  {
    int new_state = im.get_parameters().get_integer("state");
    LOG("Output state: " << output_state_ << "->" << new_state);
    if (new_state != output_state_)
    {
      if (output_state_ == -1)
      {
        start_timers();
      }
      else if (new_state == -1)
      {
        stop_timers();
      }
      output_state_ = new_state;
    }
  }
  else
  {
    service::on_msg(im);
  }
}

void dhwrec_service::on_remote_service_availability(const std::string& name,
  bool available)
{
  if (name == output_service_)
  {
    if (available)
    {
      try
      {
        string ye = DISCOVERY.get(name);
        LOG("IO service available: name=" << name << " ye=" << ye);

        // subscribe for output state change notifications
        yami::parameters params;
        params.set_integer("output", output_);
        params.set_string("name", name_);
        AGENT.send(ye, name, "subscribe_output_state_change", params);
      }
      catch (...)
      {
      }
    }
    else
    {
      LOGWARN("IO service not available");
      stop_timers();
      output_state_ = -1;
    }
  }
}

void dhwrec_service::start_timers()
{
  LOG("Start timers");
  for (int i = 0; i < 7; i++)
  {
    // starts for each day of week at 6
    tots_.insert(timeout(i, hours(6), true));
    // ends for each day of week at 23
    tots_.insert(timeout(i, hours(23), false));
  }
  
  for (int i = 1; i < 6; i++)
  {
    // starts for week days at 16
    tots_.insert(timeout(i, hours(16), true));
    // ends for week days at 7
    tots_.insert(timeout(i, hours(7), false));
  }
  
  // finding next timeout
  ptime ct = microsec_clock::local_time();
  timeout ctot(ct);
  for (set<timeout>::iterator i = tots_.begin(); i != tots_.end(); ++i)
  {
    if ((*i) > ctot)
    {
      tot_ = i;
      break;
    }
  }
  if (tot_ == tots_.end())
  {
    tot_ = tots_.begin();
  }
  
  set<timeout>::iterator prevtot = tot_;
  if (prevtot == tots_.begin())
    prevtot = tots_.end();
  prevtot--;
  
  LOG("Previous timeout = " << (*prevtot).day_of_week_ << "," << (*prevtot).time_of_day_ << "," << (*prevtot).state_);
  LOG("Next timeout     = " << (*tot_).day_of_week_ << "," << (*tot_).time_of_day_ << "," << (*tot_).state_);
  
  if ((*prevtot).state_)
    start();
  else
    stop();

  continue_ = true;  
  dt_.expires_from_now(milliseconds(0));
  dt_.async_wait([&] (const boost::system::error_code& error) { on_timeout(error); } );
}

void dhwrec_service::stop_timers()
{
  LOG("Stop timers");
  continue_ = false;
  continue_interval_ = false;
  dt_.cancel();
  interval_dt_.cancel();
  
  tots_.clear();
  tot_ = tots_.end();
}

void dhwrec_service::on_timeout(const boost::system::error_code& error)
{
  if (error != error::operation_aborted)
  {
    if (continue_)
    {
      ptime ct = microsec_clock::local_time();
      if (tot_ != tots_.end())
      {
        if ((*tot_).is_timed_out(ct))
        {
          LOG("Timeout");
          if ((*tot_).state_)
            start();
          else
            stop();
          tot_++;
          if (tot_ == tots_.end())
            tot_ = tots_.begin();

          LOG("tot_    = " << (*tot_).day_of_week_ << "," << (*tot_).time_of_day_ << "," << (*tot_).state_);
        }
      }
      dt_.expires_at(dt_.expires_at() + seconds(1));
      dt_.async_wait(boost::bind(&dhwrec_service::on_timeout, this,
        asio::placeholders::error));
    }
  }
}

void dhwrec_service::on_int_timeout(const boost::system::error_code& error)
{
  if (error != error::operation_aborted)
  {
    if (continue_ && continue_interval_)
    {
      LOG("Interval timeout");
      if (output_state_)
        interval_dt_.expires_at(interval_dt_.expires_at() + minutes(2));
      else
        interval_dt_.expires_at(interval_dt_.expires_at() + minutes(1));
      interval_dt_.async_wait(boost::bind(&dhwrec_service::on_int_timeout, this,
        asio::placeholders::error));
      send_to_output(output_state_ ? false : true);
    }
  }
}

void dhwrec_service::start()
{
  LOGINFO("Starting circulation");
  continue_interval_ = true;
  running_ = true;
  interval_dt_.cancel();
  interval_dt_.expires_from_now(minutes(5));
  interval_dt_.async_wait(boost::bind(&dhwrec_service::on_int_timeout, this,
    asio::placeholders::error));
  send_to_output(true);
}

void dhwrec_service::stop()
{
  LOGINFO("Stopping circulation");
  running_ = false;
  continue_interval_ = false;
  interval_dt_.cancel();
  send_to_output(false);
}

void dhwrec_service::send_to_output(bool state)
{
  try
  {
    string ye = DISCOVERY.get(output_service_);
    yami::parameters params;
    params.set_integer("output", output_);
    params.set_integer("state", (int)state);
    AGENT.send(ye, output_service_, "set_output_state", params);
  }
  catch (const service_not_found&)
  {
    LOGWARN("Exception: service_not_found");
    stop_timers();
  }
  catch (const yami::yami_runtime_error&)
  {
    LOGWARN("Exception: yami_runtime_error");
    stop_timers();
  }
  catch (...)
  {
    LOGWARN("Unknown exception!");
    stop_timers();
  }
}

}
}
