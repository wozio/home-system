#include "rbport.h"
#include "iorb-service.h"
#include "logger.h"
#include <boost/date_time/posix_time/posix_time.hpp>
#include <thread>
#include <iostream>

using namespace std;
using namespace boost;
using namespace boost::posix_time;
using namespace boost::asio;

namespace home_system
{
namespace input_output
{
namespace rb
{

port::port(const std::string& port, iorb_service* service)
: port_(port),
  opened_(false),
  write_timer_running_(false),
  service_(service)
{
  for (size_t i = 0; i < 8; ++i)
  {
    state_[i] = -1;
    wanted_state_[i] = 0;
  }
  
  thread t([this]() { thread_exec(); });
}

class port_error
: public std::exception
{
};

void port::thread_exec()
{
  try
  {
    while (true)
    {
      for (size_t i = 0; i < 8; ++i)
      {
        state_[i] = -1;
      }
      
      LOGINFO("Opening COM port " << port_);
      
      io_service ios;
      serial_port port(ios);
      serial_port_ = &port;
      deadline_timer write_timer(ios);
      write_timer_ = &write_timer;
      deadline_timer read_timer(ios);
      read_timer_ = &read_timer;
      
      try
      {
        port.open(port_);
        port.set_option(serial_port::baud_rate(2400));
        opened_ = true;
      }
      catch (const boost::system::system_error& e)
      {
        LOGWARN("Unable to open COM port " << port_ << ": " << e.what() <<
          ", keep trying...");
        this_thread::sleep_for(std::chrono::seconds(1));
      }
      
      while (!opened_)
      {
        try
        {
          port.open(port_);
          port.set_option(serial_port::baud_rate(2400));
          opened_ = true;
        }
        catch (const boost::system::system_error&)
        {
          this_thread::sleep_for(std::chrono::seconds(1));
        }
      }
      
      LOGINFO("Opened COM port " << port_);
      
      try
      {
        setup_read();
        
        ios.run();
      }
      catch (const std::exception& e)
      {
        LOGWARN("EXCEPTION: " << e.what());
        
      }
    }
  }
  catch (...)
  {
  }
}

void port::setup_read()
{
  serial_port_->async_read_some(buffer(buf_, 10),
    [&] (const boost::system::error_code& error, std::size_t bytes_transferred) { read_handler(error, bytes_transferred); } );

  read_timer_->cancel();
  read_timer_->expires_from_now(seconds(2));
  read_timer_->async_wait([&] (const boost::system::error_code& error) { read_timeout_handler(error); } );
}

void port::read_handler(const boost::system::error_code& error,
  std::size_t bytes_transferred)
{
  if (!error)
  {
    static bool crfound = false;
    static vector<char> value; // between cr(0xA) and endl(0xD);
    for (size_t i = 0; i < bytes_transferred; ++i)
    {
      switch (buf_[i])
      {
        case 0xA:
          crfound = true;
          break;
        case 0xD:
          if (crfound)
          {
            crfound = false;
            value.push_back(0);
            int bitmap = atoi(&value[0]);
            value.clear();
            check_state(bitmap);
            exec_state_change();
          }
        default:
          if ((buf_[i] > 0x1F) && crfound)
            value.push_back(buf_[i]);
      }
    }
    
    setup_read();
  }
  else if (error != error::operation_aborted)
  {
    LOGWARN("Error on port read");
    close_port();
  }
}

void port::read_timeout_handler(const boost::system::error_code& error)
{
  if (!error)
  {
    LOGWARN("Timeout on port read");
    close_port();
  }
}

void port::write_handler(const boost::system::error_code& error,
  std::size_t bytes_transferred)
{
  if (!error)
  {
    exec_state_change();
  }
  else if (error != error::operation_aborted)
  {
    LOGWARN("Error on port write");
    close_port();
  }
}

void port::write_timeout_handler(const boost::system::error_code& error)
{
  if (!error)
  {
    write_timer_running_ = false;
    exec_state_change();
  }
}



void port::check_state(int bitmap)
{
  for (size_t i = 0; i < 8; ++i)
  {
    int state = bitmap & 1;
    if (state != state_[i])
    {
      LOG("Relay " << i << " state: " << state_[i] <<
        "->" << state);
      service_->on_output_state_change(i, state);
      state_[i] = state;
    }
    bitmap >>= 1;
  }
}

void port::exec_state_change()
{
  if (!write_timer_running_ && opened_)
  {
    try
    {
      for (size_t j = 0; j < 8; ++j)
      {
        if (state_[j] != -1 && state_[j] != wanted_state_[j])
        {
          unsigned char buf;
          if (wanted_state_[j])
            // enable relay number is encoded from 'a' ASCII code
            buf = 97;
          else
            // disable relay number is encoded from 'i' ASCII code
            buf = 105;
          buf += j;
          LOG("executing relay " << j << " state change: " <<
            state_[j] << "->" << wanted_state_[j]);
          serial_port_->async_write_some(buffer(&buf, 1),
            [&] (const boost::system::error_code& error, std::size_t bytes_transferred) { write_handler(error, bytes_transferred); } );

          write_timer_running_ = true;
          
          write_timer_->cancel();
          write_timer_->expires_from_now(seconds(2));
          write_timer_->async_wait([&] (const boost::system::error_code& error) { write_timeout_handler(error); } );
          break;
        }
      }
    }
    catch (const boost::system::system_error& e)
    {
      LOGWARN("Unable to write to COM port " << port_ << ": " << e.what());
      throw port_error();
    }
  }
}

void port::close_port()
{
  LOGINFO("Closing COM port: " << port_);
  opened_ = false;
  serial_port_->cancel();
  serial_port_->close();
  write_timer_running_ = false;
  write_timer_->cancel();
  read_timer_->cancel();
  for (size_t i = 0; i < 8; ++i)
  {
    if (state_[i] != -1)
      service_->on_output_state_change(i, -1);
    state_[i] = -1;
  }
}

port::~port()
{
}

void port::enable_relay(unsigned int relay)
{
  wanted_state_[relay] = 1;
  exec_state_change();
}

void port::disable_relay(unsigned int relay)
{
  wanted_state_[relay] = 0;
  exec_state_change();
}

void port::disable_all()
{
  for (int i = 0; i < 8; ++i)
    wanted_state_[i] = 0;
  exec_state_change();
}

int port::get_relay_state(unsigned int relay)
{
  if (relay < 8)
    return state_[relay];
  else
    return -1;
}

}
}
}

