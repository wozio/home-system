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
  service_(service),
  serial_port_(ios_.io_service()),
  timer_(ios_),
  write_timer_(ios_)
{
  for (size_t i = 0; i < 8; ++i)
  {
    state_[i] = -1;
    wanted_state_[i] = 0;
  }
  
  open_port();
}

class port_error
: public std::exception
{
};

void port::open_port()
{
  static bool logged = false;
  if (!logged)
  {
    LOGINFO("Opening COM port " << port_);
  }

  try
  {
    serial_port_.open(port_);
    serial_port_.set_option(serial_port::baud_rate(2400));
    LOGINFO("Opened COM port " << port_);
    logged = false;
    setup_read();
  }
  catch (const boost::system::system_error& e)
  {
    if (!logged)
    {
      LOGWARN("Unable to open COM port " << port_ << ": " << e.what() <<
        ", keep trying...");
      logged = true;
    }
    timer_.set_from_now(1000, [this] () { open_port(); });
  }
}

void port::setup_read()
{
  serial_port_.async_read_some(buffer(buf_, 10),
    [&] (const boost::system::error_code& error, std::size_t bytes_transferred) { read_handler(error, bytes_transferred); } );
    
  timer_.cancel();
  timer_.set_from_now(2000, [this] () {
    LOGWARN("Timeout on port read");
    close_port();
    open_port();
  });
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
    open_port();
  }
}

void port::write_handler(const boost::system::error_code& error,
  std::size_t bytes_transferred)
{
  if (!error)
  {
    write_timer_.set_from_now(1000, [this] (){ exec_state_change(); });
  }
  else if (error != error::operation_aborted)
  {
    LOGWARN("Error on port write");
    close_port();
    open_port();
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
  write_timer_.cancel();
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
        serial_port_.async_write_some(buffer(&buf, 1),
          [&] (const boost::system::error_code& error, std::size_t bytes_transferred) { write_handler(error, bytes_transferred); } );
        break;
      }
    }
  }
  catch (const boost::system::system_error& e)
  {
    LOGWARN("Unable to write to COM port " << port_ << ": " << e.what());
    close_port();
    open_port();
  }
}

void port::close_port()
{
  if (serial_port_.is_open())
  {
    LOGINFO("Closing COM port: " << port_);
    serial_port_.cancel();
    serial_port_.close();
  }
  timer_.cancel();
  for (size_t i = 0; i < 8; ++i)
  {
    if (state_[i] != -1)
      service_->on_output_state_change(i, -1);
    state_[i] = -1;
  }
}

port::~port()
{
  if (serial_port_.is_open())
  {
    LOGINFO("Closing COM port: " << port_);
    serial_port_.cancel();
    serial_port_.close();
  }
  timer_.cancel();
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

