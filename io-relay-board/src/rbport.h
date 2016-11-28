#ifndef RBPORT_H
#define	RBPORT_H

#include "timer.h"
#include <string>

namespace home_system
{

class service;

namespace input_output
{

class iorb_service;

namespace rb
{

class port
{
public:
  port(const std::string& port, iorb_service* service);
  ~port();

  void enable_relay(unsigned int relay);
  void disable_relay(unsigned int relay);
  void disable_all();
  int get_relay_state(unsigned int relay);
  
  

private:
  std::string port_;
  int state_[8];
  int wanted_state_[8];
  iorb_service* service_;
  
  ios_wrapper ios_;
  boost::asio::serial_port serial_port_;
  home_system::timer timer_, write_timer_;
  
  char buf_[10];
  void read_handler(const boost::system::error_code& error,
    std::size_t bytes_transferred);
  void write_handler(const boost::system::error_code& error,
    std::size_t bytes_transferred);
  
  void open_port();
  void setup_read();
  void check_state(int bitmap);
  void exec_state_change();
  void close_port();
};

}
}
}


#endif

