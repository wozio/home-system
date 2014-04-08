#ifndef RBPORT_H
#define	RBPORT_H

#include <boost/asio.hpp>
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
  
  void operator()();

private:
  std::string port_;
  int state_[8];
  int wanted_state_[8];
  bool opened_, write_timer_running_;
  iorb_service* service_;
  
  boost::asio::serial_port* serial_port_;
  boost::asio::deadline_timer* write_timer_;
  boost::asio::deadline_timer* read_timer_;
  
  char buf_[10];
  void read_handler(const boost::system::error_code& error,
    std::size_t bytes_transferred);
  void read_timeout_handler(const boost::system::error_code& error);
  void write_handler(const boost::system::error_code& error,
    std::size_t bytes_transferred);
  void write_timeout_handler(const boost::system::error_code& error);
  
  void setup_read();
  void check_state(int bitmap);
  void exec_state_change();
  void close_port();
};

}
}
}


#endif

