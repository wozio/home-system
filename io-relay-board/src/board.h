#pragma once

#include "io/io_service.h"
#include "relay.h"
#include <string>

namespace home_system
{

typedef std::shared_ptr<relay> relay_t;

class board
{
public:
  board(const std::string& name, const std::string& port);
  ~board();

private:
  std::string port_;
  io_service ioservice_;
  std::vector<relay_t> relays_;
  
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
  void check_values(int bitmap);
  void exec_value_change();
  void close_port();
};

}

