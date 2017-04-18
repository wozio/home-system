#pragma once

#include "timer.h"
#include "owdevice.h"
#include "io/io_service.h"
#include <map>
#include <string>
#include <memory>
#include <functional>

namespace home_system
{

class ownet
{
public:
  ownet(const std::string &port);
  ownet(const ownet&) = delete;
  ~ownet();
  
private:
  std::string port_;

  io_service ioservice_;

  timer timer_;
  
  bool opened_, open_fault_logged_, search_fault_logged_;
  int portnum_;
  void open();
  void search();
  void process();
  void close();

  typedef std::shared_ptr<owdevice> owdevice_t;
  
  std::map<uint64_t, owdevice_t> devices_;
};

}
