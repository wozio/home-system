#pragma once

#include "utils/timer.h"
#include "owdevice.h"
#include "io/service.h"
#include <map>
#include <string>
#include <memory>
#include <functional>

class ownet
{
public:
  ownet();
  ownet(const ownet&) = delete;
  ~ownet();
  
private:
  home_system::io::service ioservice_;

  home_system::utils::timer timer_;
  home_system::utils::timer timer_proc_;
  
  int portnum_;
  void list();
  void process();

  typedef std::shared_ptr<owdevice> owdevice_t;
  
  std::map<uint64_t, owdevice_t> devices_;
};
