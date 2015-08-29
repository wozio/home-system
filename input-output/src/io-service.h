#ifndef IO_SERVICE_H
#define	IO_SERVICE_H

#include "service.h"
#include "ownetwork.h"
#include <mutex>

namespace home_system
{
namespace input_output
{

class io_service
: public home_system::service
{
public:
  io_service();
  ~io_service();
  void on_msg(yami::incoming_message & im);
private:
  ow::net net_;
  
  std::mutex subscription_mutex_;
  // key = id
  struct rs
  {
    std::string ye_;
    std::string name_;
  };
  std::multimap<uint64_t, rs> state_subscriptions_;
  void on_state_change(uint64_t id);
};

}
}

#endif	/* IO_SERVICE_H */

