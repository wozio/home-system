#ifndef IO_SERVICE_H
#define	IO_SERVICE_H

#include "service.h"
#include "ownetwork.h"
#include <mutex>
#include <set>

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

  // yamie endpoint and name
  std::set<std::pair<std::string, std::string>> subscriptions_;
  void on_state_change(uint64_t id);
  void send_current_state();
};

}
}

#endif	/* IO_SERVICE_H */

