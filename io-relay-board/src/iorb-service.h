#ifndef IORB_SERVICE_H
#define	IORB_SERVICE_H

#include "rbport.h"
#include "service.h"
#include <map>

namespace home_system
{
namespace input_output
{

namespace rb
{
class port;
}

class iorb_service
: public home_system::service
{
public:
  iorb_service(const std::string& name, const std::string& port);
  ~iorb_service();
  void on_msg(yami::incoming_message & im);
  
  void on_output_state_change(int output, int state);
private:
  std::mutex subscription_mutex_;
  rb::port port_;
  
  // key = output id and yamie endpoint of remote services subscribing
  struct rs
  {
    std::string ye_;
    std::string name_;
  };
  std::multimap<int, rs> state_subscriptions_;
};

}
}

#endif	/* IORB_SERVICE_H */

