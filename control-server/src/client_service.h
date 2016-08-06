#ifndef CLIENT_SERVICE_H
#define CLIENT_SERVICE_H

#include "service.h"
#include "handler_t.h"
#include "msg_type_t.h"
#include "ios_wrapper.h"
#include <yami4-cpp/incoming_message.h>
#include <map>

namespace home_system
{

class client_service
: public service
{
public:
  client_service(const std::string& name, handler_t handler);
  ~client_service();
  
  void init();

  void on_msg(yami::incoming_message & im);
  void on_remote_msg(const std::string& source, const std::string& target,
    msg_type_t msg_type, const std::string& msg,
    int sequence_number, const yami::parameters& params);

private:
  std::string name_;
  handler_t handler_;
  
  typedef std::map<int, yami::incoming_message> incoming_map_t;
  incoming_map_t incoming_;
  
  ios_wrapper ios_;
  
  typedef std::unique_ptr<boost::asio::deadline_timer> dt_t;
  // keyed by the same key values as incoming_
  typedef std::map<int, dt_t> timers_map_t;
  timers_map_t timers_;
  
  time_t discovery_subscription_id_;
};

}

#endif  /* CLIENT_SERVICE_H */
