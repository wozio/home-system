#ifndef CLIENT_SERVICE_H
#define CLIENT_SERVICE_H

#include "service.h"
#include "handler_t.h"
#include "msg_type_t.h"
#include "timer.h"
#include "client_binary_session.h"

namespace home_system
{

class client_service
: public service
{
public:
  client_service(const std::string& name, handler_t handler);
  ~client_service();
  
  void init();
  void add_binary_connection(ws_t ws);

  void on_msg(yami::incoming_message & im);
  void on_remote_msg(const std::string& source, const std::string& target,
    msg_type_t msg_type, const std::string& msg,
    int sequence_number, yami::parameters& params);

private:
  std::string name_;
  handler_t handler_;
  handler_t binary_handler_;
  std::unique_ptr<client_binary_session> client_binary_session_;

  std::mutex incoming_map_mutex_;
  
  typedef std::map<int, yami::incoming_message> incoming_map_t;
  incoming_map_t incoming_;
  
  timer timer_;
  typedef std::map<int, int> incoming_timeouts_t;
  incoming_timeouts_t incoming_timeouts_;
  void set_timer();
  void on_timeout();
  
  time_t discovery_subscription_id_;
};

}

#endif  /* CLIENT_SERVICE_H */
