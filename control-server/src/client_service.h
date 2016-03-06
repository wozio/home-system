#ifndef CLIENT_SERVICE_H
#define CLIENT_SERVICE_H

#include "service.h"
#include "handler_t.h"
#include "msg_type_t.h"

namespace home_system
{

class client_service
: public service
{
public:
  client_service(const std::string& name, handler_t handler);

  void on_msg(yami::incoming_message & im);
  void on_remote_msg(const std::string& source, const std::string& target,
    msg_type_t msg_type, const std::string& msg,
    int sequence_number, const yami::parameters& params);

private:
  std::string name_;
  handler_t handler_;
};

}

#endif  /* CLIENT_SERVICE_H */
