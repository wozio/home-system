#ifndef CLIENT_SERVICE_H
#define CLIENT_SERVICE_H

#include "service.h"

namespace home_system
{

class client_service
: public service
{
public:
  client_service(const std::string& name);

  void on_msg(yami::incoming_message & im);

private:
  std::string name_;
};

}

#endif  /* CLIENT_SERVICE_H */
