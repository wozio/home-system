#ifndef CLIENTS_H
#define	CLIENTS_H

#include "handler_t.h"
#include "client_service.h"
#include <map>
#include <string>

namespace home_system
{
  typedef std::shared_ptr<client_service> client_service_t;

  class clients
  {
  public:
    std::string add(const std::string& name, handler_t handler);
    void remove(const std::string& client);
    client_service_t get(const std::string& client);

  private:
    std::map<std::string, client_service_t> clients_;
  };
}

#endif	/* CLIENT_H */
