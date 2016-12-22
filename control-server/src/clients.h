#ifndef CLIENTS_H
#define	CLIENTS_H

#include "handler_t.h"
#include "client_service.h"

namespace home_system
{
  typedef std::shared_ptr<client_service> client_service_t;
  
  class clients;

  typedef std::unique_ptr<clients> clients_t;

  class clients
  {
  public:
    clients(const clients& orig) = delete;
    ~clients();
    
    static clients_t create()
    {
      return clients_t(new clients());
    };
    
    std::string add(const std::string& name, handler_t handler);
    void remove(const std::string& client);
    client_service_t get(const std::string& client);

  private:
    clients();
    
    std::map<std::string, client_service_t> clients_;
  };
}

extern home_system::clients_t _clients;

#define CLIENTS (*::_clients)

#endif	/* CLIENT_H */
