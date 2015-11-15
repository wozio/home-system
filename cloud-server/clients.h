#ifndef CLIENTS_H
#define	CLIENTS_H

#include "client.h"
#include <memory>

namespace home_system
{

class clients;
class client;

typedef std::unique_ptr<clients> clients_t;
typedef std::shared_ptr<client> client_t;

class clients {
public:
  clients(const clients& orig) = delete;
  ~clients();
  
  static clients_t create()
  {
    return clients_t(new clients());
  };
  
  void add(client_t client);
  client_t get();
  void remove(client_t client);
  
private:
  clients();
  
  client_t client_;

};

}

extern home_system::clients_t _clients;

#define CLIENTS (*::_clients)

#endif	/* CLIENTS_H */

