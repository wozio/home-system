#ifndef CLIENTS_H
#define	CLIENTS_H

#include "client.h"
#include <memory>

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
private:
  clients();

};

extern clients_t _clients;

#define CLIENTS (*::_clients)

#endif	/* CLIENTS_H */

