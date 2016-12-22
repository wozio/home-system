#include "pch.h"
#include "clients.h"

namespace home_system
{
  clients::clients()
  {
    
  }
  
  clients::~clients()
  {
    
  }
  
  std::string clients::add(const std::string& name, handler_t handler)
  {
    std::string client_name;
    do
    {
      client_name = name + std::to_string(rand());
    } while (clients_.find(client_name) != clients_.end());

    clients_[client_name] = std::make_shared<client_service>(client_name, handler);

    return client_name;
  }

  void clients::remove(const std::string& client)
  {
    clients_.erase(client);
  }

  client_service_t clients::get(const std::string& client)
  {
    return clients_.at(client);
  }
}
