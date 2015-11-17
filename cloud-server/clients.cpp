#include "clients.h"
#include "handlers.h"

namespace home_system
{

clients::clients()
{
}

clients::~clients()
{
}

void clients::add(client_t client)
{
  client_ = client;
  HANDLERS.add(client);
}

client_t clients::get()
{
  return client_;
}

void clients::remove(client_t client)
{
  client_.reset();
  HANDLERS.remove(client);
}

}
