#include "clients.h"

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
}

client_t clients::get()
{
  return client_;
}

void clients::remove(client_t client)
{
  client_.reset();
}

}
