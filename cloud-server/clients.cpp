#include "clients.h"
#include "logger.h"

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
  LOG(DEBUG) << "Client add";
  client_ = client;
}

client_t clients::get()
{
  return client_;
}

void clients::remove(client_t client)
{
  LOG(DEBUG) << "Client remove";
  client_.reset();
}

}
