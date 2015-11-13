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
  HANDLERS.add(client);
}

}
