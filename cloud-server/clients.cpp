#include "clients.h"
#include "handlers.h"

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