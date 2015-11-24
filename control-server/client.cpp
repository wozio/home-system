#include "client.h"
#include "logger.h"
#include "systems.h"
#include "clients.h"
#include "handler.h"

using namespace Poco;
using namespace std;

namespace home_system
{

client::client(ws_t ws)
: handler(ws)
{
  LOG("New client connected, performing client logging in");
  
  // any exception thrown from logging in will lead to deleting client
  //auto data = create_data();
  //int n = read(data);
  // for now just echo the message
  //send(data, n);
  
  init();
}

client::~client()
{
}

void client::on_read(data_t data, size_t data_size)
{
  // get system
  auto handler = SYSTEMS.get();
  if (handler)
  {
    // send data to system
    on_send(handler, data, data_size);
  }
}

void client::shutdown()
{
  LOG("Client shutting down");
  handler::shutdown();
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<client>
  CLIENTS.remove(dynamic_pointer_cast<client>(shared_from_this()));
}

std::string client::name()
{
  return string("client");
}


}