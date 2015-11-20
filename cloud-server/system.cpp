#include "system.h"
#include "logger.h"
#include "clients.h"
#include "systems.h"

using namespace std;

namespace home_system
{

system::system(ws_t ws)
: handler(ws)
{
  LOG("New system connected, performing system logging in");
  
  // any exception thrown from logging in will lead to deleting system
  //auto data = create_data();
  //int n = read(data);
  // for now just echo the message
  //send(data, n);
  
  init();
}

system::~system()
{
}

void system::on_read(data_t data, size_t data_size)
{
  // unpack message to find client
  // get client
  auto handler = CLIENTS.get();
  if (handler)
  {
    // send data to client
    on_send(handler, data, data_size);
  }
}

void system::shutdown()
{
  LOG("System '" << name() << "' shutdown");
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<system>
  SYSTEMS.remove(dynamic_pointer_cast<system>(shared_from_this()));
  handler::shutdown();
}

std::string system::name()
{
  return string("system");
}

}