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
  LOG("New system connected");
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
  LOG("System shutdown");
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<system>
  SYSTEMS.remove(dynamic_pointer_cast<system>(shared_from_this()));
  handler::shutdown();
}

}