#include "client.h"
#include "logger.h"
#include "ws_com_handler.h"
#include <thread>

using namespace std;

namespace home_system
{

client::client(ws_t ws)
: handler(ws)
{
  LOG("Client connected");
}

client::~client()
{
}

void client::on_read(data_t data, size_t data_size)
{
  //LOG("Read " << data_size << " bytes");
  try
  {
    thread t(handle_ws_data, data, data_size, shared_from_this());
    t.detach();
  }
  catch (...)
  {
  }
}

}
