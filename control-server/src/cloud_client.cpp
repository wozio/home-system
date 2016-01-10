#include "cloud_client.h"
#include "logger.h"

namespace home_system
{

cloud_client::cloud_client(ws_t ws, std::function<void()>shutdown_callback)
: client(ws),
  on_shutdown_(shutdown_callback)
{
  LOG("Connected to cloud server, logging in");
}

cloud_client::~cloud_client()
{
}

void cloud_client::shutdown()
{
  LOG("Cloud client shutting down");
  handler::shutdown();
  on_shutdown_();
}

}
