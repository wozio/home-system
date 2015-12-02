#include "cloud_client.h"
#include "logger.h"
#include "handler.h"

using namespace Poco;
using namespace std;

namespace home_system
{

cloud_client::cloud_client(ws_t ws, std::function<void()>shutdown_callback)
: handler(ws),
  on_shutdown_(shutdown_callback)
{
  LOG("Connected to cloud server, logging in");
}

cloud_client::~cloud_client()
{
}

void cloud_client::on_read(data_t data, size_t data_size)
{
}

void cloud_client::shutdown()
{
  LOG("Cloud client shutting down");
  handler::shutdown();
  on_shutdown_();
}

}
