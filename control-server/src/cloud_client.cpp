#include "cloud_client.h"
#include "logger.h"
#include "app.h"
#include "rapidjson/document.h"
#include "rapidjson/error/en.h"
#include <boost/interprocess/streams/bufferstream.hpp>

using namespace rapidjson;
using namespace std;

namespace home_system
{

cloud_client::cloud_client(ws_t ws, std::function<void()>shutdown_callback)
: client(ws),
  on_shutdown_(shutdown_callback)
{
  LOG("Connected to cloud server, logging in and sending allowed users");

  // fetching cloud system name, password and list of allowed users from configuration
  // and encoding to JSON
  auto cloud_name = home_system::app::config().get<std::string>("cloud.name");
  auto cloud_password = home_system::app::config().get<std::string>("cloud.password");

  size_t data_size = 0;
  auto data = create_data();
  boost::interprocess::bufferstream out(data->data(), DATA_SIZE);
  out << "{"
      << "\"system\":\"" << cloud_name << "\""
      << ",\"password\":\"" << cloud_password << "\""
      << ",\"users\": [";

  // filling array of users
  bool first = true;
  for (auto& v : home_system::app::config().get_child("users"))
  {
    if (!first)
    {
      out << ",";
    }
    first = false;
    auto email = v.second.get<std::string>("email");
    out << "\"" << email << "\"";
  }

  out << "]}";

  data_size = out.tellp();

  // sending to cloud server
  ws->sendFrame(data->data(), data_size);

  // receive response
  int flags;
  data_size = ws->receiveFrame(data->data(), DATA_SIZE, flags);

  if (data_size == 0)
  {
    throw std::runtime_error("Peer has shut down or closed connection");
  }

  (*data)[data_size] = '\0';

  // decode JSON
  rapidjson::Document d;
  d.Parse(data->data());
  if (d.HasParseError())
  {
    LOG("Parse error: " << d.GetErrorOffset() << ": " << rapidjson::GetParseError_En(d.GetParseError()));
    throw std::runtime_error("JSON parse error");
  }

  // checking result
  if (d.IsObject())
  {
    auto itr = d.FindMember("result");
    if (itr != d.MemberEnd())
    {
      if (string(itr->value.GetString()) == "success")
      {
        LOG("Successfully logged in");
        return;
      }
    }
  }

  throw std::runtime_error("Unsuccessful result or incorrect reply");
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
