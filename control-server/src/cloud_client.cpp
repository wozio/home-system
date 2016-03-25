#include "cloud_client.h"
#include "logger.h"
#include "app.h"
#include "clients.h"
#include "rapidjson/document.h"
#include "rapidjson/error/en.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace rapidjson;
using namespace std;

namespace home_system
{

cloud_client::cloud_client(ws_t ws, std::function<void()>shutdown_callback)
: client(ws),
  on_shutdown_(shutdown_callback)
{
  LOG(DEBUG) << "Connected to cloud server, logging in and sending allowed users";

  // fetching cloud system name, password and list of allowed users from configuration
  // and encoding to JSON
  Document d(kObjectType);
  Document::AllocatorType& allocator = d.GetAllocator();

  auto cloud_name = home_system::app::config().get<std::string>("cloud.name");
  d.AddMember("system", StringRef(cloud_name.c_str()), allocator);

  auto cloud_password = home_system::app::config().get<std::string>("cloud.password");
  d.AddMember("password", StringRef(cloud_password.c_str()), allocator);

  // array of users
  Value users(kArrayType);
  for (auto& v : home_system::app::config().get_child("users"))
  {
    //auto user = v.second.get<std::string>("email");
    users.PushBack(Value(v.second.get<std::string>("email").c_str(), allocator).Move(),
        allocator);
  }
  d.AddMember("users", users, allocator);

  // stringify JSON
  StringBuffer buffer;
  Writer<StringBuffer> writer(buffer);
  d.Accept(writer);

  // sending to cloud server
  ws->sendFrame(buffer.GetString(), buffer.GetSize());

  // receive response
  size_t data_size = 0;
  auto data = create_data();
  int flags;
  data_size = ws->receiveFrame(data->data(), DATA_SIZE, flags);

  if (data_size == 0)
  {
    throw std::runtime_error("Peer has shut down or closed connection");
  }

  (*data)[data_size] = '\0';

  // decode JSON
  d.Parse(data->data());
  if (d.HasParseError())
  {
    LOG(DEBUG) << "Parse error: " << d.GetErrorOffset() << ": " << rapidjson::GetParseError_En(d.GetParseError());
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
        LOG(DEBUG) << "Successfully logged in";
        return;
      }
    }
  }

  throw std::runtime_error("Unsuccessful login result or incorrect reply");
}

cloud_client::~cloud_client()
{
}

void cloud_client::shutdown()
{
  LOG(DEBUG) << "Cloud client shutting down";
  for (const auto& client : clients_ids_)
  {
    CLIENTS.remove(client);
  }
  clients_ids_.clear();
  handler::shutdown();
  on_shutdown_();
}

bool cloud_client::is_logged_in(const std::string& client)
{
  return clients_ids_.find(client) != clients_ids_.end();
}

void cloud_client::login(const std::string& client)
{
  clients_ids_.insert(client);
}

void cloud_client::logout(const std::string& client)
{
  clients_ids_.erase(client);
  CLIENTS.remove(client);
}

}
