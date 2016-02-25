#include "client.h"
#include "logger.h"
#include "systems.h"
#include "clients.h"
#include "handler.h"
#include "rapidjson/document.h"
#include "rapidjson/error/en.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace Poco;
using namespace std;

namespace home_system
{

client::client(ws_t ws)
: handler(ws),
  state_(not_connected)
{
  LOG(DEBUG) << "New client connected";
  
  // wait for login message
  auto data = create_data();
  size_t data_size = read_internal(data);

  // adding \0 character at the end for JSON parser
  // it is guaranteed that data size is bigger than data_size
  (*data)[data_size] = '\0';

  // decode JSON
  rapidjson::Document d;
  d.Parse(data->data());
  if (d.HasParseError())
  {
    LOG(DEBUG) << "Parse error: " << d.GetErrorOffset() << ": " << rapidjson::GetParseError_En(d.GetParseError());
    throw std::runtime_error("JSON parse error");
  }

  if (!d.IsObject())
  {
    LOG(DEBUG) << "Incorrect message, root element has to be Object";
    throw std::runtime_error("Incorrect message, root element has to be Object");
  }

  auto itr = d.FindMember("message");
  if (itr != d.MemberEnd())
  {
    if (itr->value.IsString())
    {
      string v(itr->value.GetString());
      if (v == "login")
      {
        LOG(DEBUG) << "Login message received";
      }
      else
        throw std::runtime_error("Not a login message");
    }
    else
      throw std::runtime_error("Wrong message field");
  }
  else
    throw std::runtime_error("No message field");
  
  itr = d.FindMember("user");
  if (itr != d.MemberEnd())
  {
    if (itr->value.IsString())
    {
      string v(itr->value.GetString());
      LOG(DEBUG) << "user in message: " << v;
      // get system associated with this user
      // it will throw an exception when user is not found in map
      // what means that system handling such user is not connected
      system_ = SYSTEMS.get(v);
    }
    else
      throw std::runtime_error("Wrong user field");
  }
  else
    throw std::runtime_error("No message field");
  
  // set up temporary system->client route
  system_->set_route("fafa", dynamic_pointer_cast<client>(shared_from_this()));
  
  // now wait for login reply
  
  // send login message towards system
}

client::~client()
{
}

void client::on_read(data_t data, size_t data_size)
{
}

void client::shutdown()
{
  LOG(DEBUG) << "Client shutdown";
  handler::shutdown();
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<client>
  CLIENTS.remove(dynamic_pointer_cast<client>(shared_from_this()));
}

}