#include "system.h"
#include "logger.h"
#include "clients.h"
#include "systems.h"
#include "rapidjson/document.h"
#include "rapidjson/error/en.h"
#include <boost/interprocess/streams/bufferstream.hpp>

using namespace std;

namespace home_system
{

system::system(ws_t ws)
: handler(ws)
{
  LOG("New system connected");

  size_t data_size = 0;
  auto data = create_data();
  int flags;
  data_size = ws->receiveFrame(data->data(), DATA_SIZE, flags);

  if (data_size == 0)
  {
    throw std::runtime_error("Peer has shut down or closed connection");
  }

  // adding \0 character at the end for JSON parser
  // it is guaranteed that data size is bigger than data_size
  (*data)[data_size] = '\0';

  // decode JSON
  rapidjson::Document d;
  d.Parse(data->data());
  if (d.HasParseError())
  {
    LOG("Parse error: " << d.GetErrorOffset() << ": " << rapidjson::GetParseError_En(d.GetParseError()));
    throw std::runtime_error("JSON parse error");
  }

  if (d.IsObject())
  {
    auto itr = d.FindMember("system");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsString())
      {
        string n(itr->value.GetString());
        if (n.size() > 0)
        {
          LOG("System name: " << itr->value.GetString());
        }
        else
          throw std::runtime_error("Empty system name");
      }
      else
        throw std::runtime_error("Wrong system name");
    }
    else
      throw std::runtime_error("No system name");
    itr = d.FindMember("users");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsArray())
      {
        for (auto vitr = itr->value.Begin(); vitr != itr->value.End(); ++vitr)
        {
          switch (vitr->GetType())
          {
          case rapidjson::kStringType:
            LOG(vitr->GetString());
            break;
          default:
            break;
          }
        }
      }
    }
  }

  boost::interprocess::bufferstream out(data->data(), DATA_SIZE);
  out << "{"
      << "\"result\":\"success\""
      << "}";

  data_size = out.tellp();

  ws->sendFrame(data->data(), data_size);
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
