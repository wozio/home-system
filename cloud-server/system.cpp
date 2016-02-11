#include "system.h"
#include "logger.h"
#include "clients.h"
#include "systems.h"
#include "rapidjson/document.h"
#include "rapidjson/error/en.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

using namespace std;
using namespace rapidjson;

namespace home_system
{

system::system(ws_t ws)
: handler(ws)
{
  LOG(DEBUG) << "New system connected";

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
    LOG(DEBUG) << "Parse error: " << d.GetErrorOffset() << ": " << rapidjson::GetParseError_En(d.GetParseError());
    throw std::runtime_error("JSON parse error");
  }

  if (!d.IsObject())
  {
    LOG(DEBUG) << "Incorrect message, root element has to be Object";
    throw std::runtime_error("Incorrect message, root element has to be Object");
  }

  auto itr = d.FindMember("system");
  if (itr != d.MemberEnd())
  {
    if (itr->value.IsString())
    {
      string n(itr->value.GetString());
      if (n.size() > 0)
      {
        LOG(DEBUG) << "System name: " << n;
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
          LOG(DEBUG) << "Allowed user: " << vitr->GetString();
          // adding client to client<->system map
          CLIENTS.put(vitr->GetString(), name_);
          break;
        default:
          break;
        }
      }
    }
  }

  // sending result to system
  Document reply(kObjectType);
  reply.AddMember("result", "success", reply.GetAllocator());
  StringBuffer buffer;
  Writer<StringBuffer> writer(buffer);
  reply.Accept(writer);

  ws->sendFrame(buffer.GetString(), buffer.GetSize());
}

system::~system()
{
}

void system::on_read(data_t data, size_t data_size)
{
  // unpack message
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

  string target;
  auto itr = d.FindMember("target");
  if (itr != d.MemberEnd())
  {
    if (itr->value.IsString())
    {
      target = itr->value.GetString();
    }
  }
  if (target.size() == 0)
  {
    throw std::runtime_error("No target in message.");
  }
  // find target client
  auto client = CLIENTS.get(target);
  if (client)
  {
    // send data to client
    on_send(client, data, data_size);
  }
}

void system::shutdown()
{
  LOG(DEBUG) << "System shutdown";
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<system>
  SYSTEMS.remove(dynamic_pointer_cast<system>(shared_from_this()));
  handler::shutdown();
}

}
