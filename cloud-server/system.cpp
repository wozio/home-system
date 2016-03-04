#include "system.h"
#include "logger.h"
#include "clients.h"
#include "systems.h"
#include "rapidjson/document.h"
#include "rapidjson/error/en.h"
#include "rapidjson/writer.h"

using namespace std;
using namespace rapidjson;

namespace home_system
{

system::system(ws_t ws)
: handler(ws),
  sys_state_(wait_for_login)
{
  LOG(DEBUG) << "New system connected";
}

system::~system()
{
}

void system::set_route(const std::string& target, client_t client)
{
  LOG(DEBUG) << "Set route: " << target;
  route_[target] = client;
  LOG(DEBUG) << "Number of routes: " << route_.size();
}

void system::unset_route(const std::string& target)
{
  LOG(DEBUG) << "Unset route: " << target;
  route_.erase(target);
  LOG(DEBUG) << "Number of routes: " << route_.size();
}

void system::on_read(data_t data, size_t data_size)
{
  switch (sys_state_)
  {
    case logged_in:
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
      auto c = route_.find(target);
      if (c != route_.end())
      {
        on_send(c->second, data, data_size);
      }
      else
      {
        LOG(WARNING) << "No route for target: " << target;
      }
      break;
    }
    case wait_for_login:
    {
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
            {
              std::string user(vitr->GetString());
              LOG(DEBUG) << "Allowed user: " << user;
              // adding user to allowed users map
              SYSTEMS.add(user, dynamic_pointer_cast<system>(shared_from_this()));
              break;
            }
            default:
              break;
            }
          }
        }
      }

      sys_state_ = logged_in;

      // sending result to system
      Document reply(kObjectType);
      reply.AddMember("result", "success", reply.GetAllocator());
      buffer_t buffer(new StringBuffer);
      Writer<StringBuffer> writer(*buffer);
      reply.Accept(writer);
      
      on_send(shared_from_this(), buffer);

      //send_internal(buffer.GetString(), buffer.GetSize());
      break;
    }
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
