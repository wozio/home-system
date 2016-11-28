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
: handler(ws, false),
  sys_state_(wait_for_login)
{
  LOGH(DEBUG) << "New system connected";
}

system::~system()
{
}

void system::set_route(const std::string& target, client_t client)
{
  LOGH(DEBUG) << "Set route: " << target;
  route_[target] = client;
  LOGH(DEBUG) << "Number of routes: " << route_.size();
}

void system::unset_route(const std::string& target)
{
  LOGH(DEBUG) << "Unset route: " << target;
  route_.erase(target);
  LOGH(DEBUG) << "Number of routes: " << route_.size();
}

void system::on_read(data_t data, size_t data_size, type_t data_type)
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
    LOGH(DEBUG) << "Parse error: " << d.GetErrorOffset() << ": " << rapidjson::GetParseError_En(d.GetParseError());
    throw std::runtime_error("JSON parse error");
  }
  if (!d.IsObject())
  {
    LOGH(DEBUG) << "Incorrect message, root element has to be Object";
    throw std::runtime_error("Incorrect message, root element has to be Object");
  }
  switch (sys_state_)
  {
    case logged_in:
    {
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
        c->second->send_to_client(d);
      }
      else
      {
        LOGH(WARNING) << "No route for target: " << target;
      }
      break;
    }
    case wait_for_login:
    {
      auto itr = d.FindMember("system");
      if (itr != d.MemberEnd())
      {
        if (itr->value.IsString())
        {
          string n(itr->value.GetString());
          if (n.size() > 0)
          {
            LOGH(DEBUG) << "System name: " << n;
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
              LOGH(DEBUG) << "Allowed user: " << user;
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
      
      on_send(buffer);
      break;
    }
  }
}

void system::send_to_system(const rapidjson::Document& d)
{
  if (sys_state_ == logged_in)
  {
    buffer_t buffer(new StringBuffer);
    Writer<StringBuffer> writer(*buffer);
    d.Accept(writer);
    
    on_send(buffer);
  }
}

void system::shutdown()
{
  LOGH(DEBUG) << "System shutdown";
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<system>
  SYSTEMS.remove(dynamic_pointer_cast<system>(shared_from_this()));
  handler::shutdown();
  
  for (auto r : route_)
  {
    r.second->system_disconnected();
  }
}

}
