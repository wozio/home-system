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
using namespace rapidjson;

namespace home_system
{

client::client(ws_t ws)
: handler(ws),
  client_state_(wait_for_login)
{
  LOG(DEBUG) << "New client connected";
  
  lock_guard<mutex> lock(client_state_mutex_);
}

void client::logout()
{
  Document msg(kObjectType);
  auto& alloc = msg.GetAllocator();
  msg.AddMember("message", "logout", alloc);
  if (client_state_ == wait_for_login_reply)
  {
    msg.AddMember("source", StringRef(tmp_route_key_.c_str()), alloc);
  }
  else
  {
    msg.AddMember("source", StringRef(route_key_.c_str()), alloc);
  }
  msg.AddMember("target", "control-server", alloc);
  
  buffer_t buffer(new StringBuffer);
  Writer<StringBuffer> writer(*buffer);
  msg.Accept(writer);

  on_send(shared_from_this(), buffer);
}

client::~client()
{
  lock_guard<mutex> lock(client_state_mutex_);
  switch (client_state_)
  {
    case wait_for_login_reply:
      system_->unset_route(tmp_route_key_);
      logout();
      break;
      
    case logged_in:
      system_->unset_route(route_key_);
      logout();
      break;
      
    default:
      break;
  }
}

void client::send(data_t data, size_t data_size)
{
  lock_guard<mutex> lock(client_state_mutex_);
  switch (client_state_)
  {
  case logged_in:
    // just forward message
    break;
  case wait_for_login_reply:
  {
    // it should be login reply
    // fetching client service id assigned by system.
    // it supposed to be unique within system so can be used as
    // system->client routing key in system object
    // decode JSON
    Document d;
    d.Parse(data->data());
    if (d.HasParseError())
    {
      LOG(WARNING) << "Parse error: " << d.GetErrorOffset() << ": " << GetParseError_En(d.GetParseError());
      throw std::runtime_error("JSON parse error");
    }

    if (!d.IsObject())
    {
      LOG(WARNING) << "Incorrect message, root element has to be Object";
      throw std::runtime_error("Incorrect message, root element has to be Object");
    }
      
    // checking sequence number
    auto itr = d.FindMember("sequence_number");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsInt())
      {
        int v = itr->value.GetInt();
        LOG(DEBUG) << "sequence_number in message: " << v;
        if (v != seq_num_)
        {
          LOG(WARNING) << "Sequence number in reply not equal to request";
          throw std::runtime_error("Sequence number in reply not equal to request");
        }
      }
      else
        throw std::runtime_error("Wrong sequence_number field");
    }
    else
      throw std::runtime_error("No sequence_number field");
      
    // checking result
    itr = d.FindMember("result");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsString())
      {
        string v = itr->value.GetString();
        LOG(DEBUG) << "result in message: " << v;
        if (v != "success")
        {
          throw std::runtime_error("Result not successful");
        }
      }
      else
        throw std::runtime_error("Wrong result field");
    }
      
    // now client id assigned by system for setting up route in system object
    itr = d.FindMember("params");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsObject())
      {
        auto itr2 = itr->value.FindMember("client_id");
        if (itr2 != itr->value.MemberEnd())
        {
          if (itr2->value.IsString())
          {
            string v = itr2->value.GetString();
            LOG(DEBUG) << "client_id in message: " << v;
            // set up route
            system_->unset_route(tmp_route_key_);
            system_->set_route(v, dynamic_pointer_cast<client>(shared_from_this()));
            route_key_ = v;
            client_state_ = logged_in;
          }
          else
            throw std::runtime_error("Wrong client_id field");
        }
        else
          throw std::runtime_error("No client_id field");
      }
      else
        throw std::runtime_error("Wrong params field");
    }
    else
      throw std::runtime_error("No params field");
      
    break;
  }
  default:
    // drop message
    return;
  }
  handler::send(data, data_size);
}

void client::on_read(data_t data, size_t data_size)
{
  lock_guard<mutex> lock(client_state_mutex_);
  switch (client_state_)
  {
  case logged_in:
    on_send(system_, data, data_size);
    break;
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

    itr = d.FindMember("parameters");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsObject())
      {
        auto itr2 = itr->value.FindMember("email");
        if (itr2 != itr->value.MemberEnd())
        {
          if (itr2->value.IsString())
          {
            string v(itr2->value.GetString());
            LOG(DEBUG) << "user in message: " << v;
            // get system associated with this user
            // it will throw an exception when user is not found in map
            // what means that system handling such user is not connected
            system_ = SYSTEMS.get(v);
          }
        }
      }
      else
        throw std::runtime_error("Wrong parameters field");
    }
    else
      throw std::runtime_error("No parameters field");

    itr = d.FindMember("sequence_number");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsInt())
      {
        int v = itr->value.GetInt();
        LOG(DEBUG) << "sequence_number in message: " << v;
        // remember this sequence number, it has to be the same in reply
        seq_num_ = v;
      }
      else
        throw std::runtime_error("Wrong sequence_number field");
    }
    else
      throw std::runtime_error("No sequence_number field");

    // set up temporary system->client route
    tmp_route_key_ = to_string(rand());
    system_->set_route(tmp_route_key_, dynamic_pointer_cast<client>(shared_from_this()));

    // adding source
    d.RemoveMember("source");
    d.AddMember("source", StringRef(tmp_route_key_.c_str()), d.GetAllocator());

    // now wait for login reply
    client_state_ = wait_for_login_reply;

    // send login message towards system
    buffer_t buffer(new StringBuffer);
    Writer<StringBuffer> writer(*buffer);
    d.Accept(writer);

    on_send(system_, buffer);
    break;
  }
  default:
    LOG(WARNING) << "message in wrong state";
    throw runtime_error("message in wrong state");
  }
}

void client::shutdown()
{
  LOG(DEBUG) << "Client shutdown";
  lock_guard<mutex> lock(client_state_mutex_);
  handler::shutdown();
  // from shared_from_this shared_ptr<handler> is obtained
  // casting it to shared_ptr<client>
  CLIENTS.remove(dynamic_pointer_cast<client>(shared_from_this()));
}

}