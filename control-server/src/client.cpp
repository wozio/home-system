#include "client.h"
#include "json_converter.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include "app.h"
#include <thread>
//#include <chrono>
#include <cstdlib>

using namespace std;

namespace home_system
{

std::map<std::string, client::client_service_t> client::clients_;

client::client(ws_t ws)
: handler(ws)
{
  LOG("Client connected");
}

client::~client()
{
  if (clients_.find(client_) != clients_.end())
  {
    clients_.erase(client_);
  }
}

void client::on_read(data_t data, size_t data_size)
{
  LOG("Read " << data_size << " bytes");
  try
  {
    thread t([this](data_t data, size_t data_size) {
      this->handle_data(data, data_size);
    }, data, data_size);
    t.detach();
  }
  catch (...)
  {
  }
}

void client::handle_login(const yami::parameters& params, long long sequence_number,
    const std::string& target)
{
  // verify if requested email is known and password is correct
  string req_email = params.get_string("email");
  string req_password = params.get_string("password");

  LOG("Login message for email:" << req_email);

  for (auto& v : home_system::app::config().get_child("users"))
  {
    string email = v.second.get<string>("email");
    if (email == req_email)
    {
      if (v.second.get<string>("password") == req_password)
      {
        string name = v.second.get<string>("name");
        LOG("User " << name << " (" << email << ") is logged in");

        // create client service and assign id
        string new_client = create_client(name);

        LOG("Client assigned: " << new_client);

        login(new_client);

        yami::parameters rparams;
        rparams.set_string("name", name);
        rparams.set_string("email", email);
        rparams.set_string("client_id", new_client);
        size_t out_size = 0;
        auto out = create_data();
        to_json(target, new_client, rparams, sequence_number, out, out_size);
        on_send(shared_from_this(), out, out_size);
        return;
      }
    }
  }
  client_ = "";
  throw runtime_error("Unknown email or wrong password");
}

void client::handle_data(data_t data, size_t data_size)
{
  try
  {
    // it is guaranteed that data size is bigger than data_size
    (*data)[data_size] = '\0';

    // converting from JSON to YAMI message
    yami::parameters params;
    std::string msg;
    std::string source;
    std::string target;
    bool expect_reply;
    long long sequence_number;

    from_json(data, source, target, msg, expect_reply, sequence_number, params);

    try
    {
      // special handling for login message
      if (msg == "login")
      {
        handle_login(params, sequence_number, target);
        return;
      }

      LOG("Message: " << msg << ", from " << source << " to " << target);

      if (!this->is_logged_in(source))
      {
        throw runtime_error("Message from not logged in source");
      }

      string ye = DISCOVERY.get(target);

      // sending message to service
      if (!expect_reply)
      {
        AGENT.send_one_way(ye, target, msg, params);
      }
      else
      {
        auto_ptr <yami::outgoing_message> message(AGENT.send(ye, target, msg, params));

        message->wait_for_completion(1000);

        switch (message->get_state())
        {
        case yami::replied:
        {
          //LOG("Replied");
          // converting yami output to json
          // yami binary values are not supported
          size_t out_size = 0;
          auto out = create_data();
          to_json(target, source, message->get_reply(), sequence_number, out, out_size);

          on_send(shared_from_this(), out, out_size);

          break;
        }

        case yami::posted:
        case yami::transmitted:
        case yami::abandoned:
          LOGWARN("Posted/Transmitted/Abandoned after timeout");
          throw runtime_error("Message was abandoned");
          break;

        case yami::rejected:
          LOGWARN("Rejected: " + message->get_exception_msg());
          throw runtime_error(
              "Message was rejected: " + message->get_exception_msg());
          break;
        }
      }
    }
    catch (const exception& e)
    {
      // reject message if reply is expected
      LOGWARN("EXCEPTION: " << e.what());
      reject(expect_reply, sequence_number, target, source, e.what());
    }
  }
  catch (const exception& e)
  {
    // JSON parsing has failed, ignore such message
    LOGWARN("EXCEPTION: " << e.what());
  }
}

void client::reject(bool expect_reply, long long sequence_number,
    const std::string& target, const std::string& source, const char* reason)
{
  reject(expect_reply, sequence_number, target, source, string(reason));
}

void client::reject(bool expect_reply, long long sequence_number,
    const std::string& target, const std::string& source, const std::string& reason)
{
  if (expect_reply)
  {
    size_t out_size = 0;
    auto out = create_data();
    to_json(target, source, reason, sequence_number, out, out_size);
    client::on_send(shared_from_this(), out, out_size);
  }
}

bool client::is_logged_in(const std::string& client)
{
  return client_.size() > 0 && client_ == client;
}

void client::login(const std::string& client)
{
  if (clients_.find(client_) != clients_.end())
  {
    clients_.erase(client_);
  }
  clients_[client] = make_shared<client_service>(client);
  client_ = client;
}

std::string client::create_client(const std::string& name)
{
  string client_name;
  do
  {
    client_name = name + to_string(rand());
  }
  while (clients_.find(client_name) != clients_.end());
  return client_name;
}

}
