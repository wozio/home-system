#include "client.h"
#include "json_converter.h"
#include "logger.h"
#include "app.h"
#include <thread>
//#include <chrono>
#include <cstdlib>

using namespace std;

namespace home_system
{

client::client(ws_t ws)
: handler(ws)
{
  LOG(DEBUG) << "Client connected";
}

client::~client()
{
}

void client::on_read(data_t data, size_t data_size)
{
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
    const std::string& source, const std::string& target)
{
  // verify if requested email is known and password is correct
  string req_email = params.get_string("email");
  string req_password = params.get_string("password");

  LOG(DEBUG) << "Login message for email:" << req_email;

  for (auto& v : home_system::app::config().get_child("users"))
  {
    string email = v.second.get<string>("email");
    if (email == req_email)
    {
      if (v.second.get<string>("password") == req_password)
      {
        string name = v.second.get<string>("name");
        LOG(DEBUG) << "User " << name << " (" << email << ") is logged in";

        // create client service and assign id
        string new_client = create_client(name);

        LOG(DEBUG) << "Client assigned: " << new_client;

        clients_[new_client] = make_shared<client_service>(new_client);

        yami::parameters rparams;
        rparams.set_string("name", name);
        rparams.set_string("email", email);
        rparams.set_string("client_id", new_client);
        size_t out_size = 0;
        auto out = create_data();
        to_json(target, source, rparams, sequence_number, out, out_size);
        on_send(shared_from_this(), out, out_size);
        return;
      }
    }
  }
  throw runtime_error("Unknown email or wrong password");
}

void client::handle_logout(const std::string& source)
{
  clients_.erase(source);
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
    long long sequence_number;

    auto msg_type = from_json(data, source, target, msg, sequence_number, params);

    try
    {
      // special handling for login message
      if (msg_type == msg_type_t::for_reply && msg == "login")
      {
        handle_login(params, sequence_number, source, target);
        return;
      }
      else if (msg == "logout")
      {
        handle_logout(source);
        return;
      }

      if (!this->is_logged_in(source))
      {
        LOG(WARNING) << "Message '" << msg << "' from not logged in source '" << source << "' to '" << "'";
        throw runtime_error("Message from not logged in source");
      }

      LOG(DEBUG) << "Message: '" << msg << "', from '" << source << "' to '" << target << "'";

      clients_[target]->on_remote_msg(source, target, msg_type, msg, sequence_number, params);

      {
        
      }
    }
    catch (const exception& e)
    {
      // reject message if reply is expected
      LOG(WARNING) << "EXCEPTION: " << e.what();
      reject(expect_reply, sequence_number, target, source, e.what());
    }
  }
  catch (const exception& e)
  {
    // JSON parsing has failed, ignore such message
    LOG(WARNING) << "EXCEPTION: " << e.what();
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
  return clients_.find(client) != clients_.end();
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
