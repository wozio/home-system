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

clients client::clients_;

client::client(ws_t ws)
: handler(ws)
{
  LOG(DEBUG) << "Client connected";
}

client::~client()
{
}

void client::shutdown()
{
  LOG(DEBUG) << "Shutdown";
  this->logout(client_id_);
  handler::shutdown();
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

  LOG(DEBUG) << "Login message for client id:" << req_email;

  for (auto& v : home_system::app::config().get_child("users"))
  {
    string email = v.second.get<string>("email");
    if (email == req_email)
    {
      if (v.second.get<string>("password") == req_password)
      {
        string name = v.second.get<string>("name");
        LOG(DEBUG) << "User " << name << " (" << email << ") is logged in";

        auto client_id = clients_.add(name, shared_from_this());
        this->login(client_id);

        LOG(DEBUG) << "Client assigned: " << client_id;

        yami::parameters rparams;
        rparams.set_string("name", name);
        rparams.set_string("email", email);
        rparams.set_string("client_id", client_id);
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
  LOG(DEBUG) << "Logout message for client id:" << source;
  this->logout(source);
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

    // special handling for login message
    if (msg_type == msg_type_t::for_reply && msg == "login")
    {
      handle_login(params, sequence_number, source, target);
      return;
    }
    else if (msg_type == msg_type_t::one_way && msg == "logout")
    {
      handle_logout(source);
      return;
    }

    if (!this->is_logged_in(source))
    {
      LOG(WARNING) << "Message '" << msg << "' from not logged in source '" << source << "' to '" << "'";
      throw runtime_error("Message from not logged in source");
    }

    clients_.get(source)->on_remote_msg(source, target, msg_type, msg, sequence_number, params);
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
  return client_id_ == client;
}

void client::login(const std::string& client)
{
  client_id_ = client;
}

void client::logout(const std::string& client)
{
  clients_.remove(client);
  client_id_ = "";
}

}
