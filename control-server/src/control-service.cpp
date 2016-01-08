#include "control-service.h"
#include "discovery.h"
#include "app.h"
#include "logger.h"
#include <vector>

using namespace std;

namespace home_system
{
namespace control_server
{

control_service::control_service()
: service("control-server")
{
}

void control_service::on_msg(yami::incoming_message & im)
{
  if (im.get_message_name() == "get_services")
  {
    auto services = DISCOVERY.get_all();
    yami::parameters params;
    params.create_string_array("services", services.size());
    size_t i = 0;
    for (auto serviceit : services)
    {
      params.set_string_in_array("services", i, serviceit.first);
      i++;
    }
    im.reply(params);
  }
  else if (im.get_message_name() == "login")
  {
    string req_email = im.get_parameters().get_string("email");
    string req_password = im.get_parameters().get_string("password");
    for (auto& v : home_system::app::config().get_child("users"))
    {
      string email = v.second.get<string>("email");
      if (email == req_email)
      {
        if (v.second.get<string>("password") == req_password)
        {
          string name = v.second.get<string>("name");
          LOG("User " << name << "(" << email << ") is logged in");
          yami::parameters params;
          params.set_string("name", name);
          params.set_string("email", email);
          im.reply(params);
          return;
        }
      }
    }
    im.reject("Unknown email or bad password");
  }
  else
  {
    service::on_msg(im);
  }
}

}
}
