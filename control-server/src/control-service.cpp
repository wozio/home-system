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
    for (auto& v : home_system::app::config().get_child("users.email"))
    {
      LOG("User: " << v.second.data());
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
