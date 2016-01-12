#include "control-service.h"
#include "discovery.h"
#include "logger.h"

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
  else
  {
    service::on_msg(im);
  }
}

}
}
