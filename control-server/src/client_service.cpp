#include "client_service.h"
#include "logger.h"

using namespace std;

namespace home_system
{

client_service::client_service(const std::string& name)
: service(name),
  name_(name)
{
}

void client_service::on_msg(yami::incoming_message & im)
{
  //if (im.get_message_name() == "get_services")
  {
  }
  //else
  {
    service::on_msg(im);
  }
}

}
