#include "io-service.h"
#include "logger.h"

using namespace yami;

namespace home_system
{
namespace input_output
{

io_service::io_service()
: service("input-output"),
  net_("DS2490-1")
{
  // todo: make more than 1 network possible with auto detect and adding each
}

io_service::~io_service()
{
}

void io_service::on_msg(incoming_message & im)
{
  if (im.get_message_name() == "get_all_inputs")
  {
    parameters params;
    std::vector<int> ids;
    ids.push_back(123);
    ids.push_back(456);
    ids.push_back(789);
    ids.push_back(98);
    ids.push_back(765);
    params.set_integer_array_shallow("inputs", &ids[0], ids.size());
    im.reply(params);
  }
  else if (im.get_message_name() == "get_input_data")
  {
    parameters params;
    int id = 0;
    id = im.get_parameters().get_integer("id");
    switch (id)
    {
    case 123:
      params.set_integer("type", 1);
      params.set_string("name", "salon");
      params.set_double_float("value", 22.4);
      break;
    case 456:
      params.set_integer("type", 1);
      params.set_string("name", "piwnica");
      params.set_double_float("value", 19.2);
      break;
    case 789:
      params.set_integer("type", 2);
      params.set_string("name", "alarm");
      params.set_boolean("value", false);
      break;
    case 98:
      params.set_integer("type", 2);
      params.set_string("name", "drzwi wejsciowe");
      params.set_boolean("value", false);
      break;
    case 765:
      params.set_integer("type", 1);
      params.set_string("name", "temp pow za gwc");
      params.set_double_float("value", 15.32);
      break;
    default:
      std::cout << "unknown input id" << std::endl;
      im.reject("unknown input id");
    }
    im.reply(params);
  }
  else if (im.get_message_name() == "get_input_history")
  {
    std::vector<double> history;
    net_.get_input_history(0, history);
    parameters params;
    params.set_double_float_array_shallow("values", &history[0], history.size());
    im.reply(params);
  }
  else
  {
    LOGWARN("unknown message" << im.get_message_name());
    im.reject("unknown message");
  }
}

}
}
