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
  if (im.get_message_name() == "get_inputs")
  {
    parameters params;
    std::vector<long long> ids;
    net_.get_inputs(ids);
    params.set_long_long_array_shallow("inputs", &ids[0], ids.size());
    im.reply(params);
  }
  else if (im.get_message_name() == "get_input_value")
  {
    uint64_t id = im.get_parameters().get_long_long("input");
    parameters params;
    ow::temp& input = net_.get_input(id);
    params.set_double_float("value", input.get_value());
    params.set_long_long("time", input.get_time());
    im.reply(params);
  }
  else
  {
    service::on_msg(im);
  }
}

}
}
