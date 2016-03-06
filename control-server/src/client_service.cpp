#include "client_service.h"
#include "logger.h"
#include "yamicontainer.h"
#include "discovery.h"
#include "handler.h"
#include "json_converter.h"

using namespace std;

namespace home_system
{

client_service::client_service(const std::string& name, handler_t handler)
: service(name),
  name_(name),
  handler_(handler)
{
}

void client_service::on_msg(yami::incoming_message & im)
{
  // every incoming message is translated to json, sequence number is assigned
  // and sent towards remote
  // timer is also started
  
}

void client_service::on_remote_msg(const std::string& source, const std::string& target,
  msg_type_t msg_type, const std::string& msg,
  int sequence_number, const yami::parameters& params)
{
  string ye = DISCOVERY.get(target);

  switch (msg_type)
  {
  case msg_type_t::one_way:
    LOG(DEBUG) << "One way message: '" << msg << "', from '" << source << "' to '" << target << "'";
    AGENT.send_one_way(ye, target, msg, params);
    break;

  case msg_type_t::for_reply:
  {
    LOG(DEBUG) << "Message expecting reply: '" << msg << "', from '" << source << "' to '" << target << "'";
    auto_ptr <yami::outgoing_message> message(AGENT.send(ye, target, msg, params));

    message->wait_for_completion(1000);

    switch (message->get_state())
    {
    case yami::replied:
    {
      LOG(DEBUG) << "Got reply";
      // converting yami output to json
      // yami binary values are not supported
      size_t out_size = 0;
      auto out = create_data();
      to_json(target, source, message->get_reply(), sequence_number, out, out_size);

      handler::on_send(handler_, out, out_size);

      break;
    }

    case yami::posted:
    case yami::transmitted:
    case yami::abandoned:
      LOG(WARNING) << "Posted/Transmitted/Abandoned after timeout";
      break;

    case yami::rejected:
    {
      LOG(WARNING) << "Rejected: " + message->get_exception_msg();
      size_t out_size = 0;
      auto out = create_data();
      to_json(target, source, message->get_exception_msg(), sequence_number, out, out_size);
      handler::on_send(handler_, out, out_size);
      break;
    }
    }
    break;
  }

  case msg_type_t::reply:
    break;
  }
}

}
