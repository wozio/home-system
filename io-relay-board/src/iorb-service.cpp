#include "iorb-service.h"
#include "logger.h"
#include "yamicontainer.h"

using namespace boost;
using namespace std;

namespace home_system
{
namespace input_output
{

enum class io_type
{
  output_switch = 1
};

iorb_service::iorb_service(const std::string& name, const std::string& port)
: service(name, false),
  port_(port, this)
{
  init();
}

iorb_service::~iorb_service()
{
  deinit();
}

void iorb_service::on_msg(yami::incoming_message & im)
{
  if (im.get_message_name() == "set_output_state")
  {
    int output = im.get_parameters().get_integer("output");
    int state = im.get_parameters().get_integer("state");

    LOG("Set output state: " << output << " to " << state);

    if (output < 8)
    {
      state ? port_.enable_relay(output) : port_.disable_relay(output);
    }
  }
  else if (im.get_message_name() == "subscribe")
  {
    auto ns = make_pair<std::string, std::string>(
      std::string(im.get_parameters().get_string("endpoint")),
      std::string(im.get_parameters().get_string("name")));
    {
      lock_guard<mutex> lock(subscription_mutex_);
      subscriptions_.insert(ns);
    }

    LOG(ns.second << " (" << ns.first << ") subscribed");
    
    send_current_state();
  }
  else
  {
    service::on_msg(im);
  }
}

void iorb_service::send_current_state()
{
  std::vector<long long> ids;
  for (size_t id = 0; id < 8; id++)
  {
    on_output_state_change(id, port_.get_relay_state(id));
  }
}

void iorb_service::on_output_state_change(int id, int state)
{
  lock_guard<mutex> lock(subscription_mutex_);

  yami::parameters params;
  params.set_string("name", service::name());
  params.set_long_long("id", id);
  params.set_integer("type", static_cast<int>(io_type::output_switch));
  params.set_integer("state", state);

  for (auto it = subscriptions_.begin(); it != subscriptions_.end();)
  {
    LOG("Sending state change to subscription " << it->second << " (" << it->first << ") for: " << id);
    try
    {
      AGENT.send_one_way(it->first, it->second,
        "state_change", params);
      ++it;
    }
    catch (const std::exception& e)
    {
      LOGWARN("EXCEPTION: " << e.what() << ". Removing subscription for: " << id);
      subscriptions_.erase(it++);
    }
  }
}

}
}

