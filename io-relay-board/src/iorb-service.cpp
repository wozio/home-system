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

enum class io_state
{
  unknown,
  ok
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
    int id = im.get_parameters().get_integer("id");
    int state = im.get_parameters().get_integer("value");

    LOG("Set output value: " << id << " to " << state);

    if (id < 8)
    {
      state ? port_.enable_relay(id) : port_.disable_relay(id);
    }
  }
  else if (im.get_message_name() == "subscribe")
  {
    auto params = im.get_parameters();
    auto ns = make_pair<std::string, std::string>(
      std::string(params.get_string("endpoint")),
      std::string(params.get_string("name")));
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
  params.set_integer("state", state == -1 ? static_cast<int>(io_state::unknown) : static_cast<int>(io_state::ok));
  // for simplicity we always send value even when state is not OK
  params.set_integer("value", state);

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

