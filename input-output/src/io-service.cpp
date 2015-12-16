#include <algorithm>
#include <map>

#include "io-service.h"
#include "logger.h"
#include "yamicontainer.h"

using namespace std;
using namespace yami;

enum class io_type
{
  input_temperature = 0
};

namespace home_system
{
namespace input_output
{

io_service::io_service()
: service("io.1wire", false),
  net_("DS2490-1",
    [this](uint64_t id)
    {
      on_state_change(id);
    })
{
  // todo: configurable path to adapter
  init();
}

io_service::~io_service()
{
}

void io_service::on_msg(incoming_message & im)
{
  if (im.get_message_name() == "subscribe")
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

void io_service::send_current_state()
{
  std::vector<long long> ids;
  net_.get_inputs(ids);
  for (auto id : ids)
  {
    LOG("Sending for id " << id);
    on_state_change(id);
  }
}

void io_service::on_state_change(uint64_t id)
{
  lock_guard<mutex> lock(subscription_mutex_);

  yami::parameters params;
  params.set_string("name", service::name());
  params.set_long_long("id", id);
  params.set_integer("type", static_cast<int>(io_type::input_temperature));
  ow::temp& input = net_.get_input(id);
  params.set_double_float("state", input.get_value());

  for (auto it = subscriptions_.begin(); it != subscriptions_.end();)
  {
    LOG("Sending state change to subscription " << it->second << " (" << it->first << ") for: " << id);
    try
    {
      AGENT.send(it->first, it->second,
        "state_change", params);
      ++it;
    }
    catch (const yami::yami_runtime_error& e)
    {
      LOGWARN("EXCEPTION: " << e.what() << ". Removing subscription for: " << id);
      subscriptions_.erase(it++);
    }
    catch (const exception& e)
    {
      LOGWARN("EXCEPTION: " << e.what() << ". Removing subscription for: " << id);
      subscriptions_.erase(it++);
    }
  }
}

}
}
