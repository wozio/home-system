#include <algorithm>

#include "io-service.h"
#include "logger.h"
#include "yamicontainer.h"

using namespace std;
using namespace yami;

namespace home_system
{
namespace input_output
{

io_service::io_service()
: service("input-output"),
  net_("DS2490-1",
    [this](uint64_t id)
    {
      on_state_change(id);
    })
{
  // todo: configurable path to adapter
  
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
  else if (im.get_message_name() == "subscribe_state_change")
  {
    uint64_t id = im.get_parameters().get_long_long("id");
    
    // adding subscription to input
    rs nrs;
    nrs.name_ = im.get_parameters().get_string("name");
    nrs.ye_ = im.get_parameters().get_string("endpoint");

    {
      lock_guard<mutex> lock(subscription_mutex_);
      state_subscriptions_.insert(std::pair<uint64_t, rs>(id, nrs));
    }

    LOG(nrs.name_ << " (" << nrs.ye_ << ") subscribed for changes of " << id);
    
    for (auto crs : state_subscriptions_)
    {
      LOG(crs.second.name_ << " (" << crs.second.ye_ << ") subscribed for changes of " << id);
    }
    
    // sending current state
    on_state_change(id);
  }
  else
  {
    service::on_msg(im);
  }
}

void io_service::on_state_change(uint64_t id)
{
  lock_guard<mutex> lock(subscription_mutex_);

  if (state_subscriptions_.find(id) != state_subscriptions_.end())
  {
    try
    {
      yami::parameters params;
      params.set_string("name", service::name());
      params.set_long_long("id", id);
      ow::temp& input = net_.get_input(id);
      params.set_double_float("state", input.get_value());
      params.set_long_long("time", input.get_time());
      
      auto subs = state_subscriptions_.equal_range(id);
      for (auto it = subs.first; it != subs.second; ++it)
      {
        LOG("Sending state change to subscription " << it->second.name_ << " (" << it->second.ye_ << ") for output: " << id);
      
        AGENT.send(it->second.ye_, it->second.name_,
          "output_state_change", params);
      }
    }
    catch (const yami::yami_runtime_error& e)
    {
      LOGWARN("EXCEPTION: yami_runtime_error: " << e.what());
      LOG("Removing subscription for output: " << id);
    }
  }
}

}
}
