#include "iorb-service.h"
#include "logger.h"
#include "yamicontainer.h"

using namespace boost;
using namespace std;

namespace home_system
{
namespace input_output
{

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
  if (im.get_message_name() == "get_all_outputs")
  {
    vector<int> outputs;
    for (size_t j = 0; j < 8; ++j)
      outputs.push_back(j);
    
    yami::parameters params;
    params.set_integer_array_shallow("outputs", &outputs[0], outputs.size());
    im.reply(params);
  }
  else if (im.get_message_name() == "get_outputs")
  {
    vector<long long> outputs;
    for (size_t j = 0; j < 8; ++j)
      outputs.push_back(j);
    
    yami::parameters params;
    params.set_long_long_array_shallow("outputs", &outputs[0], outputs.size());
    im.reply(params);
  }
  else if (im.get_message_name() == "get_output_state")
  {
    int output = im.get_parameters().get_integer("output");
    yami::parameters params;
    params.set_integer("output", output);
    params.set_integer("state", port_.get_relay_state(output));
    im.reply(params);
  }
  else if (im.get_message_name() == "set_output_state")
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
    int id = im.get_parameters().get_long_long("id");
    
    // adding subscription to port
    rs nrs;
    nrs.name_ = im.get_parameters().get_string("name");
    nrs.ye_ = im.get_parameters().get_string("endpoint");

    {
      lock_guard<mutex> lock(subscription_mutex_);
      state_subscriptions_.insert(std::pair<int, rs>(id, nrs));
    }

    LOG(nrs.name_ << " (" << nrs.ye_ << ") subscribed for changes of " << id);
    
    // sending current state
    on_output_state_change(id, port_.get_relay_state(id));
  }
  else
  {
    service::on_msg(im);
  }
}

void iorb_service::on_output_state_change(int output, int state)
{
  lock_guard<mutex> lock(subscription_mutex_);

  if (state_subscriptions_.find(output) != state_subscriptions_.end())
  {
    yami::parameters params;
    params.set_string("name", name_);
    params.set_integer("id", output);
    params.set_integer("state", state);
    
    auto subs = state_subscriptions_.equal_range(output);
    for (auto it = subs.first; it != subs.second; )
    {
      LOG("Sending state change to subscription " << it->second.name_ << " (" << it->second.ye_ << ") for: " << output);
      try
      {
        AGENT.send(it->second.ye_, it->second.name_,
          "state_change", params);
        ++it;
      }
      catch (const yami::yami_runtime_error& e)
      {
        LOGWARN("EXCEPTION: " << e.what() << ". Removing subscription for output: " << output);
        state_subscriptions_.erase(it++);
      }
    }
  }
}

}
}

