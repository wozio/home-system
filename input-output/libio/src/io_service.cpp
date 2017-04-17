#include <algorithm>
#include <map>

#include "io/io_service.h"
#include "logger.h"
#include "discovery.h"
#include "yamicontainer.h"

using namespace std;
using namespace yami;

namespace home_system
{

io_service::io_service(const std::string& name)
: service(name, false)
{
  init();
}

io_service::~io_service()
{
}

void io_service::add_device(io_device_t device)
{
  auto id = device->get_id();
  if (devices_.find(id) != devices_.end())
  {
    throw runtime_error("attempt to add device which is already added (same ID)");
  }
  devices_[id] = device;
}

void io_service::remove_device(io_id_t id)
{
  devices_.erase(id);
}

void io_service::clear_devices()
{
  devices_.clear();
}

void io_service::on_msg(incoming_message & im)
{
  if (im.get_message_name() == "subscribe")
  {
  	auto params = im.get_parameters();
    {
      lock_guard<mutex> lock(subscription_mutex_);
      subscriptions_.insert(params.get_string("name"));
    }

    LOG(DEBUG) << params.get_string("name") << " subscribed";
    
    send_current_state();
  }
  else
  {
    service::on_msg(im);
  }
}

void io_service::send_current_state()
{
  for (auto it : devices_)
  {
    LOG(DEBUG) << "Sending for id " << it.first;
    on_device_change(it.first);
  }
}

void io_service::on_device_change(io_id_t id)
{
  lock_guard<mutex> lock(subscription_mutex_);

  yami::parameters params;
  params.set_string("name", service::name());
  params.set_long_long("id", id);
  
  auto d = devices_[id];
  params.set_long_long("type", static_cast<int>(d->get_type()));
  params.set_long_long("state", static_cast<int>(d->get_state()));

  if (d->get_state() == io_state_t::ok)
  {
    auto& v = d->get_value();

    switch (d->get_type())
    {
    case io_type_t::temperature_input:
    {
      auto cv =  boost::any_cast<double>(v);
      params.set_double_float("value", cv);
      break;
    }
    default:
      throw std::runtime_error("Unsupported IO type");
    }
  }

  for (auto it = subscriptions_.begin(); it != subscriptions_.end();)
  {
    LOG(DEBUG) << "Sending state change to subscription " << *it << " for: " << hex << id;
    try
    {
      auto ep = DISCOVERY.get(*it);
      AGENT.send_one_way(ep, *it,
        "io_change", params);
      ++it;
    }
    catch (const exception& e)
    {
      LOG(WARNING) << "EXCEPTION: " << e.what() << ". Removing subscription: " << *it;
      subscriptions_.erase(it++);
    }
  }
}

}