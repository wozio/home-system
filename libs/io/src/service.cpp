#include <algorithm>
#include <map>

#include "service.h"
#include "utils/logger.h"
#include "com/discovery.h"
#include "com/yamicontainer.h"

using namespace std;
using namespace yami;

namespace home_system
{
namespace io
{

service::service(const std::string &name)
    : com::service(name, false)
{
  init();
}

service::~service()
{
}

void service::add_device(device_t device)
{
  auto id = device->get_id();
  if (devices_.find(id) != devices_.end())
  {
    throw runtime_error("attempt to add device which is already added (same ID)");
  }
  devices_[id] = device;
  device->on_state_change.connect([this](io_id_t id) {
    this->on_device_state_change(id);
  });
  device->on_value_change.connect([this](io_id_t id) {
    this->on_device_value_change(id);
  });
}

void service::remove_device(io_id_t id)
{
  devices_.erase(id);
}

void service::clear_devices()
{
  devices_.clear();
}

void service::set_state_for_all(io_state_t state)
{
  for (auto& device : devices_)
  {
    device.second->set_state(home_system::io::io_state_t::faulty);
  }
}

void service::on_msg(incoming_message &im)
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
  else if (im.get_message_name() == "set_io_value")
  {
    auto params = im.get_parameters();
    auto id = params.get_long_long("id");
    auto it = devices_.find(id);
    if (it != devices_.end())
    {
      it->second->extract_wanted_value(params);
    }
  }
  else
  {
    service::on_msg(im);
  }
}

void service::send_current_state()
{
  for (auto it : devices_)
  {
    LOG(DEBUG) << "Sending for id " << it.first;
    on_device_state_change(it.first);
    on_device_value_change(it.first);
  }
}

void service::on_device_state_change(io_id_t id)
{
  if (devices_.find(id) == devices_.end())
  {
    // it may happen when device object is removed from service but not destroyed
    return;
  }

  lock_guard<mutex> lock(subscription_mutex_);

  yami::parameters params;
  params.set_string("name", service::name());
  params.set_long_long("id", id);

  auto d = devices_[id];
  params.set_long_long("data_type", static_cast<int>(d->get_data_type()));
  params.set_string("type", d->get_type());
  params.set_long_long("state", static_cast<int>(d->get_state()));

  for (auto it = subscriptions_.begin(); it != subscriptions_.end();)
  {
    LOG(DEBUG) << "Sending io state change to subscription " << *it << " for: " << hex << id;
    try
    {
      auto ep = DISCOVERY.get(*it);
      AGENT.send_one_way(ep, *it,
                         "io_state_change", params);
      ++it;
    }
    catch (const exception &e)
    {
      LOG(WARNING) << "EXCEPTION: " << e.what() << ". Removing subscription: " << *it;
      subscriptions_.erase(it++);
    }
  }
}

void service::on_device_value_change(io_id_t id)
{
  auto di = devices_.find(id);
  if (di == devices_.end())
  {
    // it may happen when device object is removed from service but not destroyed
    return;
  }

  lock_guard<mutex> lock(subscription_mutex_);

  yami::parameters params;
  params.set_string("name", service::name());
  params.set_long_long("id", id);

  auto d = di->second;

  if (d->get_state() == io_state_t::ok)
  {
    d->write_value(params);

    for (auto it = subscriptions_.begin(); it != subscriptions_.end();)
    {
      LOG(DEBUG) << "Sending io value change to subscription " << *it << " for: " << hex << id;
      try
      {
        auto ep = DISCOVERY.get(*it);
        AGENT.send_one_way(ep, *it,
                           "io_value_change", params);
        ++it;
      }
      catch (const exception &e)
      {
        LOG(WARNING) << "EXCEPTION: " << e.what() << ". Removing subscription: " << *it;
        subscriptions_.erase(it++);
      }
    }
  }
}

}
}