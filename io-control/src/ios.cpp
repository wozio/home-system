#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/config.h"
#include "utils/logger.h"
#include "ios.h"

ios::ios(lua_State *lua)
: lua_(lua),
  home_system::com::service("io-control.devices", false)
{
  // register LUA callbacks
  lua_pushcfunction(lua_, get_io_state_value);
  lua_setglobal(lua_, "get_io_state_value");

  lua_pushcfunction(lua_, set_io_value);
  lua_setglobal(lua_, "set_io_value");

  lua_pushcfunction(lua_, register_io);
  lua_setglobal(lua_, "register_io");
}

ios::~ios()
{
}

void ios::init()
{
  // register_ios runs register_io callback for each io that is defined
  lua_getglobal(lua_, "register_ios");
  if (lua_pcall(lua_, 0, 0, 0))
  {
    LOG(ERROR) << "Error running register_ios function: " << lua_tostring(lua_, -1);
    lua_pop(lua_, 1);
    throw std::runtime_error("Error running register_ios function");
  }
}

void ios::on_msg(yami::incoming_message &im)
{
  if (im.get_message_name() == "io_value_change")
  {
    // message from IO driver service, meaning that state has changed
    // it is also sent in series after subscription
    auto params = im.get_parameters();

    auto remote_id = params.get_long_long("id");
    auto service_name = params.get_string("name");

    LOG(TRACE) << "IO value change: '" << service_name << "':" << remote_id;

    auto it = io_devices_by_service_.find(service_name);
    if (it != io_devices_by_service_.end())
    {
      auto& ios_in_service_map = it->second;
      auto it2 = ios_in_service_map.find(remote_id);
      if (it2 != ios_in_service_map.end())
      {
        auto rio = it2->second;
        if (rio)
        {
          // IO object will extract value from parameters
          rio->extract_value(params); 
        }
      }
    }
  }
  else if (im.get_message_name() == "io_state_change")
  {
    // message from IO driver service, meaning that state or value
    // has changed
    // it is also sent in series after subscription
    auto params = im.get_parameters();

    auto remote_id = params.get_long_long("id");
    auto service_name = params.get_string("name");
    auto state = params.get_long_long("state");

    LOG(TRACE) << "IO state change: '" << service_name << "':" << remote_id << " state: " << state;

    auto it = io_devices_by_service_.find(service_name);
    if (it != io_devices_by_service_.end())
    {
      auto& ios_in_service_map = it->second;
      auto it2 = ios_in_service_map.find(remote_id);
      if (it2 != ios_in_service_map.end())
      {
        auto rio = it2->second;
        if (rio)
        {
          rio->set_state(state);
        }
      }
    }
  }
}

void ios::add_remote(const std::string& service, home_system::io::io_id_t id,
    home_system::io::device_t device)
{
  io_devices_by_service_[service][id] = device;
}

void ios::add_schedule(home_system::io::io_id_t id, schedule_t schedule)
{
  schedules_[id] = schedule;
}

void ios::add(home_system::io::io_id_t id, home_system::io::device_t device)
{
  io_devices_[id] = device;
}

home_system::io::device_t ios::get(home_system::io::io_id_t id)
{
  return io_devices_.at(id);
}

void ios::kickoff()
{
  // initialize service
  home_system::com::service::init();

  DISCOVERY.subscribe([this](const std::string &name, bool available)
  {
    if (available)
    {
      // if name begins with 'io.' it means it is IO device
      // driver
      // TODO: will be changed when discovery mechanism will
      // be changed
      if (name.substr(0, 3) == "io.")
      {
        LOG(INFO) << "IO device driver service '" << name << "' discovered, subscribing";
        yami::parameters params;
        params.set_string("name", service::name());
        auto ep = DISCOVERY.get(name);
        AGENT.send_one_way(ep, name, "subscribe", params);
      }
    }
    else
    {
      if (name.substr(0, 3) == "io.")
      {
        // setting state of all IO objects belonging to this
        // service driver as unknown
        auto s = io_devices_by_service_.find(name);
        if (s != io_devices_by_service_.end())
        {
          auto& im = s->second;
          for (auto i : im)
          {
            i.second->set_state(home_system::io::io_state_t::unknown);
          }
        }
      }
    }
  });

  // kickoff schedules
  for (auto& s : schedules_)
  {
    s.second->kickoff();
  }
}

