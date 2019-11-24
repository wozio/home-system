#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/config.h"
#include "utils/logger.h"
#include "io/device_types.h"
#include "io/device_int.h"
#include "io/device_float.h"
#include "ios.h"
#include "weekly_schedule.h"
#include "interval_schedule.h"

extern ios_t _ios;

int get_io_state_value(lua_State* L)
{
  std::string io_name = lua_tostring(L, 1);
  std::string rule_name = lua_tostring(L, 2);

  LOG(TRACE) << "Get IO state and value: IO: " << io_name << " from rule " << rule_name;

  try
  {
    // get IO and its state
    auto io = _ios->get(io_name);
    auto s = io->get_state();
    // state is always returned
    lua_pushnumber(L, static_cast<int>(s));
    /// value is returned only when state is OK
    if (s == home_system::io::io_state_t::ok)
    {
      // converting to proper type
      switch (io->get_data_type())
      {
        case home_system::io::io_data_type_t::integer:
        {
          auto cv = (std::dynamic_pointer_cast<home_system::io::device_int>(io))->get_value();
          lua_pushinteger(L, cv);
          break;
        }
        case home_system::io::io_data_type_t::double_float:
        {
          auto cv = (std::dynamic_pointer_cast<home_system::io::device_float>(io))->get_value();
          lua_pushnumber(L, cv);
          break;
        }
      }
      return 2;
    }
    else
    {
      return 1;
    }
  }
  catch (std::out_of_range)
  {
    // IO not found so state is unknown
    lua_pushnumber(L, static_cast<int>(home_system::io::io_state_t::unknown));
    return 1;
  }
}

int set_io_value(lua_State* L)
{
  std::string io_name = lua_tostring(L, 1);
  std::string rule_name = lua_tostring(L, 2);

  LOG(TRACE) << "Set IO value: IO: " << io_name << " from rule " << rule_name;

  try
  {
    auto io = _ios->get(io_name);
    
    // converting from proper type
    switch (io->get_data_type())
    {
      case home_system::io::io_data_type_t::integer:
      {
        auto v = static_cast<long long>(lua_tonumber(L, 2));
        (std::dynamic_pointer_cast<home_system::io::device_int>(io))->set_wanted_value(v);
        break;
      }
      case home_system::io::io_data_type_t::double_float:
      {
        auto v = static_cast<double>(lua_tonumber(L, 2));
        (std::dynamic_pointer_cast<home_system::io::device_float>(io))->set_wanted_value(v);
        break;
      }
    }
  }
  catch (std::out_of_range)
  {
    LOG(ERROR) << "Rule tried to set value of unknown IO device: " << io_name;
  }
  return 0;
}

int register_io(lua_State* L)
{
  static int id = 0;
  std::string io_name = lua_tostring(L, 1);
  home_system::io::io_data_type_t io_data_type = static_cast<home_system::io::io_data_type_t>(lua_tointeger(L, 2));
  std::string io_type = lua_tostring(L, 3);

  try
  {
    if (io_type == "weekly_schedule")
    {
      LOG(DEBUG) << "Creating weekly schedule IO: " << static_cast<int>(io_data_type) <<
        " \"" << io_name << "\"";

      if (io_data_type == home_system::io::io_data_type_t::integer)
      {
        typedef std::shared_ptr<home_system::io::device_int> device_int_t;
        device_int_t new_io = std::make_shared<home_system::io::device_int>(id++, io_type);

        typedef weekly_schedule<device_int_t, long long> weekly_sch_int_t;
        auto new_sch = std::make_shared<weekly_sch_int_t>(new_io);

        _ios->add_schedule(io_name, new_sch);
        _ios->add(io_name, new_io);
      }
      else if (io_data_type == home_system::io::io_data_type_t::double_float)
      {
      }
    }
    else if (io_type == "interval_schedule")
    {
      LOG(DEBUG) << "Creating interval schedule IO: " << static_cast<int>(io_data_type) <<
        " \"" << io_name << "\"";

      if (io_data_type == home_system::io::io_data_type_t::integer)
      {
        typedef std::shared_ptr<home_system::io::device_int> device_int_t;
        device_int_t new_io = std::make_shared<home_system::io::device_int>(id++, io_type);

        typedef interval_schedule<device_int_t, long long> interval_sch_int_t;
        auto new_sch = std::make_shared<interval_sch_int_t>(new_io);

        _ios->add_schedule(io_name, new_sch);
        _ios->add(io_name, new_io);
      }
      else if (io_data_type == home_system::io::io_data_type_t::double_float)
      {
      }
    }
    else
    {
      std::string io_service = lua_tostring(L, 4);
      home_system::io::io_id_t io_id = lua_tointeger(L, 5);
      home_system::io::io_mode_t io_mode = static_cast<home_system::io::io_mode_t>(lua_tointeger(L, 6));

      LOG(DEBUG) << "Creating remote IO: " << io_type << " " << static_cast<int>(io_data_type) <<
        " \"" << io_name << "\" " << io_service << ":" << io_id;

      home_system::io::device_t new_io;

      if (io_data_type == home_system::io::io_data_type_t::integer)
      {
        new_io = std::make_shared<home_system::io::device_int>(id++, io_type);
      }
      else if (io_data_type == home_system::io::io_data_type_t::double_float)
      {
        new_io = std::make_shared<home_system::io::device_float>(id++, io_type);
      }

      _ios->add_remote(io_service, io_id, new_io);
      _ios->add(io_name, new_io);
    }
  }
  catch (const std::runtime_error &e)
  {
    LOG(ERROR) << "Error while creating Remote IO: " << e.what();
  }

  return 0;
}

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

void ios::add_schedule(const std::string& name, schedule_t schedule)
{
  schedules_[name] = schedule;
}

void ios::add(const std::string& name, home_system::io::device_t device)
{
  io_devices_[name] = device;
}

home_system::io::device_t ios::get(const std::string &name)
{
  return io_devices_.at(name);
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

