#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/config.h"
#include "utils/logger.h"
#include "io/device_types.h"
#include "io/device_int.h"
#include "io/device_float.h"
#include "ios.h"
#include "weekly_schedule.h"

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
  std::string io_name = lua_tostring(L, 1);
  home_system::io::io_data_type_t io_data_type = static_cast<home_system::io::io_data_type_t>(lua_tointeger(L, 2));
  std::string io_type = lua_tostring(L, 3);
  std::string io_service = lua_tostring(L, 4);
  home_system::io::io_id_t io_id = lua_tointeger(L, 5);
  home_system::io::io_mode_t io_mode = static_cast<home_system::io::io_mode_t>(lua_tointeger(L, 6));

  try
  {
    LOG(DEBUG) << "Creating Remote IO: " << static_cast<int>(io_data_type) <<
      " \"" << io_name << "\" " << io_service << ":" << io_id;

    home_system::io::device_t new_io;
    if (io_data_type == home_system::io::io_data_type_t::integer)
    {
      new_io = std::make_shared<home_system::io::device_int>(io_id, io_type);
    }
    else if (io_data_type == home_system::io::io_data_type_t::double_float)
    {
      new_io = std::make_shared<home_system::io::device_float>(io_id, io_type);
    }

    _ios->add(io_name, io_service, io_id, new_io);
    
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

#if 0
  LOG(INFO) << "Reading configuration";
  auto &conf = CONFIG.get();
  // first create known (written in configuration) io devices
  if (conf.HasMember("ios"))
  {
    auto &a = conf["ios"];
    for (auto itr = a.Begin(); itr != a.End(); ++itr)
    {
      if (itr->IsObject())
      {
        home_system::io::io_data_type_t data_type;
        std::string name;
        std::string type;
        std::string service;
        long long id;
        home_system::io::io_mode_t mode;

        if (itr->HasMember("type"))
        {
          auto &v = (*itr)["type"];
          if (v.IsString())
          {
            type = v.GetString();
          }
        }
        else
        {
          LOG(WARNING) << "IO definition without mandatory field 'type', ignoring...";
          continue;
        }

        if (itr->HasMember("data_type"))
        {
          auto &v = (*itr)["data_type"];
          if (v.IsInt())
          {
            data_type = static_cast<home_system::io::io_data_type_t>(v.GetInt());
          }
        }
        else
        {
          LOG(WARNING) << "IO definition without mandatory field 'data_type', ignoring...";
          continue;
        }

        if (itr->HasMember("name"))
        {
          auto &v = (*itr)["name"];
          if (v.IsString())
          {
            name = v.GetString();
          }
        }
        else
        {
          LOG(WARNING) << "IO definition without mandatory field 'name', ignoring...";
          continue;
        }

        if (type == "weekly_schedule")
        {
          if (itr->HasMember("triggers"))
          {
            auto& triggers = (*itr)["triggers"];
            std::map<std::string, long long> triggers_map;
            for (auto iitr = triggers.Begin(); iitr != triggers.End(); ++iitr)
            {
              if (iitr->IsObject())
              {
                if (iitr->HasMember("time") && iitr->HasMember("value"))
                {
                  auto t = ((*iitr)["time"]).GetString();
                  auto &v = (*iitr)["value"];
                  switch (data_type)
                  {
                  case home_system::io::io_data_type_t::integer:
                    triggers_map[t] = v.GetInt64();
                    break;
                  case home_system::io::io_data_type_t::double_float:
                    triggers_map[t] = v.GetDouble();
                    break;
                  }
                }
              }
            }
            if (triggers_map.size() > 0)
            {
              try
              {
                LOG(DEBUG) << "Creating weekly schedule IO: " << static_cast<int>(data_type) << " '" << name << "'";
                auto new_io = std::make_shared<home_system::io::device_int>(id, type);
                typedef weekly_schedule<home_system::io::device_int, long long> weekly_sch_int_t;
                auto new_sch = std::make_shared<weekly_sch_int_t>(new_io, triggers_map);
                schedules_.push_back(new_sch);
                io_devices_[name] = new_io;
              }
              catch (const std::runtime_error &e)
              {
                LOG(ERROR) << "Error while creating Weekly Schedule: " << e.what();
              }
            }
            else
            {
              LOG(WARNING) << "Weekly schedule definition without any valid trigger, ignoring";
            }
          }
          else
          {
            LOG(WARNING) << "Weekly schedule definition without triggers object, ignoring";
          }
        }
        else
        {
          if (itr->HasMember("mode"))
          {
            auto &v = (*itr)["mode"];
            if (v.IsInt())
            {
              mode = static_cast<home_system::io::io_mode_t>(v.GetInt());
            }
          }
          else
          {
            mode = home_system::io::io_mode_t::input;
          }
          
          if (itr->HasMember("service"))
          {
            auto &v = (*itr)["service"];
            if (v.IsString())
            {
              service = v.GetString();
            }
          }

          if (itr->HasMember("id"))
          {
            auto &v = (*itr)["id"];
            if (v.IsInt64())
            {
              id = v.GetInt64();
            }
          }
          if (name.length() > 0 && type.length() > 0 && service.length() > 0)
          {
            try
            {
              LOG(DEBUG) << "Creating Remote IO: " << static_cast<int>(data_type) <<
                " \"" << name << "\" " << service << ":" << id;
              home_system::io::device_t new_io;
              if (data_type == home_system::io::io_data_type_t::integer)
              {
                new_io = std::make_shared<home_system::io::device_int>(id, type);
              }
              else if (data_type == home_system::io::io_data_type_t::double_float)
              {
                new_io = std::make_shared<home_system::io::device_float>(id, type);
              }
              io_devices_[name] = new_io;
              io_devices_by_service_[service][id] = new_io;
            }
            catch (const std::runtime_error &e)
            {
              LOG(ERROR) << "Error while creating Remote IO: " << e.what();
            }
          }
          else
          {
            LOG(WARNING) << "Remote IO definition without mandatory field, ignoring...";
            continue;
          }
        }
      }
    }
  }
#endif
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

void ios::add(const std::string& name, const std::string& service,
  home_system::io::io_id_t id, home_system::io::device_t device)
{
  io_devices_[name] = device;
  io_devices_by_service_[service][id] = device;
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
}

