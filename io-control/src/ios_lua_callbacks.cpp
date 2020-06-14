#include "ios_lua_callbacks.h"
#include "io/device_types.h"
#include "io/device_int.h"
#include "io/device_float.h"
#include "ios.h"
#include "weekly_schedule.h"
#include "interval_schedule.h"
#include "utils/logger.h"

extern ios_t _ios;

int get_io_state_value(lua_State* L)
{
  int rule_id = lua_tointeger(L, 1);
  home_system::io::io_id_t io_id = lua_tointeger(L, 2);

  LOG(TRACE) << "Get IO state and value: IO: " << io_id << " from rule " << rule_id;

  try
  {
    // get IO and its state
    auto io = _ios->get(io_id);
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
  int rule_id = lua_tointeger(L, 1);
  home_system::io::io_id_t io_id = lua_tointeger(L, 2);

  LOG(TRACE) << "Set IO value: IO: " << io_id << " from rule " << rule_id;

  try
  {
    auto io = _ios->get(io_id);
    
    // converting from proper type
    switch (io->get_data_type())
    {
      case home_system::io::io_data_type_t::integer:
      {
        auto v = static_cast<long long>(lua_tonumber(L, 3));
        (std::dynamic_pointer_cast<home_system::io::device_int>(io))->set_wanted_value(v);
        break;
      }
      case home_system::io::io_data_type_t::double_float:
      {
        auto v = static_cast<double>(lua_tonumber(L, 3));
        (std::dynamic_pointer_cast<home_system::io::device_float>(io))->set_wanted_value(v);
        break;
      }
    }
  }
  catch (std::out_of_range)
  {
    LOG(ERROR) << "Rule tried to set value of unknown IO device: " << io_id;
  }
  return 0;
}

int register_io(lua_State* L)
{
  try
  {
    int id = lua_tointeger(L, 1);
    std::string io_name = lua_tostring(L, 2);
    home_system::io::io_data_type_t io_data_type = static_cast<home_system::io::io_data_type_t>(lua_tointeger(L, 3));
    std::string io_type = lua_tostring(L, 4);
    std::string io_service = lua_tostring(L, 5);
    home_system::io::io_id_t io_remote_id = lua_tointeger(L, 6);
    home_system::io::io_mode_t io_mode = static_cast<home_system::io::io_mode_t>(lua_tointeger(L, 7));

    LOG(DEBUG) << "Creating remote IO: " << io_type << " " << static_cast<int>(io_data_type) <<
      " \"" << io_name << "\" " << io_service << ":" << io_remote_id;

    home_system::io::device_t new_device;

    if (io_data_type == home_system::io::io_data_type_t::integer)
    {
      new_device = std::make_shared<home_system::io::device_int>(id, io_type);
    }
    else if (io_data_type == home_system::io::io_data_type_t::double_float)
    {
      new_device = std::make_shared<home_system::io::device_float>(id, io_type);
    }

    _ios->add_remote_io(io_service, io_remote_id, new_device);
    _ios->add(id, new_device, io_name);
  }
  catch (const std::runtime_error &e)
  {
    LOG(ERROR) << "Error while creating Remote IO: " << e.what();
  }

  return 0;
}

typedef std::shared_ptr<home_system::io::device_int> device_int_t;
typedef interval_schedule<device_int_t, long long> interval_sch_int_t;

typedef std::shared_ptr<home_system::io::device_float> device_float_t;
typedef interval_schedule<device_float_t, double> interval_sch_float_t;

int register_interval_schedule(lua_State* L)
{
  try
  {
    int id = lua_tointeger(L, 1);
    std::string io_name = lua_tostring(L, 2);
    home_system::io::io_data_type_t io_data_type = static_cast<home_system::io::io_data_type_t>(lua_tointeger(L, 3));
    int interval = lua_tointeger(L, 4);
  
    LOG(DEBUG) << "Creating interval schedule IO: " << static_cast<int>(io_data_type) <<
      " \"" << io_name << "\"";

    if (io_data_type == home_system::io::io_data_type_t::integer)
    {
      auto new_device = std::make_shared<home_system::io::device_int>(id, "interval_schedule");
      auto new_sch = std::make_shared<interval_sch_int_t>(new_device, interval);

      _ios->add_interval_schedule(id, new_sch);
      _ios->add(id, new_device, io_name);
    }
    else if (io_data_type == home_system::io::io_data_type_t::double_float)
    {
      auto new_device = std::make_shared<home_system::io::device_float>(id, "interval_schedule");
      auto new_sch = std::make_shared<interval_sch_float_t>(new_device, interval);

      _ios->add_interval_schedule(id, new_sch);
      _ios->add(id, new_device, io_name);
    }
  }
  catch (const std::runtime_error &e)
  {
    LOG(ERROR) << "Error while creating interval schedule: " << e.what();
  }

  return 0;
}

int add_value_to_interval_schedule(lua_State* L)
{
  try
  {
    int id = lua_tointeger(L, 1);

    auto schedule = _ios->get_interval_schedule(id);

    if (schedule->get_device()->get_data_type() == home_system::io::io_data_type_t::integer)
    {
      long long value = lua_tointeger(L, 2);
      auto interval_sch = std::dynamic_pointer_cast<interval_sch_int_t>(schedule);
      interval_sch->add_value(value);
    }
    else if (schedule->get_device()->get_data_type() == home_system::io::io_data_type_t::double_float)
    {
      double value = lua_tonumber(L, 2);
      auto interval_sch = std::dynamic_pointer_cast<interval_sch_float_t>(schedule);
      interval_sch->add_value(value);
    }
  }
  catch (const std::runtime_error &e)
  {
    LOG(ERROR) << "Error while adding value to interval schedule: " << e.what();
  }

  return 0;
}

typedef weekly_schedule<device_int_t, long long> weekly_sch_int_t;
typedef weekly_schedule<device_float_t, double> weekly_sch_float_t;

int register_weekly_schedule(lua_State* L)
{
  try
  {
    int id = lua_tointeger(L, 1);
    std::string io_name = lua_tostring(L, 2);
    home_system::io::io_data_type_t io_data_type = static_cast<home_system::io::io_data_type_t>(lua_tointeger(L, 3));

    LOG(DEBUG) << "Creating weekly schedule IO: " << static_cast<int>(io_data_type) <<
      " \"" << io_name << "\"";

    if (io_data_type == home_system::io::io_data_type_t::integer)
    {
      auto new_device = std::make_shared<home_system::io::device_int>(id, "weekly_schedule");
      auto new_sch = std::make_shared<weekly_sch_int_t>(new_device);

      _ios->add_weekly_schedule(id, new_sch);
      _ios->add(id, new_device, io_name);
    }
    else if (io_data_type == home_system::io::io_data_type_t::double_float)
    {
      auto new_device = std::make_shared<home_system::io::device_float>(id, "weekly_schedule");
      auto new_sch = std::make_shared<weekly_sch_float_t>(new_device);

      _ios->add_weekly_schedule(id, new_sch);
      _ios->add(id, new_device, io_name);
    }
  }
  catch (const std::runtime_error &e)
  {
    LOG(ERROR) << "Error while creating Remote IO: " << e.what();
  }

  return 0;
}

int add_trigger_to_weekly_schedule(lua_State* L)
{
  try
  {
    int id = lua_tointeger(L, 1);

    auto schedule = _ios->get_weekly_schedule(id);

    int day = lua_tointeger(L, 2);
    int hour = lua_tointeger(L, 3);
    int minute = lua_tointeger(L, 4);
    int second = lua_tointeger(L, 5);

    if (schedule->get_device()->get_data_type() == home_system::io::io_data_type_t::integer)
    {
      long long value = lua_tointeger(L, 6);
      auto weekly_sch = std::dynamic_pointer_cast<weekly_sch_int_t>(schedule);
      weekly_sch->add_trigger(day, hour, minute, second, value);
    }
    else if (schedule->get_device()->get_data_type() == home_system::io::io_data_type_t::double_float)
    {
      double value = lua_tonumber(L, 6);
      auto weekly_sch = std::dynamic_pointer_cast<weekly_sch_float_t>(schedule);
      weekly_sch->add_trigger(day, hour, minute, second, value);
    }
  }
  catch (const std::runtime_error &e)
  {
    LOG(ERROR) << "Error while adding value to interval schedule: " << e.what();
  }

  return 0;
}