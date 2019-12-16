#include "utils/logger.h"
#include "rule.h"
#include "ios.h"


extern ios_t _ios;

#define RLOG(level) LOG(level) << "Rule [" << id_ << "]: "

rule::rule(lua_State *lua, const int id, const char* name, const char* exec_func, home_system::utils::ios_wrapper& ios)
: lua_(lua),
  id_(id),
  name_(name),
  exec_func_(exec_func),
  ios_(ios)
{
  RLOG(INFO) << "Creating rule name: '" << name << "' exec_func: '" << exec_func << "'";
}

void rule::add_trigger(home_system::io::io_id_t trigger_id)
{
  try
  {
    auto t = _ios->get(trigger_id);
    RLOG(TRACE) << "registering for trigger: " << trigger_id;
    boost::signals2::connection c = t->on_value_change.connect([this] (home_system::io::io_id_t trigger_id_)
    {
      RLOG(TRACE) << "triggered from " << trigger_id_;
      // rule is executing in separate thread
      ios_.io_service().post([this, trigger_id_]()
      {
        try
        {
          exec(trigger_id_);
        }
        catch (const std::exception& e)
        {
          RLOG(WARNING) << "EXCEPTION thrown while running exec function: " << e.what();
        }
      });
    });
    trigger_connections_.push_back(c);
  }
  catch (const std::out_of_range& e)
  {
    RLOG(ERROR) << "trigger is not defined: " << trigger_id;
  }
}

rule::~rule()
{
  RLOG(DEBUG) << "Destroying rule";
  for (auto& c : trigger_connections_)
  {
    c.disconnect();
  }
}

void rule::exec(home_system::io::io_id_t trigger_id)
{
  if (enabled_)
  {
    lua_getglobal(lua_, exec_func_);
    
    lua_pushnumber(lua_, id_);
    lua_pushnumber(lua_, trigger_id);
    // TODO pass also current trigger value
    if (lua_pcall(lua_, 2, 0, 0))
    {
      std::string e(lua_tostring(lua_, -1));
      RLOG(ERROR) << "Error running exec function: '" << exec_func_ << "' error: " << e;
      lua_pop(lua_, 1);
      throw std::runtime_error("Error running rule script");
    }
  }
}
