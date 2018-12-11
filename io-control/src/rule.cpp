#include "utils/logger.h"
#include "rule.h"
#include "ios.h"
#include "io/device_int.h"
#include "io/device_float.h"

extern ios_t _ios;

rule::rule(const std::string& name,
    const std::string& script_file,
    const std::string& script,
    const std::vector<std::string>& triggers,
    home_system::utils::ios_wrapper& ios)
    : name_(name.c_str()),
      enabled_(true),
      ios_(ios)
{
  LOG(INFO) << "Creating '" << name << "' rule";

  // registering for callbacks in triggers
  // each time trigger changes its value it calls this callback
  for (const auto& trigger : triggers)
  {
    
    try
    {
      auto t = _ios->get(trigger);
      LOG(TRACE) << "Rule '" << name_ << "' registering for trigger '" << trigger << "':" << t->get_id();
      boost::signals2::connection c = t->on_value_change.connect([this] (home_system::io::io_id_t io)
      {
        LOG(TRACE) << "Rule '" << name_ << "' triggered from " << io;
        // rule is executing in separate thread
        ios_.io_service().post([this]()
        {
          try
          {
            exec();
          }
          catch (const std::exception& e)
          {
            LOG(WARNING) << "EXCEPTION thrown while running exec function: " << e.what();
          }
        });
      });
      trigger_connections_.push_back(c);
    }
    catch (const std::out_of_range& e)
    {
      LOG(ERROR) << "In rule '" << name_ << "' trigger is not defined: " << trigger;
    }
  }
}

rule::rule(lua_State *lua, const char* name, home_system::utils::ios_wrapper& ios)
: lua_(lua),
  name_(name),
  ios_(ios)
{
  LOG(INFO) << "Creating '" << name << "' rule";
}

void rule::init()
{
  std::string fun("register_triggers_for_");
  fun.append(name_);
  // register_triggers_for_rule maps IO triggers to rule, rule is executed when IO is changed
  lua_getglobal(lua_, fun.c_str());
  if (lua_pcall(lua_, 0, 0, 0))
  {
    LOG(ERROR) << "Rule: " << name_ << ": Error running " << fun << " function: " << lua_tostring(lua_, -1);
    lua_pop(lua_, 1);
    throw std::runtime_error("Error running rule script");
  }
}

rule::~rule()
{
    LOG(DEBUG) << "Destroying rule '"<< name_ << "'";
    for (auto& c : trigger_connections_)
    {
        c.disconnect();
    }
    lua_close(lua_);
}

void rule::exec()
{
    if (enabled_)
    {
        lua_getglobal(lua_, "exec");
        if (lua_pcall(lua_, 0, 0, 0))
        {
            LOG(ERROR) << "In rule '" << name_ << "' error running exec function: " << lua_tostring(lua_, -1);
            lua_pop(lua_, 1);
            throw std::runtime_error("Error running rule script");
        }
    }
}
