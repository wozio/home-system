#include "utils/logger.h"
#include "rule.h"
#include "ios.h"


extern ios_t _ios;

rule::rule(lua_State *lua, const int id, const char* name, const char* exec_func, home_system::utils::ios_wrapper& ios)
: lua_(lua),
  id_(id),
  name_(name),
  exec_func_(exec_func),
  ios_(ios)
{
  LOG(INFO) << "Creating rule: " << id_ << " name: '" << name << "' exec_func: '" << exec_func << "'";
}

void rule::add_trigger(home_system::io::io_id_t trigger_id)
{
  try
  {
    auto t = _ios->get(trigger_id);
    LOG(TRACE) << "Rule: " << id_ << " registering for trigger: " << trigger_id;
    boost::signals2::connection c = t->on_value_change.connect([this] (home_system::io::io_id_t io)
    {
      LOG(TRACE) << "Rule: " << id_ << " triggered from " << io;
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
    LOG(ERROR) << "In rule: " << id_ << " trigger is not defined: " << trigger_id;
  }
}

rule::~rule()
{
  LOG(DEBUG) << "Destroying rule: "<< id_;
  for (auto& c : trigger_connections_)
  {
    c.disconnect();
  }
}

void rule::exec()
{
  if (enabled_)
  {
    lua_getglobal(lua_, exec_func_);
    if (lua_pcall(lua_, 0, 0, 0))
    {
      std::string e(lua_tostring(lua_, -1));
      LOG(ERROR) << "In rule: " << id_ << " error running exec function: '" << exec_func_ << "' error: " << e;
      lua_pop(lua_, 1);
      throw std::runtime_error("Error running rule script");
    }
  }
}
