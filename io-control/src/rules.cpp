#include "utils/config.h"
#include "utils/logger.h"
#include "rules.h"

extern rules_t _rules;

int register_rule(lua_State* L)
{
  return _rules->register_rule();
}

int add_trigger(lua_State* L)
{
  return _rules->add_trigger();
}

rules::rules(lua_State *lua)
: lua_(lua),
  ios_()
{
  // callbacks definition
  lua_pushcfunction(lua_, ::register_rule);
  lua_setglobal(lua_, "register_rule");
  lua_pushcfunction(lua_, ::add_trigger);
  lua_setglobal(lua_, "add_trigger");
}

void rules::init()
{
  // register_rules runs register_rule callback for each rule that has to be registered
  lua_getglobal(lua_, "register_rules");
  if (lua_pcall(lua_, 0, 0, 0))
  {
    LOG(ERROR) << "Error running register_rules function: " << lua_tostring(lua_, -1);
    lua_pop(lua_, 1);
    throw std::runtime_error("Error running rule script");
  }
}

int rules::register_rule()
{
  const char *name = luaL_checkstring(lua_, 1);
  // TODO some error handling

  LOG(INFO) << "Register rule: '" << name << "'";

  rules_[name] = std::make_shared<rule>(lua_, name, ios_);
  return 0;
}

int rules::add_trigger()
{
  const char *name = luaL_checkstring(lua_, 1);
  const char *trigger = luaL_checkstring(lua_, 2);
  // TODO some error handling

  LOG(INFO) << "Add trigger to rule: '" << name << "': '" << trigger << "'";

  // TODO name checking
  rules_[name]->add_trigger(trigger);
  return 0;
}

rules::~rules()
{
}
