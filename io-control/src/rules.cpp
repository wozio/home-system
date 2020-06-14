#include "utils/config.h"
#include "utils/logger.h"
#include "rules.h"
#include "ios.h"

extern ios_t _ios;
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
  int rule_id = lua_tointeger(lua_, 1);
  const char *name = luaL_checkstring(lua_, 2);
  const char *exec_func = luaL_checkstring(lua_, 3);
  // TODO some error handling

  LOG(INFO) << "Register rule: " << rule_id << " name: '" << name << "' exec_func: '" << exec_func << "'";

  rules_[rule_id] = std::make_shared<rule>(lua_, rule_id, name, exec_func, ios_);
  return 0;
}

int rules::add_trigger()
{
  int rule_id = lua_tointeger(lua_, 1);
  const std::string trigger = luaL_checkstring(lua_, 2);

  home_system::io::io_id_t trigger_id = _ios->get(trigger);
  // TODO some error handling

  LOG(INFO) << "Add trigger: " << trigger << "[" << trigger_id << "] to rule: " << rule_id;

  // TODO range checking
  rules_[rule_id]->add_trigger(trigger_id);
  return 0;
}

rules::~rules()
{
}
