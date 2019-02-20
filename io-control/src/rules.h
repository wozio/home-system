#pragma once

#include "rule.h"
#include <map>

class rules;

typedef std::unique_ptr<rules> rules_t;

class rules
{
public:
  static rules_t create(lua_State *lua)
  {
    return rules_t(new rules(lua));
  }
  rules(lua_State *lua);
  ~rules();

  void init();

  int register_rule();
  int add_trigger();

private:
  home_system::utils::ios_wrapper ios_;

  lua_State *lua_;

  // rules are keyed by name
  typedef std::shared_ptr<rule> rule_t;
  std::map<std::string, rule_t> rules_;
};
