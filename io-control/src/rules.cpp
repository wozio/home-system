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
  ios_(4)
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

#if 0

    LOG(INFO) << "Reading configuration";
    auto &conf = CONFIG.get();
    auto i = conf.FindMember("rules");
    if (i != conf.MemberEnd() && i->value.IsArray())
    {
        const auto& arr = i->value;
        for (auto j = arr.Begin(); j != arr.End(); ++j)
        {
            if (j->IsObject())
            {
                // getting rule name
                std::string name;
                const auto& v = j->FindMember("name");
                if (v != j->MemberEnd() && v->value.IsString())
                {
                    name = v->value.GetString();
                }
                else
                {
                    LOG(WARNING) << "Rule name not found or incorrect, ignoring";
                    continue;
                }

                // getting rule script
                std::string rule_script_file;
                std::string rule_script;
                const auto& vs = j->FindMember("rule_file");
                if (vs != j->MemberEnd() && vs->value.IsString())
                {
                    rule_script_file = vs->value.GetString();
                }
                else
                {
                    const auto& vs = j->FindMember("rule");
                    if (vs != j->MemberEnd() && vs->value.IsString())
                    {
                        rule_script = vs->value.GetString();
                    }
                    else
                    {
                        LOG(WARNING) << "Rule script not found or incorrect, ignoring rule";
                        continue;
                    }
                }

                // getting triggers
                std::vector<std::string> triggers;
                const auto& ta = j->FindMember("triggers");
                if (ta != j->MemberEnd() && ta->value.IsArray())
                {
                    for (auto ti = ta->value.Begin(); ti != ta->value.End(); ++ti)
                    {
                        if (ti->IsString())
                        {
                            triggers.push_back(ti->GetString());
                        }
                    }
                }
                try
                {
                    rules_[name] = std::make_shared<rule>(name, rule_script_file, rule_script, triggers, ios_);
                }
                catch (const std::exception e)
                {
                    LOG(ERROR) << "Error creating rule";
                }
            }
        }
    }
}

#endif

rules::~rules()
{
}
