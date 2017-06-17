#include "utils/logger.h"
#include "rule.h"
#include "ios.h"
extern "C" {
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>
}

extern ios_t _ios;

rule::rule(const std::string& name,
    const std::string& script,
    const std::vector<std::string>& triggers)
{
    LOG(INFO) << "Creating '" << name << "' rule";

    // registering for callbacks in triggers
    for (const auto& trigger : triggers)
    {
        try
        {
            auto t = _ios->get(trigger);
            t->on_value_state_change.connect([this] (io_t io){
                LOG(DEBUG) << "Triggered from \"" << io->get_name() << '"';
            });
        }
        catch (const std::out_of_range& e)
        {
            LOG(ERROR) << "Trigger is not defined: " << trigger;
        }
    }

    lua_State *L = lua_open();
    luaL_openlibs(L);

    if (luaL_loadbuffer(L, script.c_str(), script.length(), name.c_str()))
    {
        LOG(ERROR) << "Error while loading rule script: " << lua_tostring(L, -1);
        lua_pop(L, 1);
        lua_close(L);
        throw std::runtime_error("Error while creating rule");
    }
    if (lua_pcall(L, 0, 0, 0))
    {
        LOG(ERROR) << "Error while running rule script: " << lua_tostring(L, -1);
        lua_pop(L, 1);
        lua_close(L);
        throw std::runtime_error("Error while creating rule");
    }

    lua_close(L);
}

rule::~rule()
{

}

void rule::exec()
{

}
