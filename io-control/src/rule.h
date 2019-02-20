#pragma once

extern "C" {
#include <lua5.3/lua.h>
#include <lua5.3/lauxlib.h>
#include <lua5.3/lualib.h>
}

#include "utils/ios_wrapper.h"
#include <boost/signals2.hpp>
#include <memory>
#include <string>
#include <vector>

class rule
{
public:
    rule(const std::string& name,
        const std::string& script_file,
        const std::string& script,
        const std::vector<std::string>& triggers,
        home_system::utils::ios_wrapper& ios);
    rule(lua_State *lua, const char* name, home_system::utils::ios_wrapper& ios);
    ~rule();

    void init();

    void enable();
    void disable();

    void add_trigger(const char* trigger);
private:

    void exec();

    const char* name_;
    bool enabled_;
    home_system::utils::ios_wrapper& ios_;

    lua_State *lua_;
    std::vector<char> chunk_;
    std::vector<boost::signals2::connection> trigger_connections_;

    int get_io_state_value();
    int set_io_value();
};
