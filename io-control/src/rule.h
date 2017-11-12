#pragma once

extern "C" {
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>
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
    ~rule();

    void enable();
    void disable();
private:

    void exec();

    std::string name_;
    bool enabled_;
    home_system::utils::ios_wrapper& ios_;

    lua_State *lua_;
    std::vector<char> chunk_;
    std::vector<boost::signals2::connection> trigger_connections_;
};
