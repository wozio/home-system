#pragma once

extern "C" {
#include <lua5.3/lua.h>
#include <lua5.3/lauxlib.h>
#include <lua5.3/lualib.h>
}

#include "io/device.h"
#include "utils/ios_wrapper.h"
#include <boost/signals2.hpp>
#include <memory>
#include <string>
#include <vector>

class rule
{
public:
    rule(lua_State *lua, const int id, const char* name, const char* exec_func,
      home_system::utils::ios_wrapper& ios);
    ~rule();

    void enable();
    void disable();

    void add_trigger(home_system::io::io_id_t trigger_id);
private:

    void exec(home_system::io::io_id_t trigger_id);

    const int id_;
    const char* name_;
    const char* exec_func_;
    bool enabled_;
    home_system::utils::ios_wrapper& ios_;

    lua_State *lua_;
    std::vector<char> chunk_;
    std::vector<boost::signals2::connection> trigger_connections_;

    int get_io_state_value();
    int set_io_value();
};
