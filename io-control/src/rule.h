#pragma once

extern "C" {
#include <lua5.1/lua.h>
#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>
}

#include <boost/signals2.hpp>
#include <memory>
#include <string>
#include <vector>

class rule;

typedef std::shared_ptr<rule> rule_t;

class rule
{
public:
    rule(const std::string& name,
        const std::string& script,
        const std::vector<std::string>& triggers);
    ~rule();

    void enable();
    void disable();
private:

    void exec();

    std::string name_;
    bool enabled_;
    lua_State *lua_;
    std::vector<char> chunk_;
    std::vector<boost::signals2::connection> trigger_connections_;

};
