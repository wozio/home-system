#pragma once
extern "C" {
#include <lua5.3/lua.h>
#include <lua5.3/lauxlib.h>
#include <lua5.3/lualib.h>
}


int get_io_state_value(lua_State* L);

int set_io_value(lua_State* L);

int register_io(lua_State* L);

int register_interval_schedule(lua_State* L);
int add_value_to_interval_schedule(lua_State* L);

int register_weekly_schedule(lua_State* L);
int add_trigger_to_weekly_schedule(lua_State* L);
