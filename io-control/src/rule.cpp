#include "utils/logger.h"
#include "rule.h"
#include "ios.h"
#include "io/device_int.h"
#include "io/device_float.h"

extern ios_t _ios;

rule::rule(const std::string& name,
    const std::string& script_file,
    const std::string& script,
    const std::vector<std::string>& triggers,
    home_system::utils::ios_wrapper& ios)
    : name_(name),
      enabled_(true),
      ios_(ios)
{
    LOG(INFO) << "Creating '" << name << "' rule";

    // registering for callbacks in triggers
    // each time trigger changes its value it calls this callback
    for (const auto& trigger : triggers)
    {
      
      try
      {
        auto t = _ios->get(trigger);
        LOG(TRACE) << "Rule '" << name_ << "' registering for trigger '" << trigger << "':" << t->get_id();
        boost::signals2::connection c = t->on_value_change.connect([this] (home_system::io::io_id_t io)
        {
          LOG(TRACE) << "Rule '" << name_ << "' triggered from " << io;
          // rule is executing in separate thread
          ios_.io_service().post([this]()
          {
            try
            {
              exec();
            }
            catch (const std::exception& e)
            {
              LOG(WARNING) << "EXCEPTION thrown while running exec function: " << e.what();
            }
          });
        });
        trigger_connections_.push_back(c);
      }
      catch (const std::out_of_range& e)
      {
        LOG(ERROR) << "In rule '" << name_ << "' trigger is not defined: " << trigger;
      }
    }

    // initializing LUA
    lua_ = lua_open();
    luaL_openlibs(lua_);

    // loading script
    if (!script_file.empty())
    {
        if (luaL_loadfile(lua_, script_file.c_str()))
        {
            LOG(ERROR) << "In rule '" << name_ << "' error while loading rule script file: " << script_file << ": " << lua_tostring(lua_, -1);
            lua_pop(lua_, 1);
            lua_close(lua_);
            throw std::runtime_error("Error loading rule script");
        }
    }
    else if (luaL_loadbuffer(lua_, script.c_str(), script.length(), name.c_str()))
    {
        LOG(ERROR) << "In rule '" << name_ << "' error while loading rule script: " << lua_tostring(lua_, -1);
        lua_pop(lua_, 1);
        lua_close(lua_);
        throw std::runtime_error("Error loading rule script");
    }

    // push callbacks
    // get_io_state_value
    lua_pushcfunction(lua_, [](lua_State *L)->int{

        // get IO name
        std::string io_name = lua_tostring(L, 1);
        
        try
        {
            // get IO and its state
            auto io = _ios->get(io_name);
            auto s = io->get_state();
            // state is always returned
            lua_pushnumber(L, static_cast<int>(s));
            /// value is returned only when state is OK
            if (s == home_system::io::io_state_t::ok)
            {
                // converting to proper type
                switch (io->get_data_type())
                {
                    case home_system::io::io_data_type_t::integer:
                    {
                        auto cv = (std::dynamic_pointer_cast<home_system::io::device_int>(io))->get_value();
                        lua_pushinteger(L, cv);
                        break;
                    }
                    case home_system::io::io_data_type_t::double_float:
                    {
                        auto cv = (std::dynamic_pointer_cast<home_system::io::device_float>(io))->get_value();
                        lua_pushnumber(L, cv);
                        break;
                    }
                }
                return 2;
            }
            else
            {
                return 1;
            }
        }
        catch (std::out_of_range)
        {
            // IO not found so state is unknown
            lua_pushnumber(L, static_cast<int>(home_system::io::io_state_t::unknown));
            return 1;
        }
        
    });
    lua_setglobal(lua_, "get_io_state_value");

    // set_io_value
    lua_pushcfunction(lua_, [](lua_State *L)->int
    {
      // get IO name
      std::string io_name = lua_tostring(L, 1);
      
      try
      {
        auto io = _ios->get(io_name);
        
        // converting from proper type
        switch (io->get_data_type())
        {
          case home_system::io::io_data_type_t::integer:
          {
            auto v = static_cast<long long>(lua_tonumber(L, 2));
            (std::dynamic_pointer_cast<home_system::io::device_int>(io))->set_wanted_value(v);
            break;
          }
          case home_system::io::io_data_type_t::double_float:
          {
            auto v = static_cast<double>(lua_tonumber(L, 2));
            (std::dynamic_pointer_cast<home_system::io::device_float>(io))->set_wanted_value(v);
            break;
          }
        }
      }
      catch (std::out_of_range)
      {
        LOG(ERROR) << "Rule tried to set value of unknown IO device: " << io_name;
      }
      return 0;
    });
    lua_setglobal(lua_, "set_io_value");

    // priming run
    if (lua_pcall(lua_, 0, 0, 0))
    {
        LOG(ERROR) << "In rule '"<< name_ << "' error in priming call: " << lua_tostring(lua_, -1);
        lua_pop(lua_, 1);
        throw std::runtime_error("Error in priming call");
    }
}

rule::~rule()
{
    LOG(DEBUG) << "Destroying rule '"<< name_ << "'";
    for (auto& c : trigger_connections_)
    {
        c.disconnect();
    }
    lua_close(lua_);
}

void rule::exec()
{
    if (enabled_)
    {
        lua_getglobal(lua_, "exec");
        if (lua_pcall(lua_, 0, 0, 0))
        {
            LOG(ERROR) << "In rule '" << name_ << "' error running exec function: " << lua_tostring(lua_, -1);
            lua_pop(lua_, 1);
            throw std::runtime_error("Error running rule script");
        }
    }
}
