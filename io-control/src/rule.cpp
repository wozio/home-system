#include "utils/logger.h"
#include "rule.h"
#include "ios.h"

extern ios_t _ios;

rule::rule(const std::string& name,
    const std::string& script,
    const std::vector<std::string>& triggers)
    : name_(name),
      enabled_(false)
{
    LOG(INFO) << "Creating '" << name << "' rule";

    // registering for callbacks in triggers
    for (const auto& trigger : triggers)
    {
        try
        {
            auto t = _ios->get(trigger);
            boost::signals2::connection c = t->on_value_state_change.connect([this] (io_t io){
                LOG(DEBUG) << "Triggered from \"" << io->get_name() << '"';
                exec();
            });
            trigger_connections_.push_back(c);
        }
        catch (const std::out_of_range& e)
        {
            LOG(ERROR) << "Trigger is not defined: " << trigger;
        }
    }

    // initializing LUA
    lua_ = lua_open();
    luaL_openlibs(lua_);

    // loading script
    if (luaL_loadbuffer(lua_, script.c_str(), script.length(), name.c_str()))
    {
        LOG(ERROR) << "Error while loading rule script: " << lua_tostring(lua_, -1);
        lua_pop(lua_, 1);
        lua_close(lua_);
        throw std::runtime_error("Error loading rule script");
    }

    // get compiled chunk and save for further use
    // it is not possible to convert lambda to function pointer
    // if it captures something so passing chunk as a user data pointer
    if (lua_dump(lua_, [] (lua_State *L, const void* p,
        size_t sz, void* ud) -> int{
            auto chunk = (std::vector<char>*)ud;
            chunk->insert(chunk->end(), (const char*)p, (const char*)p + sz);
            return 0;
        }, (void *)&chunk_))
    {
        LOG(ERROR) << "Error while loading compiled chunk: " << lua_tostring(lua_, -1);
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
                auto v = io->get_value();
                // converting to proper type
                switch (io->get_data_type())
                {
                    case home_system::io::io_data_type_t::integer:
                    {
                        auto cv = boost::any_cast<long long>(v);
                        lua_pushinteger(L, cv);
                        break;
                    }
                    case home_system::io::io_data_type_t::double_float:
                    {
                        auto cv = boost::any_cast<double>(v);
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
    lua_pushcfunction(lua_, [](lua_State *L)->int{

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
                    io->set_wanted_value(v);
                    break;
                }
                case home_system::io::io_data_type_t::double_float:
                {
                    auto v = static_cast<double>(lua_tonumber(L, 2));
                    io->set_wanted_value(v);
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
}

rule::~rule()
{
    LOG(DEBUG) << "Destroying rule \"" << name_ << "\"";
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
        // load compiled chunk
        if (luaL_loadbuffer(lua_, chunk_.data(), chunk_.size(), name_.c_str()))
        {
            LOG(ERROR) << "Error while loading rule script: " << lua_tostring(lua_, -1);
            lua_pop(lua_, 1);
            throw std::runtime_error("Error loading rule script");
        }

        // execute
        if (lua_pcall(lua_, 0, 0, 0))
        {
            LOG(ERROR) << "Error running rule script: " << lua_tostring(lua_, -1);
            lua_pop(lua_, 1);
            throw std::runtime_error("Error running rule script");
        }
    }
}
