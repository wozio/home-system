#include "ios.h"
#include "rules.h"
#include "services.h"
#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/logger_init.h"
#include "utils/config.h"
#include "utils/app.h"
#include <boost/program_options.hpp>
#include <iostream>

using namespace std;
namespace po = boost::program_options;

home_system::utils::config_t _config;
home_system::com::yc_t _yc;
home_system::com::discovery_t _discovery;
ios_t _ios;
rules_t _rules;
services_t _services;

int main(int argc, char** argv)
{
	cout << "Home System IO Control" << endl;

	po::options_description desc("Allowed options");
	desc.add_options()
	("help,h", "produce help message")
	("daemonize,d", "run as daemon")
	("config-file,c", po::value<string>()->default_value("io-control.conf"), "path to configuration file")
	;
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);

	home_system::utils::app app(vm.count("daemonize"));

	home_system::utils::init_log("io-control", !vm.count("daemonize"));

	LOG(INFO) << "Home System IO Control started";

  lua_State *lua;

	app.run([&] ()
  {
		_config = home_system::utils::config::create(vm["config-file"].as<std::string>());
		_yc = home_system::com::yami_container::create();
		_discovery = home_system::com::discovery::create();

    // initializing LUA
    lua = luaL_newstate();
    luaL_openlibs(lua);

    if (luaL_loadfile(lua, "io-control.lua"))
    {
        LOG(ERROR) << "Failed to load io-control.lua file: " << lua_tostring(lua, -1);
        lua_pop(lua, 1);
        throw std::runtime_error("Error loading lua script");
    }
    // priming run
    if (lua_pcall(lua, 0, 0, 0))
    {
        LOG(ERROR) << "Error in priming call: " << lua_tostring(lua, -1);
        lua_pop(lua, 1);
        throw std::runtime_error("Error in priming call");
    }

		_ios = ::ios::create(lua);
    _rules = ::rules::create(lua);
    _services = ::services::create();

    _ios->init();
    _rules->init();

    // now everything is created and connected its time to kickoff
    // it means that ios object will subscribe in IO remote drivers
    // to receive state and value
    _ios->kickoff();
	},
	[&] ()
  {
    _services.reset();
    _rules.reset();
		_ios.reset();
    lua_close(lua);
		_discovery.reset();
		_yc.reset();
		_config.reset();
	});

	LOG(INFO) << "Home System IO Control quitting";

  return 0;
}
