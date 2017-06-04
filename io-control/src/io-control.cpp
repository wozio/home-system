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

	app.run([&] () {
		_config = home_system::utils::config::create(vm["config-file"].as<std::string>());
		_yc = home_system::com::yami_container::create();
		_discovery = home_system::com::discovery::create();
		_ios = ::ios::create();
        _rules = ::rules::create();
        _services = ::services::create();
	},
	[&] () {
        _services.reset();
        _rules.reset();
		_ios.reset();
		_discovery.reset();
		_yc.reset();
		_config.reset();
	});

	LOG(INFO) << "Home System IO Control quitting";

    return 0;
}
