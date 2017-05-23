#include "stdafx.h"
#include "com/discovery.h"
#include "utils/logger_init.h"
#include "utils/config.h"
#include "utils/app.h"
#include "ios.h"

using namespace std;
namespace po = boost::program_options;

home_system::utils::config_t _config;
home_system::com::yc_t _yc;
home_system::com::discovery_t _discovery;
ios_t _ios;

int main(int argc, char** argv)
{
	po::options_description desc("Allowed options");
	desc.add_options()
	("help,h", "produce help message")
	("daemonize,d", "run as daemon")
	("config-file,c", po::value<string>()->default_value("io-control.conf"), "path to configuration file")
	;
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);

	home_system::utils::init_log("io-control.log", !vm.count("daemonize"));

	LOG(INFO) << "Home System IO Control started";

	home_system::utils::app app(vm.count("daemonize"));

	app.run([&] () {
		_config = home_system::utils::config::create(vm["config-file"].as<std::string>());
		_yc = home_system::com::yami_container::create();
		_discovery = home_system::com::discovery::create();
		_ios = ::ios::create();
	},
	[&] () {
		_ios.reset();
		_discovery.reset();
		_yc.reset();
		_config.reset();
	});

	LOG(INFO) << "Home System IO Control quitting";

    return 0;
}
