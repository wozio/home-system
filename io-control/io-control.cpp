#include "stdafx.h"
#include "yamicontainer.h"
#include "discovery.h"
#include "logger_init.h"
#include "config.h"
#include "app.h"

INITIALIZE_EASYLOGGINGPP

using namespace std;
namespace po = boost::program_options;

home_system::config_t _config;
home_system::yc_t _yc;
home_system::discovery_t _discovery;

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

	home_system::init_log("io-control.log", !vm.count("daemonize"));

	LOG(INFO) << "Home System IO Control started";

	try
	{
		_config = home_system::config::create(vm["config-file"].as<std::string>());
		home_system::app::prepare(vm.count("daemonize"));

		_yc = home_system::yami_container::create();
		_discovery = home_system::discovery::create();

		home_system::app::run(vm.count("daemonize"));
	}
	catch (const exception& e)
	{
		LOG(ERROR) << "EXCEPTION: " << e.what();
	}
	catch (...)
	{
		LOG(ERROR) << "UNKNOWN EXCEPTION";
	}

	_discovery.reset();
	_yc.reset();
	_config.reset();

	LOG(INFO) << "Home System IO Control exit";

    return 0;
}

