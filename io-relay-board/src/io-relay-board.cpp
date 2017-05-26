#include "board.h"
#include "utils/app.h"
#include "utils/logger_init.h"
#include "com/yamicontainer.h"
#include "com/discovery.h"
#include <boost/program_options.hpp>
#include <iostream>

using namespace std;
namespace po = boost::program_options;

home_system::com::yc_t _yc;
home_system::com::discovery_t _discovery;

int main(int argc, char** argv)
{
  cout << "Home System IO relay board" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("name,n", po::value<string>()->default_value("relay-board"), "service name")
    ("port,p", po::value<string>(), "COM port")
  ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help"))
  {
    cout << desc << endl;
    return 1;
  }
  
  if (!vm.count("port"))
  {
    cout << "Port must be specified" << endl;
    cout << desc << endl;
    return 1;
  }
  
	home_system::utils::app app(vm.count("daemonize"));

  home_system::utils::init_log("io-relay-board.log", !vm.count("daemonize"));

  LOG(INFO) << "Home System IO relay board started";

  std::unique_ptr<board> service;

	app.run([&] () {
		_yc = home_system::com::yami_container::create();
    _discovery = home_system::com::discovery::create();
      
    service.reset(new board(vm["name"].as<string>(),
      vm["port"].as<string>()));
	},
	[&] () {
    service.reset();
		_discovery.reset();
		_yc.reset();
	});

  LOG(INFO) << "Home System IO Relay Board quitting";
  return 0;
}