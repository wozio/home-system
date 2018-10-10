#include "gpios.h"
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
  cout << "Home System IO GPIO" << endl;

  std::vector<int> ports;
  std::vector<int> modes;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("name,n", po::value<string>()->default_value("io.gpio"), "service name")
    ("port,p", po::value<std::vector<int>>(&ports), "GPIO port")
    ("mode,m", po::value<std::vector<int>>(&modes), "Mode of GPIO port (0 - output, 1 - input)")
  ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help"))
  {
    cout << desc << endl;
    return 1;
  }
  
  if (ports.size() == 0)
  {
    cout << "At least one port must be specified" << endl;
    cout << desc << endl;
    return 1;
  }

  if (modes.size() == 0)
  {
    cout << "At least one mode must be specified" << endl;
    cout << desc << endl;
    return 1;
  }

  if (ports.size() != modes.size())
  {
    cout << "Mode must be specified the same number of times as gpio" << endl;
    cout << desc << endl;
    return 1;
  }

  for (auto mode : modes)
  {
    if (mode != 1 && mode != 0)
    {
      cout << "Invalid value for mode parameter, it must be 0 or 1" << endl;
      cout << desc << endl;
      return 1;
    }
  }

	home_system::utils::app app(vm.count("daemonize"));

  home_system::utils::init_log("io-gpio.log", !vm.count("daemonize"));

  LOG(INFO) << "Home System IO GPIO started";

  std::unique_ptr<gpios> service;

	app.run([&] ()
  {
		_yc = home_system::com::yami_container::create();
    _discovery = home_system::com::discovery::create();

    service.reset(new gpios(vm["name"].as<string>(),
      ports, modes));
	},
	[&] ()
  {
    service.reset();
		_discovery.reset();
		_yc.reset();
	});

  LOG(INFO) << "Home System IO GPIO quitting";
  return 0;
}