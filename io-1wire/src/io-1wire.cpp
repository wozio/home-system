#include "ownetwork.h"
#include "utils/app.h"
#include "utils/logger_init.h"
#include "com/yamicontainer.h"
#include "com/discovery.h"
#include <boost/program_options.hpp>
#include <chrono>
#include <thread>
#include <iostream>

INITIALIZE_EASYLOGGINGPP

using namespace std;
namespace po = boost::program_options;

home_system::com::yc_t _yc;
home_system::com::discovery_t _discovery;

int main(int argc, char** argv)
{
  cout << "Home System IO 1 wire" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help"))
  {
    cout << desc << endl;
    return 1;
  }

  home_system::utils::init_log("io-1wire.log", !vm.count("daemonize"));

  LOG(INFO) << "Home System IO 1wire started";

  home_system::utils::app app(vm.count("daemonize"));

  std::unique_ptr<ownet> net;

  app.run([&] () {
		_yc = home_system::com::yami_container::create();
    _discovery = home_system::com::discovery::create();
    
    // TODO configurable device name
    // TODO configurable service name
    net.reset(new ownet("DS2490-1"));
	},
	[&] () {
    net.reset();
		_discovery.reset();
    _yc.reset();
	});

  LOG(INFO) << "Home System IO 1wire quitting";

  return 0;
}
