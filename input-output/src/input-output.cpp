#include "io-service.h"
#include "app.h"
#include "logger_init.h"
#include "yamicontainer.h"
#include <boost/program_options.hpp>
#include <chrono>
#include <thread>
#include <iostream>

INITIALIZE_EASYLOGGINGPP

using namespace std;
namespace po = boost::program_options;

home_system::yc_t _yc;

int main(int argc, char** argv)
{
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

  home_system::init_log("input-output.log", !vm.count("daemonize"));

  if (vm.count("daemonize"))
  {
    cout << "1 wire input output started" << endl;
  }
  LOG(INFO) << "1 wire input output started";

  home_system::app app(vm.count("daemonize"));
    
  bool exit_init_loop = false;
  int init_try = 0;
  do
  {
    try
    {
      _yc = home_system::yami_container::create();
      
      home_system::input_output::io_service service;

      exit_init_loop = true;

      app.run();
    }
    catch (const std::exception & e)
    {
      LOG(ERROR) << "Exception: " << e.what();
    }
    catch (...)
    {
      LOG(ERROR) << "Unknown Exception";
    }
    if (!exit_init_loop)
    {
      if (++init_try < 60)
      {
        LOG(ERROR) << "Initialization not done, waiting 1 second before next try...";
        this_thread::sleep_for(chrono::seconds(1));
      }
      else
      {
        exit_init_loop = true;
      }
    }
  }
  while (!exit_init_loop);
  
  _yc.reset();

  LOG(INFO) << "1 wire input output quitting";

  return 0;
}
