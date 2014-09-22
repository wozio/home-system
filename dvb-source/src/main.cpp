#include "dvb-service.h"
#include "app.h"
#include "logger.h"
#include "discovery.h"
#include "yamicontainer.h"
#include <boost/program_options.hpp>
#include <boost/asio.hpp>
#include <iostream>
#include <string>

using namespace std;
namespace po = boost::program_options;

home_system::yc_t _yc;
home_system::discovery_t _discovery;

int main(int argc, char** argv)
{
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("name,n", po::value<string>()->default_value("dvb-source"), "service name")
    ("adapter,a", po::value<int>()->default_value(0), "Adapter to use")
    ("frontend,f", po::value<int>()->default_value(0), "Frontend to use")
    ("transponders,t", po::value<string>()->default_value("transponders.conf"), "Transponders definition file in (dvb)scan format")
    ("channels,c", po::value<string>()->default_value("channels.conf"), "Channel definition file in (dvb)scan format")
    ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help"))
  {
    cout << desc << endl;
    return 1;
  }

  home_system::logger::_log_file_path = "dvb-source.log";

  LOGINFO("DVB Source started");
  
  home_system::media::dvb_service* service_p = nullptr;
  
  home_system::app app(vm.count("daemonize"), [&] (const std::vector<std::string>& cmd)
  {
    if (cmd[0] == "lc") // list local channels
    {
      if (service_p != nullptr)
      {
        service_p->print_channels();
      }
    }
    else if (cmd[0] == "lt") // list transponders
    {
      if (service_p != nullptr)
      {
        service_p->print_transponders();
      }
    }
    else if (cmd[0] == "ct") // current transponder
    {
      if (service_p != nullptr)
      {
        service_p->print_current_transponder();
      }
    }
  });
  
  bool exit_init_loop = false;
  int init_try = 0;
  do
  {
    try
    {
      _yc = home_system::yami_container::create([] (const std::string& msg) {
        LOG("YC: " << msg);
      });
      _discovery = home_system::discovery::create([] (const std::string& msg) {
        LOG("Discovery: " << msg);
      });
      
      home_system::media::dvb_service service(vm["name"].as<string>(),
        vm["adapter"].as<int>(),
        vm["frontend"].as<int>(),
        vm["transponders"].as<string>(),
        vm["channels"].as<string>());

      exit_init_loop = true;
      
      service_p = &service;

      app.run();
      
    }
    catch (const std::exception & e)
    {
      LOGERROR("Exception: " << e.what());
    }
    catch (...)
    {
      LOGERROR("Unknown Exception");
    }
    if (!exit_init_loop)
    {
      if (++init_try < 60)
      {
        LOGERROR("Initialization not done, waiting 1 second before next try...");
        this_thread::sleep_for(chrono::seconds(1));
      }
      else
      {
        exit_init_loop = true;
      }
    }
  }
  while (!exit_init_loop);
  
  _discovery.reset();
  _yc.reset();

  LOGINFO("DVB Source quitting");
  return 0;
}
