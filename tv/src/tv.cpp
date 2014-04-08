#include "app.h"
#include "logger.h"
#include "tv-service.h"
#include <boost/program_options.hpp>
#include <vector>

using namespace std;
namespace po = boost::program_options;

int main(int argc, char** argv)
{
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("log_level,l", po::value<string>()->default_value("debug"), "Logging level, valid values are:\nerror\nwarning\ninformation\ndebug")
    ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);

  if (vm.count("help"))
  {
    cout << desc << endl;
    return 1;
  }
  
  home_system::logger::configure("tv.log", vm["log_level"].as<string>(), !vm.count("daemonize"));

  LOGINFO("TV started");

  home_system::app app(vm.count("daemonize"));

  try
  {
    home_system::media::tv_service service_;
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

  LOGINFO("TV quitting");
  return 0;
}

