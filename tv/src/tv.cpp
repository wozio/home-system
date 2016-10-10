#include "pch.h"
#include "app.h"
#include "logger_init.h"
#include "db.h"
#include "tv-service.h"
#include "discovery.h"
#include "yamicontainer.h"

INITIALIZE_EASYLOGGINGPP

using namespace std;
namespace po = boost::program_options;

home_system::yc_t _yc;
home_system::discovery_t _discovery;

int main(int argc, char** argv)
{
  cout << "Home System TV" << endl;

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
  
  home_system::init_log("tv.log", !vm.count("daemonize"));
  
  LOG(INFO) << "Started";
  
  home_system::app app(vm.count("daemonize"));
  
  try
  {
    _yc = home_system::yami_container::create();
    _discovery = home_system::discovery::create();
    
    home_system::media::db db_;
    home_system::media::tv_service service_(db_);
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
  
  _discovery.reset();
  _yc.reset();

  LOG(INFO) << "Quitting";
  return 0;
}

