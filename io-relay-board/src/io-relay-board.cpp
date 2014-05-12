#include "iorb-service.h"

#include "logger.h"
#include "yamicontainer.h"

#include <boost/program_options.hpp>
#include <iostream>

#ifdef __linux__
#include <signal.h>
#endif

home_system::yc_t _yc;

using namespace std;

namespace po = boost::program_options;

int main(int argc, char** argv)
{
  cout << "Home System IO Relay Board controller" << endl;

  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("name,n", po::value<string>()->default_value("relay-board"), "service name")
    ("port,p", po::value<string>(), "COM port")
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
  
  if (!vm.count("port"))
  {
    cout << "Port must be specified" << endl;
    cout << desc << endl;
    return 1;
  }
  
  home_system::logger::configure("io-relay-board.log", vm["log_level"].as<string>(), !vm.count("daemonize"));

#ifdef __linux__
  LOGINFO("Relay Board started");
  
  if (vm.count("daemonize"))
  {
    cout << "Running as daemon" << endl;
    
    pid_t pid = fork();
    if (pid < 0)
    {
      LOGERROR("Cannot fork");
      exit(EXIT_FAILURE);
    }
    else if (pid > 0)
    {
      exit(EXIT_SUCCESS);
    }
    
    umask(0);

    pid_t sid = setsid();
    if (sid < 0)
    {
      LOGERROR("Cannot setsid");
      exit(EXIT_FAILURE);
    }
    
    fclose(stdin);
    fclose(stdout);
    fclose(stderr);
  }
#endif
  
  try
  {
    _yc = home_system::yami_container::create();
    
    home_system::input_output::iorb_service service(vm["name"].as<string>(),
      vm["port"].as<string>());
#ifdef __linux__
    if (vm.count("daemonize"))
    {
      sigset_t sset;
      sigemptyset(&sset);
      sigaddset(&sset, SIGQUIT);
      sigaddset(&sset, SIGTERM);
      sigprocmask(SIG_BLOCK, &sset, NULL);
      int sig;
      sigwait(&sset, &sig);
    }
    else
#endif
    {
      cout << "Enter q to quit..." << endl;
      std::string input_line;
      while (std::getline(std::cin, input_line))
      {
        if (input_line == "q" || input_line == "quit")
        {
          break;
        }
      }
    }
  }
  catch (const std::exception & e)
  {
    LOGERROR("Exception: " << e.what());
  }
  catch (...)
  {
    LOGERROR("Unknown Exception");
  }

  _yc.reset();

  LOGINFO("Relay Board quitting");
  return 0;
}
