#include "dhwrec-service.h"
#include "logger.h"
#include "discovery.h"
#include "yamicontainer.h"
#include <boost/program_options.hpp>
#ifdef __linux__
#include <signal.h>
#endif
#include <iostream>

using namespace std;
namespace po = boost::program_options;

home_system::yami_container _yc;
home_system::discovery _discovery;

int main(int argc, char** argv)
{
  cout << "Home System DHW Recirculation" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("service,s", po::value<string>()->default_value("relay-board"),
      "output handling service which owns output")
    ("output,o", po::value<int>()->default_value(0),
      "output number to be controlled")
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
  
  home_system::logger::configure("dhw-recirculation.log", vm["log_level"].as<string>(), !vm.count("daemonize"));
  
  LOGINFO("Home System DHW Recirculation started");

#ifdef __linux__
  if (vm.count("daemonize"))
  {
    cout << "Running as daemon" << endl;
    
    _discovery.notify_fork(boost::asio::io_service::fork_prepare);
    
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
    
    _discovery.notify_fork(boost::asio::io_service::fork_child);

    umask(0);

    pid_t sid = setsid();
    if (sid < 0)
    {
      LOGERROR("Cannot setsid");
      exit(EXIT_FAILURE);
    }
    
#ifndef DEBUG
    if ((chdir("/")) < 0)
    {
      LOGERROR("Cannot chdir");
      exit(EXIT_FAILURE);
    }
#endif

    fclose(stdin);
    fclose(stdout);
    fclose(stderr);
  }
#endif
  
  try
  {
    home_system::comfort::dhwrec_service service(vm["service"].as<string>(),
      vm["output"].as<int>());
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
  
  LOGINFO("Home System DHW Recirculation quitting");

  return 0;
}

