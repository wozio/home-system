#include "http.h"
#include "control-service.h"
#include "logger.h"
#include "yamicontainer.h"
#include "discovery.h"
#ifdef __linux__
#include <unistd.h>
#endif
#include <Poco/Net/HTTPServer.h>
#include <Poco/Net/HTTPServerParams.h>
#include <Poco/Net/ServerSocket.h>
#include <boost/program_options.hpp>
#include <signal.h>
#include <iostream>

using namespace std;
namespace po = boost::program_options;

home_system::yc_t _yc;
home_system::discovery_t _discovery;

int main(int argc, char** argv)
{
  cout << "Home System Control Server" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
    ("daemonize,d", "run as daemon")
#ifdef _DEBUG
    ("port,p", po::value<int>()->default_value(5002),
#else
    ("port,p", po::value<int>()->default_value(5001),
#endif
      "port number for http access")
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
  
  home_system::logger::_log_file_path = "control-server.log";
  
  LOGINFO("Home System Control Server started");
#ifdef __linux__  
  if (vm.count("daemonize"))
  {
    cout << "Running as daemon" << endl;
    
    pid_t pid = fork();
    if (pid < 0)
    {
      cout << "Cannot fork" << endl;
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
      cout << "Cannot setsid" << endl;
      exit(EXIT_FAILURE);
    }
    
#ifndef _DEBUG
    if ((chdir("/")) < 0)
    {
      cout << "Cannot chdir" << endl;
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
    _yc = home_system::yami_container::create();
    _discovery = home_system::discovery::create();

    int port = vm["port"].as<int>();
    Poco::Net::ServerSocket svs(port);
    Poco::Net::HTTPServer srv(
      new home_system::control_server::request_handler_factory(),
      svs, new Poco::Net::HTTPServerParams);
    srv.start();

    LOGINFO("Listening for http access on " << boost::asio::ip::host_name() <<
      ":" << port);

    home_system::control_server::control_service csrv;
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
      string input_line;
      while (std::getline(std::cin, input_line))
      {
        if (input_line == "q" || input_line == "quit")
        {
          break;
        }
      }
    }

    srv.stop();
  }
  catch (const exception& e)
  {
    LOGERROR("Exception: " << e.what());
  }
  catch (...)
  {
    LOGERROR("Unknown Exception");
  }

  _discovery.reset();
  _yc.reset();
  
  LOGINFO("Home System Control Server quitting");
  
  return 0;
}

