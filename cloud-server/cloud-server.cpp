#include "request_handler_factories.h"
#include <boost/asio.hpp>
#include "logger.h"
#include "systems.h"
#include "clients.h"
#include "handlers.h"
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

home_system::clients_t _clients;
home_system::systems_t _systems;
home_system::handlers_t _handlers;

int main(int argc, char** argv)
{
  cout << "Home System Cloud Server" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
#ifdef __linux__
    ("daemonize,d", "run as daemon")
#endif
    ("client_port,c", po::value<int>()->default_value(5000), "port number for client access")
    ("system_port,s", po::value<int>()->default_value(5001), "port number for system access")
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
  
  home_system::logger::_log_file_path = "cloud-server.log";
  
  LOGINFO("Home System Cloud Server started");
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
    _handlers = home_system::handlers::create();
    _clients = home_system::clients::create();
    _systems = home_system::systems::create();
    
    int port = vm["client_port"].as<int>();
    Poco::Net::ServerSocket client_svs(port);
    Poco::Net::HTTPServer client_srv(
      new home_system::client_request_handler_factory(),
      client_svs, new Poco::Net::HTTPServerParams);
    client_srv.start();

    LOGINFO("Listening for client access on " << boost::asio::ip::host_name() <<
      ":" << port);
    
    port = vm["system_port"].as<int>();
    Poco::Net::ServerSocket system_svs(port);
    Poco::Net::HTTPServer system_srv(
      new home_system::system_request_handler_factory(),
      system_svs, new Poco::Net::HTTPServerParams);
    system_srv.start();

    LOGINFO("Listening for system access on " << boost::asio::ip::host_name() <<
      ":" << port);
    
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

    
    system_srv.stopAll(true);
    client_srv.stopAll(true);
    
    _clients.reset();
    _systems.reset();
    _handlers.reset();
  }
  catch (const exception& e)
  {
    LOGERROR("Exception: " << e.what());
  }
  catch (...)
  {
    LOGERROR("Unknown Exception");
  }

  LOGINFO("Home System Cloud Server quitting");
  
  return 0;
}

