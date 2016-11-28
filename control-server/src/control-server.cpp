#include "pch.h"
#include "http.h"
#include "control-service.h"
#include "app.h"
#include "logger_init.h"
#include "yamicontainer.h"
#include "discovery.h"
#include "cloud_ws.h"
#include "handlers.h"
#include "clients.h"

INITIALIZE_EASYLOGGINGPP

using namespace std;
namespace po = boost::program_options;

home_system::yc_t _yc;
home_system::discovery_t _discovery;
home_system::handlers_t _handlers;
home_system::clients_t _clients;

int main(int argc, char** argv)
{
  cout << "Home System Control Server" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
  ("help,h", "produce help message")
  ("daemonize,d", "run as daemon")
  ("no-cloud,c", "do not integrate with cloud server")
  ("cloud-host", po::value<string>()->default_value("atorchardstreet.com"), "cloud server host name")
  ("cloud-port", po::value<int>()->default_value(443), "cloud server port number")
  ("cloud-uri", po::value<string>()->default_value("/access/system/"), "cloud server uri")
  ("cloud-no-secure", "do not use SSL when communicating with cloud server (NOT SECURE!)")
  ("root,r", po::value<std::string>()->default_value("/var/www"), "path to web page root")
  ("port,p", po::value<int>()->default_value(80), "port number for web page access")
  ;

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, desc), vm);
  po::notify(vm);
  
  if (vm.count("help"))
  {
    cout << desc << endl;
    return 1;
  }
  
  home_system::init_log("control-server.log", !vm.count("daemonize"));
  
  LOG(INFO) << "Started";

  try
  {
    home_system::app app("control-server.conf", vm.count("daemonize"));

    _yc = home_system::yami_container::create();
    _discovery = home_system::discovery::create();
    _handlers = home_system::handlers::create();
    _clients = home_system::clients::create();

    int port = vm["port"].as<int>();
    Poco::Net::ServerSocket svs(port);
    Poco::Net::HTTPServer srv(
      new home_system::request_handler_factory(vm["root"].as<std::string>()),
      svs, new Poco::Net::HTTPServerParams);
    srv.start();

    LOG(INFO) << "Listening for http access on " << boost::asio::ip::host_name() <<
      ":" << port;
    
    std::unique_ptr<home_system::cloud_ws> cws;
    
    if (!vm.count("no-cloud"))
    {
      cws.reset(new home_system::cloud_ws(
        vm["cloud-host"].as<std::string>(),
        vm["cloud-port"].as<int>(),
        vm["cloud-uri"].as<std::string>(),
        vm.count("cloud-no-secure")
      ));
    }

    home_system::control_server::control_service csrv;

    app.run();

    srv.stopAll(true);
  }
  catch (const exception& e)
  {
    LOG(ERROR) << "Exception: " << e.what();
  }
  catch (...)
  {
    LOG(ERROR) << "Unknown Exception";
  }
  
  _clients.reset();
  _handlers.reset();
  _discovery.reset();
  _yc.reset();
  
  LOG(INFO) << "Home System Control Server quitting";
  
  return 0;
}

