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


#include "Poco/Net/HTTPRequest.h"
#include "Poco/Net/HTTPResponse.h"
#include "Poco/Net/HTTPMessage.h"
#include "Poco/Net/WebSocket.h"
#include "Poco/Net/HTTPSClientSession.h"
#include "Poco/Net/NetException.h"
#include "Poco/Net/SSLException.h"
#include "Poco/Net/SSLManager.h"
#include "Poco/Net/KeyConsoleHandler.h"
#include "Poco/Net/PrivateKeyPassphraseHandler.h"
#include "Poco/Net/ConsoleCertificateHandler.h"
#include "Poco/Net/InvalidCertificateHandler.h"
#include "Poco/Net/AcceptCertificateHandler.h"
#include "Poco/Net/SecureStreamSocket.h"
#include "Poco/Net/SecureServerSocket.h"
#include "Poco/Net/HTTPServerRequestImpl.h"
#include "Poco/Net/HTTPRequestHandler.h"
#include "Poco/Net/HTTPRequestHandlerFactory.h"
#include "boost/date_time/posix_time/posix_time.hpp"

using namespace std;
namespace po = boost::program_options;

using Poco::Net::HTTPSClientSession;
using Poco::Net::HTTPRequest;
using Poco::Net::HTTPResponse;
using Poco::Net::HTTPMessage;
using Poco::Net::WebSocket;

home_system::yc_t _yc;
home_system::discovery_t _discovery;


class TimeRequestHandler: public Poco::Net::HTTPRequestHandler
{
public:
	TimeRequestHandler()
	{
	}
	
	void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response)
	{
		std::cout << "Request from " << request.clientAddress().toString() << std::endl;

		
	}
};


class TimeRequestHandlerFactory: public Poco::Net::HTTPRequestHandlerFactory
{
public:
	TimeRequestHandlerFactory()
	{
	}

	Poco::Net::HTTPRequestHandler* createRequestHandler(const Poco::Net::HTTPServerRequest& request)
	{
	  return new TimeRequestHandler();
	}
};



int main(int argc, char** argv)
{
  cout << "Home System Control Server" << endl;
  
  // Declare the supported options.
  po::options_description desc("Allowed options");
  desc.add_options()
    ("help,h", "produce help message")
    ("daemonize,d", "run as daemon")
    ("cloud,c", "integrate with cloud server")
    ("root,r", po::value<std::string>()->default_value("/var/www"), "path to web page root")
    ("port,p", po::value<int>()->default_value(80), "port number for web page access")
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
      new home_system::request_handler_factory(vm["root"].as<std::string>()),
      svs, new Poco::Net::HTTPServerParams);
    srv.start();

    LOGINFO("Listening for http access on " << boost::asio::ip::host_name() <<
      ":" << port);
    
    
    {
      Poco::Net::initializeSSL();
      
      try {
        if (vm.count("cloud"))
        {
          // set-up a server socket
          Poco::Net::ServerSocket svc(5000);
          // set-up a HTTPServer instance
          Poco::Net::HTTPServer ssrv(new TimeRequestHandlerFactory(), svc, new Poco::Net::HTTPServerParams);
          // start the HTTPServer
          ssrv.start();
          
          cout << "Enter q to quit..." << endl;
          string input_line;
          while (std::getline(std::cin, input_line))
          {
            if (input_line == "q" || input_line == "quit")
            {
              break;
            }
          }
          ssrv.stop();
        }
        else
        {
          Poco::SharedPtr<Poco::Net::PrivateKeyPassphraseHandler> pConsoleHandler = new Poco::Net::KeyConsoleHandler(false);
          Poco::SharedPtr<Poco::Net::InvalidCertificateHandler> pInvalidCertHandler = new Poco::Net::AcceptCertificateHandler(false);
          Poco::Net::Context::Ptr pContext = new Poco::Net::Context(Poco::Net::Context::CLIENT_USE, "", Poco::Net::Context::VERIFY_NONE, 9, true);
          Poco::Net::SSLManager::instance().initializeClient(pConsoleHandler, pInvalidCertHandler, pContext);

          std::cout << "client session" << std::endl;

          HTTPSClientSession cs("atorchardstreet.com");
          //cs.setProxy("172.23.0.100", 8080);

          std::cout << "request" << std::endl;
          HTTPRequest request(HTTPRequest::HTTP_GET, "/access/", HTTPMessage::HTTP_1_1);
          request.set("origin", "http://some.origin");
          HTTPResponse response;

          std::cout << "ws" << std::endl;
          WebSocket ws(cs, request, response);
          char *testStr="";
          char receiveBuff[2560];

          boost::posix_time::time_duration dur;
          
          for (int i = 0; i < 100; i++)
          {
	    boost::posix_time::ptime pt1 = boost::posix_time::microsec_clock::local_time();
            int len = ws.sendFrame(testStr, strlen(testStr) + 1, WebSocket::FRAME_TEXT);
            //std::cout << "Sent bytes " << len << std::endl;
            int flags = 0;

            int rlen = ws.receiveFrame(receiveBuff, 2560, flags);
            //std::cout << "Received bytes " << rlen << std::endl;
            //std::cout << receiveBuff << std::endl;
	    boost::posix_time::ptime pt2 = boost::posix_time::microsec_clock::local_time();

            dur += pt2 - pt1;
          }

          std::cout << dur / 100 << std::endl;
        }
      } catch (Poco::Net::HTTPException &e) {
          std::cout << "HTTP Exception " << e.displayText() << std::endl;
      } catch (Poco::Net::SSLException &e) {
        std::cout << "SSL Exception " << e.displayText() << std::endl;
      } catch (Poco::Net::WebSocketException &e) {
        std::cout << "WebSocket Exception " << e.displayText() << std::endl;
      } catch (std::exception& e) {
        std::cout << "Exception " << e.what() << std::endl;
      }
      
      Poco::Net::uninitializeSSL();
      
    }

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

