#include "cloud_ws.h"
#include "logger.h"
#include "ws_com_handler.h"
#include "Poco/Net/HTTPSClientSession.h"
#include "Poco/Net/NetException.h"
#include "Poco/Net/SSLException.h"
#include "Poco/Net/SSLManager.h"
#include "Poco/Net/KeyConsoleHandler.h"
#include "Poco/Net/PrivateKeyPassphraseHandler.h"
#include "Poco/Net/InvalidCertificateHandler.h"
#include "Poco/Net/AcceptCertificateHandler.h"
#include "Poco/Net/HTTPRequest.h"
#include "Poco/Net/HTTPResponse.h"

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

cloud_ws::cloud_ws()
: run_thread_(true),
  thr_([this] () {this->thr_exec();})
{
  
}

cloud_ws::~cloud_ws()
{
  run_thread_ = false;
  thr_.join();
}

void cloud_ws::thr_exec()
{
  LOGINFO("Integrating with cloud server");
  
  Poco::Net::initializeSSL();
  
  bool errorLogged = false;
  while (run_thread_)
  {
    try
    {
      SharedPtr<PrivateKeyPassphraseHandler> pConsoleHandler = new KeyConsoleHandler(false);
      SharedPtr<InvalidCertificateHandler> pInvalidCertHandler = new AcceptCertificateHandler(false);
      Context::Ptr pContext = new Context(Context::CLIENT_USE, "", Context::VERIFY_NONE, 9, true);
      SSLManager::instance().initializeClient(pConsoleHandler, pInvalidCertHandler, pContext);

      // TODO: configurable cloud server
      HTTPSClientSession cs("atorchardstreet.com");

      // TODO: add proxy handling as below but from system or from command line params
      //cs.setProxy("172.23.0.100", 8080);

      // TODO: configurable URI
      HTTPRequest request(HTTPRequest::HTTP_GET, "/access/system/", HTTPMessage::HTTP_1_1);

      HTTPResponse response;

      WebSocket ws(cs, request, response);
      
      LOG("Connected");
      
      errorLogged = false;
      
      handle_ws_communication(ws);
    }
    catch (Exception &e)
    {
      if (!errorLogged)
      {
        LOGERROR("Error: " << e.displayText() << ", reconnecting");
        errorLogged = true;
      }
    }
  }
  Poco::Net::uninitializeSSL();
}

}
