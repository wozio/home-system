#include "cloud_ws.h"
#include "logger.h"
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

namespace home_system
{

cloud_ws::cloud_ws()
{
  LOGINFO("Integrating with cloud server");
  
  Poco::Net::initializeSSL();
  
  try {
    SharedPtr<PrivateKeyPassphraseHandler> pConsoleHandler = new KeyConsoleHandler(false);
    SharedPtr<InvalidCertificateHandler> pInvalidCertHandler = new AcceptCertificateHandler(false);
    Context::Ptr pContext = new Context(Context::CLIENT_USE, "", Context::VERIFY_NONE, 9, true);
    SSLManager::instance().initializeClient(pConsoleHandler, pInvalidCertHandler, pContext);

    // TODO: configurable cloud server
    HTTPSClientSession cs("atorchardstreet.com");
    
    // TODO: add proxy handling as below but from system or from command line params
    //cs.setProxy("172.23.0.100", 8080);

    HTTPRequest request(HTTPRequest::HTTP_GET, "/access/", HTTPMessage::HTTP_1_1);
    
    HTTPResponse response;

    ws_.reset(new WebSocket(cs, request, response));
    char *testStr="";
    char receiveBuff[2560];

    int len = ws_->sendFrame(testStr, strlen(testStr) + 1, WebSocket::FRAME_TEXT);
    int flags = 0;
    int rlen = ws_->receiveFrame(receiveBuff, 2560, flags);
    
  } catch (HTTPException &e) {
      LOGERROR("HTTP Exception: " << e.displayText());
  } catch (SSLException &e) {
    LOGERROR("SSL Exception " << e.displayText());
  } catch (WebSocketException &e) {
    LOGERROR("WebSocket Exception " << e.displayText());
  } catch (std::exception& e) {
    LOGERROR("Exception: " << e.what());
  }

  Poco::Net::uninitializeSSL();
}

cloud_ws::~cloud_ws()
{
  
}

}
