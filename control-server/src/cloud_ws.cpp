#include "pch.h"
#include "cloud_ws.h"
#include "logger.h"
#include "cloud_client.h"

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

cloud_ws::cloud_ws(const std::string& host, int port, const std::string& uri, bool no_ssl)
: host_(host),
  port_(port),
  uri_(uri),
  no_ssl_(no_ssl)
{
  if (!no_ssl_)
    Poco::Net::initializeSSL();
  timer_.set_from_now(1000, [this](){ this->connect(); });
}

cloud_ws::~cloud_ws()
{
  timer_.cancel();
  if (!no_ssl_)
    Poco::Net::uninitializeSSL();
}

void cloud_ws::connect()
{
  LOG(INFO) << "Connecting to cloud server";
  
  try
  {
    unique_ptr<HTTPClientSession> cs;
    if (!no_ssl_)
    {
      SharedPtr<PrivateKeyPassphraseHandler> pConsoleHandler = new KeyConsoleHandler(false);
      SharedPtr<InvalidCertificateHandler> pInvalidCertHandler = new AcceptCertificateHandler(false);
      Context::Ptr pContext = new Context(Context::CLIENT_USE, "", Context::VERIFY_NONE, 9, true);
      SSLManager::instance().initializeClient(pConsoleHandler, pInvalidCertHandler, pContext);

      cs.reset(new HTTPSClientSession(host_, port_));
    }
    else
    {
      cs.reset(new HTTPClientSession(host_, port_));
    }

    // TODO: add proxy handling as below but from system or from command line params
    //cs->setProxy("172.23.0.100", 8080);

    HTTPRequest request(HTTPRequest::HTTP_GET, uri_, HTTPMessage::HTTP_1_1);

    HTTPResponse response;

    ws_t ws(new WebSocket(*cs, request, response));

    shared_ptr<cloud_client> h(new cloud_client(ws, [this](){
      timer_.set_from_now(1000, [this](){
        connect();
      });
    }));
    h->init();
  }
  catch (const std::exception &e)
  {
    LOG(ERROR) << "Error: " << e.what();
    timer_.set_from_now(1000, [this](){
      this->connect();
    });
  }
}

}
