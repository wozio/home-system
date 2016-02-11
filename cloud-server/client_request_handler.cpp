#include "client_request_handler.h"
#include "clients.h"
#include "logger.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/NetException.h>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{
  
client_request_handler::client_request_handler()
{
}

client_request_handler::~client_request_handler()
{
}

void client_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    LOG(DEBUG) << "Request for client connection";
    
    // any exceptions thrown on WebSocket handshake or client validation
    // will lead to not registering client
    ws_t ws(new WebSocket(request, response));
    client_t h(new client(ws));
    h->init();
    
    CLIENTS.add(h);
  }
  catch (const WebSocketException& exc)
  {
    LOG(ERROR) << exc.displayText();
    switch (exc.code())
    {
    case WebSocket::WS_ERR_HANDSHAKE_UNSUPPORTED_VERSION:
      response.set("Sec-WebSocket-Version", WebSocket::WEBSOCKET_VERSION);
      // fallthrough
    case WebSocket::WS_ERR_NO_HANDSHAKE:
    case WebSocket::WS_ERR_HANDSHAKE_NO_VERSION:
    case WebSocket::WS_ERR_HANDSHAKE_NO_KEY:
      response.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST);
      response.setContentLength(0);
      response.send();
      break;
    }
  }
  catch (const Exception& e)
  {
    LOG(ERROR) << e.displayText();
  }
  catch (const exception& e)
  {
    LOG(ERROR) << e.what();
  }
}

}
