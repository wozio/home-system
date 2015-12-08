#include "ws_request_handler.h"
#include "client.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/NetException.h>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

ws_request_handler::ws_request_handler()
{
}

ws_request_handler::~ws_request_handler()
{
}

void ws_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    ws_t ws(new WebSocket(request, response));

    shared_ptr<client> h(new client(ws));
    h->init();
  }
  catch (WebSocketException& exc)
  {
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
}
  
}
