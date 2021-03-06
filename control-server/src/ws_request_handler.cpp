#include "pch.h"
#include "ws_request_handler.h"
#include "client.h"
#include "clients.h"

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

ws_request_handler::ws_request_handler(const std::string& client_id)
  : client_id_(client_id)
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

	  // get client by client_id
    try
    {
      LOG(DEBUG) << "Check for binary connection request: " << client_id_;
      CLIENTS.get(client_id_)->add_binary_connection(ws);
      return;
    }
    catch (const std::out_of_range& e)
    {
    }
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
