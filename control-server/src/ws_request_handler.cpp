#include "ws_request_handler.h"
#include "json_converter.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include "rapidjson/memorystream.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/Net/WebSocket.h>
#include <Poco/Net/NetException.h>
#include <Poco/URI.h>
#include <boost/algorithm/string.hpp>
#include <vector>
#include <memory>

using namespace Poco::Net;
using namespace Poco;
using namespace std;

namespace home_system
{

ws_request_handler::ws_request_handler()
{
}

void ws_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    Poco::Net::WebSocket ws(request, response);
    std::unique_ptr<char[]> data(new char[1024]);
    int flags;
    int n;
    do
    {
      n = ws.receiveFrame(data.get(), 1024, flags);
      data[n] = '\n';
      yami::parameters params;
      std::string service;
      std::string msg;
      home_system::Handler handler(service, msg, params);
      rapidjson::Reader reader;
      rapidjson::MemoryStream ss(data.get(), n);
      reader.Parse(ss, handler);
      
      
      
      
      ws.sendFrame(data.get(), n, flags);
    }
    while (n > 0 || (flags & Poco::Net::WebSocket::FRAME_OP_BITMASK) != Poco::Net::WebSocket::FRAME_OP_CLOSE);
  }
  catch (Poco::Net::WebSocketException& exc)
  {
    switch (exc.code())
    {
    case Poco::Net::WebSocket::WS_ERR_HANDSHAKE_UNSUPPORTED_VERSION:
      response.set("Sec-WebSocket-Version", Poco::Net::WebSocket::WEBSOCKET_VERSION);
      // fallthrough
    case Poco::Net::WebSocket::WS_ERR_NO_HANDSHAKE:
    case Poco::Net::WebSocket::WS_ERR_HANDSHAKE_NO_VERSION:
    case Poco::Net::WebSocket::WS_ERR_HANDSHAKE_NO_KEY:
      response.setStatusAndReason(HTTPResponse::HTTP_BAD_REQUEST);
      response.setContentLength(0);
      response.send();
      break;
    }
  }
  
  return;
  
  try
  {
    // parsing query
    // we should have execute?name=<service_name>&msg=<msg>
    // and in request body we may have json with parameters of the message
    URI uri(request.getURI());
    string query = uri.getQuery();
    if (query.size() == 0)
      throw bad_request();

    string name;
    string msg;

    size_t pos1 = 0;
    size_t pos2;
    do
    {
      pos2 = query.find_first_of("&", pos1);
      size_t epos = query.find_first_of("=", pos1);
      if (epos == string::npos || epos > pos2 || epos == pos1 || epos == pos2)
        throw bad_request();
      string parname = query.substr(pos1, epos - pos1);
      string value = query.substr(epos + 1, pos2 - epos - 1);
      if (parname == "name")
      {
        name = value;
      }
      else if (parname == "msg")
      {
        msg = value;
      }
      pos1 = pos2 + 1;
    }
    while (pos2 != string::npos);

    if (name.size() == 0 || msg.size() == 0)
    {
      LOGWARN("Bad query: " << query);
      throw bad_request();
    }

    // getting service from service discovery
    string ye = DISCOVERY.get(name);

    LOG("Service: " << name << " (" << ye << ")");

    yami::parameters params;

    try
    {
      
    }
    catch (const std::exception& e)
    {
      LOG("EXCEPTION during XML parsing: " << e.what());
    }

    // sending message to service
    auto_ptr<yami::outgoing_message> message(
      AGENT.send(ye, name, msg, params));

    message->wait_for_completion(1000);

    switch (message->get_state())
    {
      case yami::replied:
      {
        LOG("Replied");
        response.setChunkedTransferEncoding(true);
        response.setContentType("application/json");
        response.add("Expires", "-1");
        response.add("Cache-control", "no-cache");

        // converting yami output to xml
        // yami binary values are not supported

        ostream& resp_str = response.send();
        yami::parameters* reply = message->extract_reply();
        process_parameters(reply, resp_str);
        delete reply;

        break;
      }

      case yami::posted:
      case yami::transmitted:
        LOG("Posted/transmitted");
        response.send();
        break;

      case yami::abandoned:
        LOG("Abandoned");
        throw service_unavailable("Message was abandoned");
        break;

      case yami::rejected:
        LOG("Rejected: " + message->get_exception_msg());
        throw service_unavailable("Message was rejected: " + message->get_exception_msg());
        break;
    }
  }
  catch (const service_not_found& e)
  {
    LOGWARN("EXCEPTION: service_not_found: " << e.what());
    response.setStatus(HTTPServerResponse::HTTP_SERVICE_UNAVAILABLE);
    response.send() << "service_not_found: " << e.what();
  }
  catch (const service_unavailable& e)
  {
    LOGWARN("EXCEPTION: service_unavailable: " << e.what());
    response.setStatus(HTTPServerResponse::HTTP_SERVICE_UNAVAILABLE);
    response.send() << "service_unavailable: " << e.what();
  }
  catch (const yami::yami_runtime_error& e)
  {
    LOGWARN("EXCEPTION: yami_runtime_error: " << e.what());
    response.setStatus(HTTPServerResponse::HTTP_SERVICE_UNAVAILABLE);
    response.send() << "yami_runtime_error: " << e.what();
  }
  catch (const runtime_error& e)
  {
    LOGWARN("EXCEPTION: runtime_error: " << e.what());
    response.setStatus(HTTPServerResponse::HTTP_BAD_REQUEST);
    response.send() << "runtime_error: " << e.what();
  }
  catch (const bad_request& e)
  {
    LOGWARN("EXCEPTION: bad_request: " << e.what());
    response.setStatus(HTTPServerResponse::HTTP_BAD_REQUEST);
    response.send() << "bad_request: " << e.what();
  }
  catch (const Poco::Exception& e)
  {
    LOGWARN("EXCEPTION: " << e.displayText());
    response.setStatus(HTTPServerResponse::HTTP_INTERNAL_SERVER_ERROR);
    response.send() << e.displayText();
  }
  catch (const std::exception& e)
  {
    LOGWARN("EXCEPTION: " << e.what() << " request: " << request.getURI());
    response.setStatus(HTTPServerResponse::HTTP_INTERNAL_SERVER_ERROR);
    response.send() << e.what();
  }
}

}