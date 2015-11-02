#include "http.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include "file_request_handler.h"
#include "json_converter.h"
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/URI.h>

using Poco::Net::HTTPRequestHandler;
using Poco::Net::HTTPServerRequest;
using Poco::Net::HTTPServerResponse;

using namespace std;
using namespace Poco;

namespace home_system
{
  
class request_handler : public HTTPRequestHandler
{
public:
  request_handler()
  {
  }

  void handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
  {
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
        home_system::Handler handler(params);
        rapidjson::Reader reader;
        home_system::stream ss(request.stream());
        reader.Parse(ss, handler);
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

private:
  // exceptions

  class bad_request : public std::exception
  {
  };

  class service_unavailable : public std::exception
  {
  public:

    service_unavailable(const string& reason) throw ()
    : reason_(reason)
    {
    }

    ~service_unavailable() throw ()
    {
    }

    const char* what() const throw ()
    {
      return reason_.c_str();
    }
  private:
    string reason_;
  };
};

request_handler_factory::request_handler_factory(const std::string& root)
: root_(root)
{
  fill_media_types();
}

HTTPRequestHandler* request_handler_factory::createRequestHandler(const HTTPServerRequest& request)
{
  string uri = request.getURI();
  LOG("Request: " << request.clientAddress().toString() << " URI: " << uri);
  if (uri == "/access/" || uri == "/access")
  {
    return new request_handler();
  }
  else
  {
    return new file_request_handler(root_);
  }
}

}
