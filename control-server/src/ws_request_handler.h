#ifndef WS_REQUEST_HANDLER_H
#define	WS_REQUEST_HANDLER_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <string>

namespace home_system
{

class ws_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  ws_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);

private:
  // exceptions

  class bad_request : public std::exception
  {
  };

  class service_unavailable : public std::exception
  {
  public:

    service_unavailable(const std::string& reason) throw ()
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
    std::string reason_;
  };
};

}

#endif	/* WS_REQUEST_HANDLER_H */

