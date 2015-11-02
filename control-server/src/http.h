#ifndef HTTP_H
#define	HTTP_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/HTTPRequestHandlerFactory.h>
#include <Poco/Net/HTTPServerRequest.h>
#include <string>

namespace home_system
{

class request_handler_factory : public Poco::Net::HTTPRequestHandlerFactory
{
public:
  request_handler_factory(const std::string& root);
  Poco::Net::HTTPRequestHandler* createRequestHandler(
    const Poco::Net::HTTPServerRequest& request);

private:
  std::string root_;
};

}

#endif	/* HTTP_H */

