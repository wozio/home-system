#ifndef REQUEST_HANDLER_FACTORIES_H
#define	REQUEST_HANDLER_FACTORIES_H

#include <Poco/Net/HTTPRequestHandler.h>
#include <Poco/Net/HTTPRequestHandlerFactory.h>
#include <Poco/Net/HTTPServerRequest.h>

namespace home_system
{

class client_request_handler_factory : public Poco::Net::HTTPRequestHandlerFactory
{
public:
  client_request_handler_factory();
  Poco::Net::HTTPRequestHandler* createRequestHandler(
    const Poco::Net::HTTPServerRequest& request);

private:
};

class system_request_handler_factory : public Poco::Net::HTTPRequestHandlerFactory
{
public:
  system_request_handler_factory();
  Poco::Net::HTTPRequestHandler* createRequestHandler(
    const Poco::Net::HTTPServerRequest& request);

private:
};

}

#endif	/* REQUEST_HANDLER_FACTORIES_H */

