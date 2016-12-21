#ifndef HTTP_H
#define	HTTP_H

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

