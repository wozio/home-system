#ifndef WS_REQUEST_HANDLER_H
#define	WS_REQUEST_HANDLER_H

namespace home_system
{

class ws_request_handler : public Poco::Net::HTTPRequestHandler
{
public:
  ws_request_handler(const std::string& client_id);
  ~ws_request_handler();
  void handleRequest(Poco::Net::HTTPServerRequest& request, Poco::Net::HTTPServerResponse& response);

private:
  std::string client_id_;
};

}

#endif	/* WS_REQUEST_HANDLER_H */

