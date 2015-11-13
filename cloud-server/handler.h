#ifndef HANDLER_H
#define	HANDLER_H

#include <Poco/Net/WebSocket.h>
#include <memory>

typedef std::shared_ptr<Poco::Net::WebSocket> ws_t;

class handler
{
public:
  handler(ws_t ws);
  handler(const handler& orig) = delete;
  virtual ~handler();
private:

};

#endif	/* HANDLER_H */

