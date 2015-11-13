#ifndef HANDLER_H
#define	HANDLER_H

#include <Poco/Net/WebSocket.h>
#include <memory>

namespace home_system
{

typedef std::shared_ptr<Poco::Net::WebSocket> ws_t;

class handler
{
public:
  handler(ws_t ws);
  handler(const handler& orig) = delete;
  virtual ~handler();
  
protected:
  size_t read(std::unique_ptr<char[]>& data, size_t data_size);
  void send(std::unique_ptr<char[]>& data, size_t data_size);
  
private:
  ws_t ws_;
};

}

#endif	/* HANDLER_H */

