#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"

namespace home_system
{

class client
: public handler
{
public:
  client(ws_t ws);
  client(const client& orig) = delete;
  ~client();
  
  void on_read(data_t data, size_t data_size);
private:
};

typdedef std::shared_ptr<client> client_t;

}

#endif	/* CLIENT_H */

