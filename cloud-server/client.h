#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"

class client
: public handler
{
public:
  client(ws_t ws);
  client(const client& orig) = delete;
  ~client();
  
private:
};

#endif	/* CLIENT_H */

