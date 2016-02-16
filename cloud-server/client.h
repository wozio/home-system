#ifndef CLIENT_H
#define	CLIENT_H

#include "system.h"

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
  
  void shutdown();
  
private:
  
  system_t system_;
  
};

}

#endif	/* CLIENT_H */

