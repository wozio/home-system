#ifndef SYSTEM_H
#define	SYSTEM_H

#include "handler.h"

namespace home_system
{
  
class system
: public handler
{
public:
  system(ws_t ws);
  system(const system& orig) = delete;
  ~system();
  
  void on_read(data_t data, size_t data_size);
  
  void shutdown();
  std::string name();
  
private:
};

}

#endif	/* SYSTEM_H */

