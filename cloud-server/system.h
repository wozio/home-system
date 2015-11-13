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
  
private:
};

}

#endif	/* SYSTEM_H */

