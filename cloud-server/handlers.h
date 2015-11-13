#ifndef HANDLERS_H
#define	HANDLERS_H
#include "handler.h"
#include "ios_wrapper.h"
#include <memory>

namespace home_system
{

class handlers;

typedef std::unique_ptr<handlers> handlers_t;
typedef std::shared_ptr<handler> handler_t;

class handlers {
public:
  handlers(const handlers& orig) = delete;
  ~handlers();
  
  static handlers_t create()
  {
    return handlers_t(new handlers());
  };
  
  void add(handler_t handler);
  
private:
  handlers();
  
  ios_wrapper ios_;
  
  void read(handler_t handler);

};

}

extern home_system::handlers_t _handlers;

#define HANDLERS (*::_handlers)

#endif	/* WS_HANDLER_H */

