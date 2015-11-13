#include "handlers.h"
#include "logger.h"
#include <utility>

using namespace std;

namespace home_system
{

handlers::handlers()
{
}

handlers::~handlers()
{
  
}

void handlers::add(handler_t handler)
{
  // start reading from assiociated websocket
  ios_.io_service().post([handler, this] ()
    {
      this->read(handler);
    }
  );
}

void handlers::read(handler_t handler)
{
  auto data = create_data();
  int n = handler->read(data);
  ios_.io_service().post([this, handler, data, n] ()
    {
      this->send(handler, data, n);
    }
  );
  ios_.io_service().post([this, handler] ()
    {
      this->read(handler);
    }
  );
}

void handlers::send(handler_t handler, data_t data, int data_size)
{
  handler->send(data, data_size);
}

}
