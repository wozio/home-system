#include "handlers.h"
#include "logger.h"

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
  LOG("Reading data...");
  unique_ptr<char[]> data(new char[1025]);
  int n = handler->read(data, 1024);
  ios_.io_service().post([handler, this] ()
    {
      this->read(handler);
    }
  );
}

}
