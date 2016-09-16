#include "handler.h"

namespace home_system
{

class binary_connection
  : public handler
{
public:
  binary_connection(ws_t ws);
  binary_connection(const binary_connection& orig) = delete;
  ~binary_connection();

  void on_read(data_t data, size_t data_size);

private:

};

}