#include "handler.h"

namespace home_system
{

class binary_connection
  : public handler
{
public:
  binary_connection(ws_t ws, const std::string& name);
  binary_connection(const binary_connection& orig) = delete;
  ~binary_connection();

  void on_read(data_t data, size_t data_size, type_t data_type);

  const std::string& get_endpoint();

  void operator()(yami::incoming_message & im);

private:
  yami::agent agent_;
  std::string endpoint_;
};

}