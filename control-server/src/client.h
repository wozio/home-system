#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"
#include <yami4-cpp/parameters.h>

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
  void add(ws_t ws);

protected:
  void handle_data(data_t data, size_t data_size);

  virtual bool is_logged_in(const std::string& client);
  virtual void login(const std::string& client);
  virtual void logout(const std::string& client);

private:
  void handle_login(const yami::parameters& params, long long sequence_number,
      const std::string& source, const std::string& target);
  void handle_logout(const std::string& source);

  std::string client_id_;
};

}

#endif	/* CLIENT_H */

