#ifndef CLIENT_H
#define	CLIENT_H

#include "handler.h"
#include "client_service.h"
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

protected:
  void handle_data(data_t data, size_t data_size);

  void reject(bool expect_reply, long long sequence_number,
      const std::string& target, const std::string& source, const std::string& reason);
  void reject(bool expect_reply, long long sequence_number,
      const std::string& target, const std::string& source, const char* reason);

  virtual bool is_logged_in(const std::string& client);
  virtual void login(const std::string& client);

private:
  void handle_login(const yami::parameters& params, long long sequence_number,
      const std::string& source, const std::string& target);

  std::string create_client(const std::string& name);

  typedef std::shared_ptr<client_service> client_service_t;
  static std::map<std::string, client_service_t> clients_;
};

}

#endif	/* CLIENT_H */

