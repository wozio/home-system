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

protected:
  void handle_data(data_t data, size_t data_size);

  void reject(bool expect_reply, long long sequence_number,
      const std::string& target, const std::string& source, const std::string& reason);
  void reject(bool expect_reply, long long sequence_number,
      const std::string& target, const std::string& source, const char* reason);

  virtual bool is_logged_in(const std::string& source);

private:
  // logged in source
  std::string source_;
  void handle_login(const yami::parameters& params, long long sequence_number,
      const std::string& target);
  std::string create_client(const std::string& name);
};

}

#endif	/* CLIENT_H */

