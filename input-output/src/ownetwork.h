#ifndef OWNETWORK_H
#define	OWNETWORK_H

#include "timer.h"
#include "owtemp.h"
#include <map>
#include <string>
#include <memory>

namespace home_system
{

namespace input_output
{
namespace ow
{

class net
{
public:
  net(const std::string &port);
  net(const net&) = delete;
  ~net();
  
  void get_inputs(std::vector<long long>& ids);
  // TODO: other devices than temperature
  temp& get_input(uint64_t id);
  
private:
  std::string port_;
  
  timer timer_;
  
  bool opened_, open_fault_logged_, search_fault_logged_;
  int portnum_;
  void open();
  void search();
  void send_request();
  void read_temp();
  void close();
  
  // TODO: other devices than temperature
  std::map<uint64_t, temp> devices_;
};

}
}
}


#endif	/* OWNETWORK_H */

