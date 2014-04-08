#ifndef OWNETWORK_H
#define	OWNETWORK_H

#include "timer.h"
#include "owtemp.h"
#include <string>
typedef unsigned char uchar;

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
  
  void get_input_history(int input, std::vector<double>& history);
  
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
  std::vector<temp> devices_;
};

}
}
}


#endif	/* OWNETWORK_H */

