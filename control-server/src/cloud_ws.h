#ifndef CLOUD_WS_H
#define	CLOUD_WS_H

#include <thread>

namespace home_system
{
  
class cloud_ws
{
public:
  cloud_ws();
  cloud_ws(const cloud_ws& orig) = delete;
  ~cloud_ws();
private:
  bool run_thread_;
  std::thread thr_;
  void thr_exec();
};

}

#endif	/* CLOUD_WS_H */
