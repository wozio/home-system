#pragma once

#include <boost/any.hpp>
#include <cstdint>

namespace home_system
{

class device
{
public:
  device(int portnum, uint64_t serial_num);
  virtual ~device();

  uint64_t get_id();

  enum state_t
  {
      unknown = -1,
      ok,
      faulty
  };
  state_t get_state();
  void set_state(state_t state);

  // called every second
  virtual void process() = 0;

  boost::any get_value();

protected:
  void set_value(boost::any& value);
  
  int portnum_;
  uint64_t serial_num_;

private:
  
  state_t state_;
  boost::any value_;
};

}
