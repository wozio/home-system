#pragma once

#include "ios_wrapper.h"

namespace home_system
{
namespace utils
{

class timer
{
public:
  timer();
  timer(ios_wrapper &ios);
  ~timer();
  timer(const timer &) = delete;
  timer &operator=(const timer &) = delete;

  /**
   * Set timer to timeout after duration calling handler callback
   * @param duration timer duration from now in milliseconds
   * @param handler callback to call on timeout, will not be called when timer is cancelled
   */
  void set_from_now(unsigned int duration, std::function<void()> handler);

  bool is_set();

  void cancel();

  /**
   * Return internal ios_wrapper object.
   * Multiple timers may be created sharing the same ios_wrapper object
   * thus all working on in one thread
   */
  ios_wrapper &get_ios();

private:
  ios_wrapper my_ios_;
  ios_wrapper &ios_;
  boost::asio::deadline_timer dt_;
  bool set_;
};
}
}
