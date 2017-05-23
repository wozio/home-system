#pragma once

#include <functional>

namespace home_system
{
namespace utils
{

class app
{
public:
  app(bool daemonize);
  virtual ~app();

  int run();
  int run(std::function<void()> init_callback, std::function<void()> cleanup_callback);

private:
  bool daemonize_;
};
}
}
