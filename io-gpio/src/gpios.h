#pragma once

#include "gpio.h"
#include "io/service.h"
#include <string>

typedef std::shared_ptr<gpio> gpio_t;

class gpios
{
public:
  gpios(const std::string &name, const std::vector<int>& ports, const std::vector<int>& modes);
  ~gpios();

private:
  home_system::io::service ioservice_;
  std::vector<gpio_t> gpios_;
};
