#include "gpios.h"
#include "utils/logger.h"
#include <thread>
#include <iostream>

gpios::gpios(const std::string &name, const std::vector<int>& ports, const std::vector<int>& modes)
: ioservice_(name)
{
  for (auto i = 0; i < ports.size(); ++i)
  {
    gpio_t g(new gpio(ports[i], modes[i] == 0 ? gpio_mode::output : gpio_mode::input));
    ioservice_.add_device(g);
    gpios_.push_back(g);
    g->set_wanted_value(0);
  }
}

gpios::~gpios()
{
}
