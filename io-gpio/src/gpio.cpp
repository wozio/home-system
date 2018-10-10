#include "gpio.h"
#include "utils/logger.h"
#include <fstream>

gpio::gpio(int port, gpio_mode mode)
: home_system::io::device_int(port, "binary_switch"),
  port_(port),
  mode_(mode)
{
  LOG(INFO) << "GPIO driver for GPIO port " << port << " and mode " << static_cast<int>(mode);
  // export GPIO file
  std::stringstream fp;
  fp.str("/sys/class/gpio/export");
  std::ofstream f(fp.str());
  if (!f.is_open())
  {
    LOG(ERROR) << "Unable to open GPIO export file: " << fp.str();
    throw std::runtime_error("Unable to open GPIO export file");
  }
  f << port;
  f.close();

  // set GPIO mode (direction)
  fp.str("");
  fp << "/sys/class/gpio/gpio" << port << "/direction";
  f.open(fp.str());
  if (!f.is_open())
  {
    LOG(ERROR) << "Unable to open GPIO direction file: " << fp.str();
    throw std::runtime_error("Unable to open GPIO direction file");
  }
  f << (mode == gpio_mode::input ? "in" : "out");
  f.close();
}

gpio::~gpio()
{
}

void gpio::exec_value_change()
{
  if (mode_ == gpio_mode::output)
  {
    // set GPIO value
    std::stringstream fp;
    fp.str("/sys/class/gpio/gpio");
    fp << port_ << "/value";
    std::ofstream f(fp.str());
    if (!f.is_open())
    {
      LOG(ERROR) << "Unable to open GPIO value file: " << fp.str();
      throw std::runtime_error("Unable to open GPIO value file");
    }
    f << (get_wanted_value() == 0 ? "0" : "1");
    f.close();
  }
}