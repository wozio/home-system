#include "gpio.h"
#include "utils/logger.h"
#include <fstream>
#include <chrono>
#include <thread>

gpio::gpio(int port, gpio_mode mode)
: home_system::io::device_int(port, "binary_switch"),
  port_(port),
  mode_(mode)
{
  LOG(INFO) << "GPIO driver for GPIO port " << port << " and mode " << static_cast<int>(mode);
  // export GPIO file
  std::string fp("/sys/class/gpio/export");
  std::ofstream f(fp);
  if (!f.is_open())
  {
    LOG(ERROR) << "Unable to open GPIO export file: " << fp;
    throw std::runtime_error("Unable to open GPIO export file");
  }
  f << port;
  f.close();

  // set GPIO mode (direction)
  int tries = 0;
  fp = "/sys/class/gpio/gpio" + std::to_string(port) + "/direction";
  f.open(fp);
  while (!f.is_open())
  {
    LOG(DEBUG) << "Unable to open GPIO direction file: " << fp;
    if (++tries >= 5)
    {
      LOG(ERROR) << "Unable to open GPIO direction file: " << fp;
      throw std::runtime_error("Unable to open GPIO direction file");
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    f.open(fp);
  }
  f << (mode == gpio_mode::input ? "in" : "out");
  f.close();

  set_state(home_system::io::io_state_t::ok);

  read();
}

gpio::~gpio()
{
  LOG(INFO) << "Destroing GPIO driver for GPIO port " << port_;
  // unexport GPIO file
  std::string fp("/sys/class/gpio/unexport");
  std::ofstream f(fp);
  if (f.is_open())
  {
    f << port_;
    f.close();
  }
  else
  {
    LOG(ERROR) << "Unable to open GPIO unexport file: " << fp;
  }
}

void gpio::read()
{
  std::string fp, line;
  fp = "/sys/class/gpio/gpio" + std::to_string(port_) + "/value";
  LOG(TRACE) << "Reading GPIO port value " << port_ << " from file: " << fp;
  std::ifstream f(fp);
  if (!f.is_open())
  {
    LOG(ERROR) << "Unable to open GPIO value file: " << fp;
    throw std::runtime_error("Unable to open GPIO value file");
  }
  int v;
  std::getline(f, line);
  LOG(DEBUG) << line;
  v = std::stoi(line);
  LOG(DEBUG) << v;
  set_value(v);
  f.close();
}

void gpio::exec_value_change()
{
  if (mode_ == gpio_mode::output)
  {
    LOG(TRACE) << "Executing GPIO port value change " << port_;
    // set GPIO value
    std::string fp;
    fp = "/sys/class/gpio/gpio" + std::to_string(port_) + "/value";
    std::ofstream f(fp);
    if (!f.is_open())
    {
      LOG(ERROR) << "Unable to open GPIO value file: " << fp;
      throw std::runtime_error("Unable to open GPIO value file");
    }
    f << (get_wanted_value() == 0 ? "0" : "1");
    f.close();
  }
}