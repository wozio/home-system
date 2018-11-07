#pragma once

#include <cstdint>
#include <string>

class owdevice
{
public:
  owdevice(uint64_t serial_num, const std::string& dev_path);
  ~owdevice();

  // called every second
  virtual void process() = 0;

protected:
  uint64_t serial_num_;
  std::string dev_path_;

private:
};
