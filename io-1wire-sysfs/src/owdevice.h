#pragma once

#include <cstdint>
#include <boost/filesystem.hpp>

class owdevice
{
public:
  owdevice(uint64_t serial_num, boost::filesystem::path dev_path);
  ~owdevice();

  // called every second
  virtual void process() = 0;

protected:
  uint64_t serial_num_;
  boost::filesystem::path dev_path_;

private:
};
