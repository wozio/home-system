#pragma once

#include "io/device.h"

class schedule
{
public:
  schedule(std::shared_ptr<home_system::io::device> device);
  std::shared_ptr<home_system::io::device> get_device();
  virtual void kickoff() = 0;
private:
  std::shared_ptr<home_system::io::device> device_;
};