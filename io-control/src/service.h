#pragma once

#include <memory>

class service;

typedef std::shared_ptr<service> service_t;

class service
{
  public:
    service();
    ~service();

  private:
};
