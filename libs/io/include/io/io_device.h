#pragma once

#include <boost/any.hpp>
#include <memory>

namespace home_system
{

enum class io_type_t
{
    temperature_input,
    switch_output
};

enum class io_state_t
{
    unknown = 0,
    ok,
    faulty
};

typedef long long io_id_t;

class io_device;
typedef std::shared_ptr<io_device> io_device_t;

class io_service;

class io_device
{
public:
  io_device(io_service& ios, io_id_t id, io_type_t type);
  virtual ~io_device();

  io_id_t get_id();
  io_type_t get_type();

  io_state_t get_state();
  void set_state(io_state_t state);

  boost::any& get_value();

protected:
  void set_value_int(boost::any& value);

private:
  io_service& ios_;
  io_id_t id_;
  io_type_t type_;
  io_state_t state_;
  boost::any value_;
};

}
