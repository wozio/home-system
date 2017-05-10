#pragma once

#include <boost/any.hpp>
#include <boost/signals2.hpp>
#include <memory>

namespace home_system
{

enum class io_data_type_t
{
    double_float, // 32 bit double float
    integer       // 32 bit signed integer
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

class io_device
{
public:
  io_device(io_id_t id, io_data_type_t data_type, const std::string& type);
  virtual ~io_device();

  io_id_t get_id();
  io_data_type_t get_data_type();
  std::string get_type();

  io_state_t get_state();
  void set_state(io_state_t state);

  boost::any& get_value();
  virtual void set_wanted_value(const boost::any& v);

  boost::signals2::signal<void (io_id_t id)> on_state_change;

protected:
  void set_value(boost::any& value);

private:
  const io_id_t id_;
  const io_data_type_t data_type_;
  const std::string type_;
  io_state_t state_;
  boost::any value_;
  boost::any wanted_value_;
};

}
