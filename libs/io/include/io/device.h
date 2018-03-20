#pragma once

#include "device_types.h"
#include "yami4-cpp/parameters.h"
#include <boost/any.hpp>
#include <boost/signals2.hpp>
#include <memory>

namespace home_system
{
namespace io
{

class device
{
public:
  device(io_id_t id, io_data_type_t data_type, const std::string& type);
  virtual ~device();

  io_id_t get_id();
  io_data_type_t get_data_type();
  std::string get_type();

  io_state_t get_state();
  void set_state(io_state_t state);
  void set_state(long long state);

  boost::signals2::signal<void (io_id_t id)> on_state_change;
  boost::signals2::signal<void (io_id_t id)> on_value_change;
  boost::signals2::signal<void (io_id_t id)> on_wanted_value_change;

  virtual void extract_value(const yami::parameters& params) = 0;
  virtual void extract_wanted_value(const yami::parameters& params) = 0;
  virtual void write_value(yami::parameters& params) = 0;

private:
  const io_id_t id_;
  const io_data_type_t data_type_;
  const std::string type_;
  io_state_t state_;
};

typedef std::shared_ptr<device> device_t;

}
}