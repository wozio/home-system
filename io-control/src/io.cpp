#include "stdafx.h"
#include "io.h"
#include "io_in_float.h"
#include "logger.h"

namespace home_system
{
    io_t io::create(const std::string& type, const std::string& name, const std::string& service, long long id)
    {
      LOG(DEBUG) << "Creating IO: " << type << ' "' << name << '" ' << service << ":" << id;
      if (type == "temperature")
      {
        return io_t(new io_in_float(type, name, service, id));
      }
      else
      {
        throw std::runtime_error("Unsupported io type");
      }
    }

    io::io(const std::string& type, const std::string& name, const std::string& service, long long id)
    : type_(type),
      name_(name),
      service_(service),
      id_(id),
      state_(state_t::unknown)
    {
    }

    io::~io()
    {
    }

    void io::on_value_state_change(const yami::parameters& params)
    {
      state_ = static_cast<state_t>(params.get_long_long("state"));
    }

    void io::write_value_state(yami::parameters& params)
    {
      params.set_long_long("state", static_cast<long long>(state_));
    }
}