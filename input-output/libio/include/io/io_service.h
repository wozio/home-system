#pragma once

#include "io/io_device.h"
#include "service.h"
#include <mutex>
#include <set>

namespace home_system
{
    
  class io_service
  : public home_system::service
  {
  public:
    io_service(const std::string& name);
    ~io_service();

    void on_msg(yami::incoming_message & im);

    void add_device(io_device_t device);
    void remove_device(io_id_t id);
    void clear_devices();

    void on_device_change(io_id_t device_id);

  private:
    std::mutex subscription_mutex_;

    std::map<io_id_t, io_device_t> devices_;

    // service name
    std::set<std::string> subscriptions_;
    void send_current_state();
  };

}
