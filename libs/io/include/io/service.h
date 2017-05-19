#pragma once

#include "device.h"
#include "com/service.h"
#include <mutex>
#include <set>

namespace home_system
{
namespace io
{
    
  class service
  : public home_system::com::service
  {
  public:
    service(const std::string& name);
    ~service();

    void on_msg(yami::incoming_message & im);

    void add_device(device_t device);
    void remove_device(io_id_t id);
    void clear_devices();

    void on_device_change(io_id_t device_id);

  private:
    std::mutex subscription_mutex_;

    std::map<io_id_t, device_t> devices_;

    // service name
    std::set<std::string> subscriptions_;
    void send_current_state();
  };

}
}