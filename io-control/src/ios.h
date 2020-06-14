#pragma once

#include "ios_lua_callbacks.h"
#include "io/device.h"
#include "com/service.h"
#include "schedule.h"
#include <map>
#include <memory>

class ios;
typedef std::unique_ptr<ios> ios_t;
typedef std::shared_ptr<schedule> schedule_t;

/// Owner of all IO devices objects and service for
/// subscribing for notifications from IO device drivers
/// which are passed to proper objects.
/// Done this way to avoid service in each IO device object.
/// Entry point for subscriptions from clients
class ios
: public home_system::com::service
{
public:
  static ios_t create(lua_State *lua)
  {
    return ios_t(new ios(lua));
  }
  ios(lua_State *lua);
  ~ios();

  void init();

  void add_remote_io(const std::string& service, home_system::io::io_id_t id,
    home_system::io::device_t device);

  void add(home_system::io::io_id_t id, home_system::io::device_t device, const std::string& name);
  home_system::io::device_t get(home_system::io::io_id_t id);
  home_system::io::io_id_t get(const std::string& name);

  void add_interval_schedule(home_system::io::io_id_t id, schedule_t schedule);
  schedule_t get_interval_schedule(home_system::io::io_id_t id);

  void add_weekly_schedule(home_system::io::io_id_t id, schedule_t schedule);
  schedule_t get_weekly_schedule(home_system::io::io_id_t id);

  void kickoff();

private:
  lua_State *lua_;
  // IO devices keyed by id
  std::map<home_system::io::io_id_t, home_system::io::device_t> io_devices_;
  // name to ID map
  std::map<std::string, home_system::io::io_id_t> io_devices_by_name_;

  // Remote IO devices keyed by
  // service name and remote id
  typedef std::map<std::string, std::map<long long, home_system::io::device_t>> io_devices_by_service_t;
  io_devices_by_service_t io_devices_by_service_;

  // schedules keyed by id
  std::map<home_system::io::io_id_t, schedule_t> interval_schedules_;
  std::map<home_system::io::io_id_t, schedule_t> weekly_schedules_;

  void on_msg(yami::incoming_message &im);
};
