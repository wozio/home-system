#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/config.h"
#include "utils/logger.h"
#include "io/device_types.h"
#include "io/device_int.h"
#include "io/device_float.h"
#include "ios.h"
#include "weekly_schedule.h"

ios::ios()
: home_system::com::service("io-control.devices", false)
{
  LOG(INFO) << "Reading configuration";
  auto &conf = CONFIG.get();
  // first create known (written in configuration) io devices
  if (conf.HasMember("ios"))
  {
    auto &a = conf["ios"];
    for (auto itr = a.Begin(); itr != a.End(); ++itr)
    {
      if (itr->IsObject())
      {
        home_system::io::io_data_type_t data_type;
        std::string name;
        std::string type;
        std::string service;
        long long id;
        home_system::io::io_mode_t mode;

        if (itr->HasMember("type"))
        {
          auto &v = (*itr)["type"];
          if (v.IsString())
          {
            type = v.GetString();
          }
        }
        else
        {
          LOG(WARNING) << "IO definition without mandatory field 'type', ignoring...";
          continue;
        }

        if (itr->HasMember("data_type"))
        {
          auto &v = (*itr)["data_type"];
          if (v.IsInt())
          {
            data_type = static_cast<home_system::io::io_data_type_t>(v.GetInt());
          }
        }
        else
        {
          LOG(WARNING) << "IO definition without mandatory field 'data_type', ignoring...";
          continue;
        }

        if (itr->HasMember("name"))
        {
          auto &v = (*itr)["name"];
          if (v.IsString())
          {
            name = v.GetString();
          }
        }
        else
        {
          LOG(WARNING) << "IO definition without mandatory field 'name', ignoring...";
          continue;
        }

        if (type == "weekly_schedule")
        {
          if (itr->HasMember("triggers"))
          {
            auto& triggers = (*itr)["triggers"];
            std::map<std::string, long long> triggers_map;
            for (auto iitr = triggers.Begin(); iitr != triggers.End(); ++iitr)
            {
              if (iitr->IsObject())
              {
                if (iitr->HasMember("time") && iitr->HasMember("value"))
                {
                  auto t = ((*iitr)["time"]).GetString();
                  auto &v = (*iitr)["value"];
                  switch (data_type)
                  {
                  case home_system::io::io_data_type_t::integer:
                    triggers_map[t] = v.GetInt64();
                    break;
                  case home_system::io::io_data_type_t::double_float:
                    triggers_map[t] = v.GetDouble();
                    break;
                  }
                }
              }
            }
            if (triggers_map.size() > 0)
            {
              try
              {
                LOG(DEBUG) << "Creating weekly schedule IO: " << static_cast<int>(data_type) << " '" << name << "'";
                auto new_io = std::make_shared<home_system::io::device_int>(id, type);
                typedef weekly_schedule<home_system::io::device_int, long long> weekly_sch_int_t;
                auto new_sch = std::make_shared<weekly_sch_int_t>(new_io, triggers_map);
                schedules_.push_back(new_sch);
                io_devices_[name] = new_io;
              }
              catch (const std::runtime_error &e)
              {
                LOG(ERROR) << "Error while creating Weekly Schedule: " << e.what();
              }
            }
            else
            {
              LOG(WARNING) << "Weekly schedule definition without any valid trigger, ignoring";
            }
          }
          else
          {
            LOG(WARNING) << "Weekly schedule definition without triggers object, ignoring";
          }
        }
        else
        {
          if (itr->HasMember("mode"))
          {
            auto &v = (*itr)["mode"];
            if (v.IsInt())
            {
              mode = static_cast<home_system::io::io_mode_t>(v.GetInt());
            }
          }
          else
          {
            mode = home_system::io::io_mode_t::input;
          }
          
          if (itr->HasMember("service"))
          {
            auto &v = (*itr)["service"];
            if (v.IsString())
            {
              service = v.GetString();
            }
          }

          if (itr->HasMember("id"))
          {
            auto &v = (*itr)["id"];
            if (v.IsInt64())
            {
              id = v.GetInt64();
            }
          }
          if (name.length() > 0 && type.length() > 0 && service.length() > 0)
          {
            try
            {
              LOG(DEBUG) << "Creating Remote IO: " << static_cast<int>(data_type) <<
                " \"" << name << "\" " << service << ":" << id;
              home_system::io::device_t new_io;
              if (data_type == home_system::io::io_data_type_t::integer)
              {
                new_io = std::make_shared<home_system::io::device_int>(id, type);
              }
              else if (data_type == home_system::io::io_data_type_t::double_float)
              {
                new_io = std::make_shared<home_system::io::device_float>(id, type);
              }
              io_devices_[name] = new_io;
              io_devices_by_service_[service][id] = new_io;
            }
            catch (const std::runtime_error &e)
            {
              LOG(ERROR) << "Error while creating Remote IO: " << e.what();
            }
          }
          else
          {
            LOG(WARNING) << "Remote IO definition without mandatory field, ignoring...";
            continue;
          }
        }
      }
    }
  }
}

ios::~ios()
{
}

void ios::on_msg(yami::incoming_message &im)
{
  if (im.get_message_name() == "io_value_change")
  {
    // message from IO driver service, meaning that state has changed
    // it is also sent in series after subscription
    auto params = im.get_parameters();

    auto remote_id = params.get_long_long("id");
    auto service_name = params.get_string("name");

    LOG(TRACE) << "IO value change: '" << service_name << "':" << remote_id;

    auto it = io_devices_by_service_.find(service_name);
    if (it != io_devices_by_service_.end())
    {
      auto& ios_in_service_map = it->second;
      auto it2 = ios_in_service_map.find(remote_id);
      if (it2 != ios_in_service_map.end())
      {
        auto rio = it2->second;
        if (rio)
        {
          // IO object will extract value from parameters
          rio->extract_value(params); 
        }
      }
    }
  }
  else if (im.get_message_name() == "io_state_change")
  {
    // message from IO driver service, meaning that state or value
    // has changed
    // it is also sent in series after subscription
    auto params = im.get_parameters();

    auto remote_id = params.get_long_long("id");
    auto service_name = params.get_string("name");
    auto state = params.get_long_long("state");

    LOG(TRACE) << "IO state change: '" << service_name << "':" << remote_id << " state: " << state;

    auto it = io_devices_by_service_.find(service_name);
    if (it != io_devices_by_service_.end())
    {
      auto& ios_in_service_map = it->second;
      auto it2 = ios_in_service_map.find(remote_id);
      if (it2 != ios_in_service_map.end())
      {
        auto rio = it2->second;
        if (rio)
        {
          rio->set_state(state);
        }
      }
    }
  }
}

home_system::io::device_t ios::get(const std::string &name)
{
  return io_devices_.at(name);
}

void ios::kickoff()
{
  // initialize service
  init();

  DISCOVERY.subscribe([this](const std::string &name, bool available)
  {
    if (available)
    {
      // if name begins with 'io.' it means it is IO device
      // driver
      // TODO: will be changed when discovery mechanism will
      // be changed
      if (name.substr(0, 3) == "io.")
      {
        LOG(INFO) << "IO device driver service '" << name << "' discovered, subscribing";
        yami::parameters params;
        params.set_string("name", service::name());
        auto ep = DISCOVERY.get(name);
        AGENT.send_one_way(ep, name, "subscribe", params);
      }
    }
    else
    {
      if (name.substr(0, 3) == "io.")
      {
        // setting state of all IO objects belonging to this
        // service driver as unknown
        auto s = io_devices_by_service_.find(name);
        if (s != io_devices_by_service_.end())
        {
          auto& im = s->second;
          for (auto i : im)
          {
            i.second->set_state(home_system::io::io_state_t::unknown);
          }
        }
      }
    }
  });
}
