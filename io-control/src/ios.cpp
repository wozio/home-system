#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/config.h"
#include "utils/logger.h"
#include "io/device_types.h"
#include "ios.h"
#include "io_remote.h"
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
                        rapidjson::Value& triggers = (*itr)["triggers"];
                        try
                        {
                            LOG(DEBUG) << "Creating schedule: " << static_cast<int>(data_type) << " \"" << name << "\"";
                            auto new_io = std::make_shared<weekly_schedule>(data_type, name, triggers);
                            io_devices_[name] = new_io;
                        }
                        catch (const std::runtime_error &e)
                        {
                            LOG(ERROR) << "Error while creating Remote IO: " << e.what();
                        }
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
                            LOG(DEBUG) << "Creating Remote IO: " << static_cast<int>(data_type) << " " << type << " \"" << name << "\" " << service << ":" << id;
                            auto new_io = std::make_shared<io_remote>(data_type, type, mode, name, service, id);
                            io_devices_[name] = new_io;
                            auto id_string = service + std::to_string(id);
                            io_devices_by_id_[id_string] = new_io;
                            io_devices_by_service_.insert(io_devices_by_service_t::value_type(service, new_io));
                        }
                        catch (const std::runtime_error &e)
                        {
                            LOG(ERROR) << "Error while creating Remote IO: " << e.what();
                        }
                    }
                    else
                    {
                        LOG(WARNING) << "Remote IO definition without mandatory field, ignoring...";
                    }
                }
            }
        }
    }

    // initialize service
    init();

    DISCOVERY.subscribe([this](const std::string &name, bool available) {
        if (available)
        {
            // if name begins with 'io.' it means it is IO device
            // driver
            // TODO: will be changed when discovery mechanism will
            // be changed
            if (name.substr(0, 3) == "io.")
            {
                LOG(INFO) << "IO device driver service discovered, subscribing";
                yami::parameters params;
                params.set_string("name", service::name());
                auto ep = DISCOVERY.get(name);
                AGENT.send_one_way(ep, name,
                                   "subscribe", params);
            }
        }
        else
        {
            if (name.substr(0, 3) == "io.")
            {
                // setting state of all IO objects belonging to this
                // service driver as unknown
                auto r = io_devices_by_service_.equal_range(name);
                for (auto i = r.first; i != r.second; ++i)
                {
                    i->second->set_state(home_system::io::io_state_t::unknown);
                }
            }
        }
    });
}

ios::~ios()
{
}

void ios::on_msg(yami::incoming_message &im)
{
    if (im.get_message_name() == "io_change")
    {
        // message from IO driver service, meaning that state or value
        // has changed
        // it is also sent in series after subscription
        auto params = im.get_parameters();

        auto remote_id = params.get_long_long("id");
        auto service_name = params.get_string("name");
        auto id_string = service_name + ":" + std::to_string(remote_id);

        auto it = io_devices_by_id_.find(id_string);
        if (it != io_devices_by_id_.end())
        {
            auto rio = std::dynamic_pointer_cast<io_remote>(it->second);
            if (rio)
            {
                LOG(DEBUG) << "IO \"" << rio->get_name() << "\" updated";

                // IO object will extract value and state from parameters
                rio->extract_value_state(params);
            }
        }
        else
        {
            LOG(TRACE) << "Update from unknown IO device, ignoring (" << id_string << ")";
        }
    }
}

io_t ios::get(const std::string &name)
{
    return io_devices_.at(name);
}
