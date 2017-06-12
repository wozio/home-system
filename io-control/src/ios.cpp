#include "com/discovery.h"
#include "com/yamicontainer.h"
#include "utils/config.h"
#include "utils/logger.h"
#include "io/device_types.h"
#include "ios.h"

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

                if (itr->HasMember("name"))
                {
                    auto &v = (*itr)["name"];
                    if (v.IsString())
                    {
                        name = v.GetString();
                    }
                }

                if (itr->HasMember("type"))
                {
                    auto &v = (*itr)["type"];
                    if (v.IsString())
                    {
                        type = v.GetString();
                    }
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
                else
                {
                    LOG(WARNING) << "IO definition without mandatory field 'id', ignoring...";
                    continue;
                }
                if (name.length() > 0 && type.length() > 0 && service.length() > 0)
                {
                    try
                    {
                        LOG(DEBUG) << "Creating IO: " << static_cast<int>(data_type) << " " << type << " \"" << name << "\" " << service << ":" << id;
                        auto new_io = std::make_shared<io>(data_type, type, mode, name, service, id);
                        io_devices_[name] = new_io;
                        auto id_string = service + std::to_string(id);
                        io_devices_by_id_[id_string] = new_io;
                    }
                    catch (const std::runtime_error &e)
                    {
                        LOG(ERROR) << "Error while creating IO: " << e.what();
                    }
                }
                else
                {
                    LOG(WARNING) << "IO definition without mandatory field, ignoring...";
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
            // TODO setting state of all IO objects belonging to this
            // driver as unknown
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
        auto params = im.get_parameters();

        auto remote_id = params.get_long_long("id");
        auto service_name = params.get_string("name");
        auto id_string = service_name + std::to_string(remote_id);

        auto it = io_devices_by_id_.find(id_string);
        if (it != io_devices_by_id_.end())
        {
            LOG(DEBUG) << "IO \"" << it->second->get_name() << "\" updated";

            // IO object will extract value and state from parameters
            it->second->on_value_state_change(params);
        }
        else
        {
            LOG(TRACE) << "Update from unknown IO device, ignoring (" << service_name << ":" << remote_id << ")";
        }
    }
}

io_t ios::get(const std::string &name)
{
    return io_devices_.at(name);
}
