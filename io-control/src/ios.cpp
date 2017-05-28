#include "utils/config.h"
#include "com/service.h"
#include "utils/logger.h"
#include "io/device_types.h"
#include "ios.h"

ios::ios()
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
                        io_devices_[id] = new_io;
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
}

ios::~ios()
{
}
