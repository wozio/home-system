#include "stdafx.h"
#include "config.h"
#include "service.h"
#include "logger.h"
#include "ios.h"

namespace home_system
{
    ios::ios()
    {
        LOG(INFO) << "IOS: Reading configuration";
        auto& conf = CONFIG.get();
        // first create known (written in configuration) ioses
        if (conf.HasMember("inputs"))
        {
            auto& a = conf["inputs"];
            for (auto itr = a.Begin(); itr != a.End(); ++itr)
            {
                if (itr->IsObject())
                {
					std::string name;
					std::string type;
					std::string service;
					long long id;

                    if (itr->HasMember("name"))
                    {
						auto& v = (*itr)["name"];
						if (v.IsString())
						{
							name = v.GetString();
						}                        
                    }
					if (itr->HasMember("type"))
					{
						auto& v = (*itr)["type"];
						if (v.IsString())
						{
							type = v.GetString();
						}
					}
					if (itr->HasMember("service"))
					{
						auto& v = (*itr)["service"];
						if (v.IsString())
						{
							service = v.GetString();
						}
					}
					if (itr->HasMember("id"))
					{
						auto& v = (*itr)["id"];
						if (v.IsInt64())
						{
							id = v.GetInt64();
						}
					}
					else
					{
						continue;
					}
					if (name.length() > 0 && type.length() > 0 && service.length() > 0)
					{
						if (type == "temp")
						{
							LOG(DEBUG) << "IOS: Creating input: " << name;
						}
					}
                }
            }
        }
    }

    ios::~ios()
    {

    }
}