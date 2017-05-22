#include "stdafx.h"
#include "utils/config.h"
#include "com/service.h"
#include "utils/logger.h"
#include "ios.h"
#include "io.h"

ios::ios()
{
	LOG(INFO) << "IOS: Reading configuration";
	auto &conf = CONFIG.get();
	// first create known (written in configuration) ioses
	if (conf.HasMember("ios"))
	{
		auto &a = conf["ios"];
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
					continue;
				}
				if (name.length() > 0 && type.length() > 0 && service.length() > 0)
				{
					try
					{
						io::create(type, name, service, id);
					}
					catch (const std::runtime_error &e)
					{
						LOG(ERROR) << "Error while creating IO: " << e.what();
					}
				}
			}
		}
	}
}

ios::~ios()
{
}
