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
        auto conf = CONFIG.get();
        // first create known (written in configuration) ioses
        if (conf.HasMember("inputs"))
        {
            auto a = conf["inputs"];
            for (auto itr = a.Begin(); itr != a.End(); ++itr)
            {
                if (itr->IsObject())
                {
                    if (itr->HasMember("name"))
                    {
                        LOG(DEBUG) << "IOS: Input found: " << (*itr)["name"];
                    }
                }
            }
        }
    }

    ios::~ios()
    {

    }
}