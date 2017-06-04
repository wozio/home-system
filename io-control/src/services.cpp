#include "utils/config.h"
#include "com/service.h"
#include "utils/logger.h"
#include "services.h"

services::services()
{
    LOG(INFO) << "Reading configuration";
    auto &conf = CONFIG.get();
    if (conf.HasMember("services"))
    {
        auto &a = conf["services"];
        for (auto itr = a.Begin(); itr != a.End(); ++itr)
        {
            if (itr->IsObject())
            {

            }
        }
    }
}

services::~services()
{
}
