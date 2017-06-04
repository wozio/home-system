#include "utils/logger.h"
#include "rule.h"
#include "ios.h"

rule::rule(const std::string& name,
    const std::string& script,
    const std::vector<std::string>& triggers)
{
    LOG(INFO) << "Creating '" << name << "' rule";

    // registering for callbacks in triggers
    for (const auto& trigger : triggers)
    {

    }
}

rule::~rule()
{

}

void rule::exec()
{

}
