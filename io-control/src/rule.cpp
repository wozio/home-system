#include "utils/logger.h"
#include "rule.h"
#include "ios.h"

extern ios_t _ios;

rule::rule(const std::string& name,
    const std::string& script,
    const std::vector<std::string>& triggers)
{
    LOG(INFO) << "Creating '" << name << "' rule";

    // registering for callbacks in triggers
    for (const auto& trigger : triggers)
    {
        try
        {
            auto t = _ios->get(trigger);
            t->on_value_state_change.connect([this] (io_t io){
                LOG(DEBUG) << "Triggered from \"" << io->get_name() << '"';
            });
        }
        catch (const std::out_of_range e)
        {
            LOG(ERROR) << "Trigger is not defined: " << trigger;
        }
    }
}

rule::~rule()
{

}

void rule::exec()
{

}
