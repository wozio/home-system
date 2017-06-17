#include "utils/config.h"
#include "utils/logger.h"
#include "rules.h"

rules::rules()
{
    LOG(INFO) << "Reading configuration";
    auto &conf = CONFIG.get();
    auto i = conf.FindMember("rules");
    if (i != conf.MemberEnd() && i->value.IsArray())
    {
        const auto& arr = i->value;
        for (auto j = arr.Begin(); j != arr.End(); ++j)
        {
            if (j->IsObject())
            {
                // getting rule name
                std::string name;
                const auto& v = j->FindMember("name");
                if (v != j->MemberEnd() && v->value.IsString())
                {
                    name = v->value.GetString();
                }
                else
                {
                    LOG(WARNING) << "Rule name not found or incorrect, ignoring";
                    continue;
                }

                // getting rule script, for now in string
                /// TODO: file based rule script
                std::string rule_script;
                const auto& vs = j->FindMember("rule");
                if (vs != j->MemberEnd() && vs->value.IsString())
                {
                    rule_script = vs->value.GetString();
                }
                else
                {
                    LOG(WARNING) << "Rule script not found or incorrect, ignoring rule";
                    continue;
                }

                // getting triggers
                std::vector<std::string> triggers;
                const auto& ta = j->FindMember("triggers");
                if (ta != j->MemberEnd() && ta->value.IsArray())
                {
                    for (auto ti = ta->value.Begin(); ti != ta->value.End(); ++ti)
                    {
                        if (ti->IsString())
                        {
                            triggers.push_back(ti->GetString());
                        }
                    }
                }
                try
                {
                    rule_t r = std::make_shared<rule>(name, rule_script, triggers);
                }
                catch (const std::exception e)
                {
                    LOG(ERROR) << "Error creating rule";
                }
            }
        }
    }
}

rules::~rules()
{
}
