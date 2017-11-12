#pragma once

#include "rule.h"
#include <map>

class rules;

typedef std::unique_ptr<rules> rules_t;

class rules
{
public:
    static rules_t create()
    {
        return rules_t(new rules());
    }
    rules();
    ~rules();
private:
    home_system::utils::ios_wrapper ios_;
    // rules are keyed by name
    typedef std::shared_ptr<rule> rule_t;
    std::map<std::string, rule_t> rules_;

};
