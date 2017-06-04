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
    // rules are keyed by name
    std::map<std::string, rule_t> rules_;

};
