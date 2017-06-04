#pragma once

#include <memory>
#include <string>
#include <vector>

class rule;

typedef std::shared_ptr<rule> rule_t;

class rule
{
public:
    rule(const std::string& name,
        const std::string& script,
        const std::vector<std::string>& triggers);
    ~rule();
private:

    void exec();

};
