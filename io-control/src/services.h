#pragma once

#include <memory>

class services;

typedef std::unique_ptr<services> services_t;

class services
{
public:
    static services_t create()
    {
        return services_t(new services());
    }
    services();
    ~services();
private:

};
