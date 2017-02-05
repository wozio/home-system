#pragma once

namespace home_system
{
class ios;

typedef std::unique_ptr<ios> ios_t;

class ios
{
public:
    static ios_t create()
    {
        return ios_t(new ios());
    }
    ios();
    ~ios();
private:
};

}