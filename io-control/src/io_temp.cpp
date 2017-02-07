#include "stdafx.h"
#include "io_temp.h"

namespace home_system
{

    io_temp::io_temp(const std::string& type, const std::string& name, const std::string& service, long long id)
    : io(type, name, service, id)
    {
    }

    io_temp::~io_temp()
    {
    }
}