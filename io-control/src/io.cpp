#include "stdafx.h"
#include "io.h"

namespace home_system
{
    io_t io::create(const std::string& type, const std::string& name, const std::string& service, long long id)
    {

    }

    io::io(const std::string& type, const std::string& name, const std::string& service, long long id)
    : type_(type),
      name_(name),
      service_(service),
      id_(id)
    {

    }

    io::~io()
    {

    }
}