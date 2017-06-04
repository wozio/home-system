#pragma once

#include <boost/any.hpp>

namespace home_system
{
namespace io
{

enum class io_data_type_t
{
    double_float = 0, // 32 bit double float
    integer           // 32 bit signed integer
};

template<typename T>
void convert_to(const boost:any& from, T& to)
{
    to = boost::any_cast<T>(from);
}

enum class io_state_t
{
    unknown = 0,
    ok,
    faulty
};

typedef long long io_id_t;

enum class io_mode_t
{
    input = 0,
    output
};

}
}


