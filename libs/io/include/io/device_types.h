#pragma once

#include <boost/any.hpp>

namespace home_system
{
namespace io
{

enum class io_data_type_t
{
    double_float = 0, // 32 bit double float
    integer           // 64 bit signed integer
};

enum class io_state_t
{
    unknown = 0,
    ok,
    faulty
    
};

const char* io_state_to_string(io_state_t s);

typedef long long io_id_t;

enum class io_mode_t
{
    input = 0,
    output
};

}
}


