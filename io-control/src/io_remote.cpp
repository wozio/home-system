#include "io_remote.h"

using namespace std;
using namespace home_system::io;

io_remote::io_remote(home_system::io::io_data_type_t data_type,
    const std::string &type,
    home_system::io::io_mode_t mode,
    const std::string &name,
    const std::string &service,
    home_system::io::io_id_t id)
    : io(data_type, type, mode, name),
      service_(service),
      id_(id)
{
}

io_remote::~io_remote()
{
}

void io_remote::extract_value_state(const yami::parameters &params)
{
    // state extract
    auto s = static_cast<io_state_t>(params.get_long_long("state"));
    
    // value extract
    if (s == io_state_t::ok)
    {
        switch (data_type_)
        {
            case home_system::io::io_data_type_t::integer:
            {
                auto nv = params.get_long_long("value");
                check_value(nv);
                break;
            }
            case home_system::io::io_data_type_t::double_float:
            {
                auto nv = params.get_double_float("value");
                check_value(nv);
                break;
            }
        }
    }

    set_state(s);
}
