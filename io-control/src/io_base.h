#pragma once

#include <boost/any.hpp>
#include <boost/signals2.hpp>
#include <string>
#include <memory>

class io_base
{
    public:
        io_base(const std::string &name);

        // called every time when value or state is updated
        boost::signals2::signal<void (io_t)> on_value_state_change;
    private:

        const std::string name_;

    
};
