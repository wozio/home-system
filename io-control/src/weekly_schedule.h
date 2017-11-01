#pragma once

#include "io.h"
#include "utils/timer.h"
#include <rapidjson/document.h>

class weekly_schedule
: public io
{
public:
    weekly_schedule(
        home_system::io::io_data_type_t data_type,
        const std::string &name,
        const rapidjson::Value &triggers);
    void kickoff();
private:
    home_system::utils::timer timer_;
    std::map<int, boost::any> triggers_;
    void on_timer();
};
