#include "weekly_schedule.h"

weekly_schedule::weekly_schedule(
    home_system::io::io_data_type_t data_type,
    const std::string &name,
    const rapidjson::Value &triggers)
    : io(data_type, "weekly_schedule",
        home_system::io::io_mode_t::input, name)
{
    // read configuration
    if (triggers.IsArray())
    {
        for (auto itr = triggers.Begin(); itr != triggers.End(); ++itr)
        {
            if (itr->IsObject())
            {
                int time_of_week;
                if (itr->HasMember("time_of_week"))
                {
                    auto &v = (*itr)["time_of_week"];
                    if (v.IsInt())
                    {
                        time_of_week = v.GetInt();
                    }
                }
                else
                {
                    LOG(WARNING) << "Incomplete trigger, no time of week";
                    continue;
                }
                boost::any value;
                if (itr->HasMember("value"))
                {
                    auto &v = (*itr)["value"];
                    if (v.IsInt())
                    {
                        value = v.GetInt();
                    }
                }
                else
                {
                    LOG(WARNING) << "Incomplete trigger, no value";
                    continue;
                }
                triggers_[time_of_week] = value;
            }
        }
    }
    set_state(home_system::io::io_state_t::ok);
    set_timer();
}

void weekly_schedule::set_timer()
{
    timer_.set_from_now(100, [this] (){
        on_timer();
    });
}

void weekly_schedule::on_timer()
{
    set_timer();

    if (triggers_.size() > 0)
    {
        // first find out what is the time now from begining of the week
        time_t rawtime;
        time(&rawtime);
        tm* ti = localtime(&rawtime);
        int ct = ti->tm_wday * 24 * 3600 + ti->tm_hour * 3600 + ti->tm_min * 60 + ti->tm_sec;

        auto t = triggers_.upper_bound(ct);
        if (t == triggers_.begin())
        {
            t = triggers_.end();
        }
        t--;
        check_value(t->second);
    }
}
