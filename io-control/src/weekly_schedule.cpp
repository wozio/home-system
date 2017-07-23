#include "weekly_schedule.h"

weekly_schedule::weekly_schedule()
{
    set_timer();
}

void weekly_schedule::set_timer()
{
    timer_.set_from_now(1000, [this] (){
        on_timer();
    });
}