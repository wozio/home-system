#include "schedule.h"

schedule::schedule(std::shared_ptr<home_system::io::device> device)
: device_(device)
{
}

std::shared_ptr<home_system::io::device> schedule::get_device()
{
 return device_;
}
