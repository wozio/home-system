#include "frontend.h"
#include "logger.h"

#include <linux/dvb/frontend.h>

#include <iomanip> 
#include <sstream>

using namespace std;

namespace home_system
{
namespace media
{

frontend::frontend(int adapter, int frontend)
: adapter_(adapter),
  frontend_(frontend),
  state_(frontend_state::closed)
{
  open_adapter();
}

frontend::~frontend()
{
  timer_.cancel();
}

frontend_state frontend::get_state()
{
  return state_;
}

void frontend::set_state_callback(state_callback_t callback)
{
  state_callback_ = callback;
}

void frontend::change_state(frontend_state new_state)
{
  timer_.cancel();
  state_ = new_state;
  if (state_callback_ != nullptr)
  {
    state_callback_(state_);
  }
}

void frontend::set_timer(int duration)
{
  timer_.set_from_now(duration, [this] () {
    check();});
}

void frontend::check()
{
  switch (state_)
  {
  case frontend_state::closed:
    open_adapter();
    break;
    
  case frontend_state::opened:
    break;

  case frontend_state::tunning:
    check_tunning();
    break;

  case frontend_state::tuned:
    check_signal();
    break;
  }
}
  
void frontend::open_adapter()
{
  static bool fault_logged = false;
  char an[200];
  sprintf(an, "/dev/dvb/adapter%d/frontend0", adapter_);
  fe_ = open(an, O_RDWR|O_NONBLOCK);
  if (fe_ < 0)
  {
    if (!fault_logged)
    {
      LOGWARN("Unable to open adapter, keep trying...");
      fault_logged = true;
    }
    
    set_timer(1000);
    return;
  }
  
  dvb_frontend_info result;
  memset(&result, 0, sizeof(result));
  ioctl(fe_, FE_GET_INFO, &result);
  
  LOG("Opened frontend: " << result.name);
  
  change_state(frontend_state::opened);
}

void frontend::set_transponder(std::shared_ptr<transponder> transponder)
{
  if (state_ != frontend_state::closed)
  {
    LOG("Setting to transponder: " << transponder);
    transponder_ = transponder;
    change_state(frontend_state::tunning);
    transponder->tune(fe_);
    set_timer(100);
  }
}

std::shared_ptr<transponder> frontend::get_transponder()
{
  return transponder_;
}

void frontend::check_tunning()
{
  fe_status_t status;
  
  if ((ioctl(fe_, FE_READ_STATUS, &status)) == -1)
  {
    LOGWARN("FE_READ_STATUS failed");
  }
  else
  {
    if (status & FE_HAS_LOCK)
    {
      LOG("We have lock");
      change_state(frontend_state::tuned);
//      set_timer(5000);
    }
    else
    {
      set_timer(100);
    }
  }
}

void frontend::check_signal()
{
  ostringstream str;
  fe_status_t status;

  if ((ioctl(fe_, FE_READ_STATUS, &status)) == -1)
  {
    LOGWARN("FE_READ_STATUS failed");
  }

  str << "status: " <<
    (status & FE_HAS_SIGNAL ? "S" : "_") <<
    (status & FE_HAS_CARRIER ? "C" : "_") <<
    (status & FE_HAS_VITERBI ? "V" : "_") <<
    (status & FE_HAS_SYNC ? "Y" : "_") <<
    (status & FE_HAS_LOCK ? "L" : "_");

  int16_t strength;
  if ((ioctl(fe_, FE_READ_SIGNAL_STRENGTH, &strength)) == -1)
  {
    LOGWARN("FE_READ_SIGNAL_STRENGTH failed");
  }

  if (status & FE_HAS_SIGNAL)
  {
    int16_t snr;
    if ((ioctl(fe_, FE_READ_SNR, &snr)) == -1)
    {
      LOGWARN("FE_READ_SNR failed");
    }

    str << " sig: " << hex << showbase << setw(4) << setfill('0') << strength <<
    " snr: " << hex << showbase << setw(4) << setfill('0') << snr;
  }

  if (status & FE_HAS_VITERBI)
  {
    uint32_t ber;
    if ((ioctl(fe_, FE_READ_BER, &ber)) == -1)
    {
      LOGWARN("FE_READ_BER failed");
    }

    int32_t ublocks;
    if ((ioctl(fe_, FE_READ_UNCORRECTED_BLOCKS, &ublocks)) == -1)
    {
      LOGWARN("FE_READ_UNCORRECTED_BLOCKS failed");
    }

    str << " ber: " << hex << showbase << setw(4) << setfill('0') << ber <<
      " unc: " << hex << showbase << setw(4) << setfill('0') << ublocks;
  }
  LOG(str.str());
  
//  set_timer(5000);
}

}
}
