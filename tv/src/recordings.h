#ifndef RECORDINGS_H
#define	RECORDINGS_H

#include "timer.h"

namespace home_system
{
namespace media
{

class recordings
{
public:
  recordings();
  ~recordings();
  recordings(const recordings&) = delete;
  
  void check();
  int handle_schedule_event_recording();
  
private:
  timer timer_;
  void on_timeout();
  
  void start(int recording_id);
  void stop(int recording_id);
};

}
}

#endif	/* RECORDINGS_H */

