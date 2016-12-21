#ifndef SESSION_CALLBACK_T_H
#define	SESSION_CALLBACK_T_H

#include <functional>

namespace dvb
{

class session_error
: public std::runtime_error
{
public:
  session_error()
  : std::runtime_error("session_error")
  {
  }
};

/**
 * Session event
 */
enum class session_event_t
{
  /**
   * Session is under initialization, requests to frontend and demux has been sent
   */
  starting,
  /**
   * Frontend is locked, demux is set up, first stream packet has been received
   */
  started,
  /**
   * Session ended due to request or error
   */
  ended
};

typedef std::function<void (session_event_t session_event)> session_callback_t;

}

#endif	/* SESSION_CALLBACK_T_H */
