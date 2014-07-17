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

enum class session_event_t
{
  ended
};

typedef std::function<void (session_event_t session_event)> session_callback_t;
typedef std::function<void (size_t size, char* buffer)> session_stream_part_callback_t;

}

#endif	/* SESSION_CALLBACK_T_H */

