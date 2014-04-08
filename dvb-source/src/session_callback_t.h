#ifndef SESSION_CALLBACK_T_H
#define	SESSION_CALLBACK_T_H

#include <functional>

namespace dvb
{

typedef std::function<void (size_t size, char* buffer)> session_callback_t;

}

#endif	/* SESSION_CALLBACK_T_H */

