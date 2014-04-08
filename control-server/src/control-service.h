#ifndef CONTROL_SERVICE_H
#define	CONTROL_SERVICE_H

#include "service.h"

namespace home_system
{
namespace control_server
{

class control_service
: public home_system::service
{
public:
  control_service();
  void on_msg(yami::incoming_message & im);
private:
};

}
}

#endif	/* CONTROL_SERVICE_H */

