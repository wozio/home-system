#ifndef IO_SERVICE_H
#define	IO_SERVICE_H

#include "service.h"
#include "ownetwork.h"

namespace home_system
{
namespace input_output
{

class io_service
: public home_system::service
{
public:
  io_service();
  ~io_service();
  void on_msg(yami::incoming_message & im);
private:
  ow::net net_;
};

}
}

#endif	/* IO_SERVICE_H */

