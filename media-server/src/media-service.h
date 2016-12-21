/* 
 * File:   media-service.h
 * Author: wopi
 *
 * Created on June 10, 2011, 12:02 PM
 */

#ifndef MEDIA_SERVICE_H
#define	MEDIA_SERVICE_H

#include "service.h"

namespace home_system
{
namespace media
{

class media_service
: public home_system::service
{
public:
  media_service();
  void on_msg(yami::incoming_message & im);
private:
};

}
}

#endif	/* MEDIA_SERVICE_H */

