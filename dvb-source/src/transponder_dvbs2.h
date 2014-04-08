#ifndef TRANSPONDERDVBS2_H
#define	TRANSPONDERDVBS2_H

#include "transponder_dvbs.h"
#include <vector>

namespace home_system
{
namespace media
{

class transponder_dvbs2
: public transponder_dvbs
{
public:
  transponder_dvbs2(const std::vector<std::string>& fields);
  ~transponder_dvbs2();
  
  void tune(int fd);
  void print(std::ostream& str) const;
private:
  fe_rolloff rolloff_;
  fe_modulation mod_;
};

}
}

#endif	/* TRANSPONDERDVBS2_H */

