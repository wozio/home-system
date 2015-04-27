#ifndef TRANSPONDERDVBT_H
#define	TRANSPONDERDVBT_H

#include "transponder.h"
#include <linux/dvb/frontend.h>
#include <vector>

namespace home_system
{
namespace media
{

class transponder_dvbt
: public transponder
{
public:
  transponder_dvbt(const std::vector<std::string>& fields);
  ~transponder_dvbt();
  
  void tune(int fd);
  void print(std::ostream& str) const;
  bool isless(const transponder* right);
  void save(std::ostream& str) const;
private:
  
  void parse(const std::vector<std::string>& fields);
  
  int frequency_;
  int bandwidth_;
  fe_code_rate fec_hi_;
  fe_code_rate fec_lo_;
  fe_modulation mod_;
  fe_transmit_mode tm_;
  fe_guard_interval guard_;
  fe_hierarchy hi_;
};

}
}

#endif	/* TRANSPONDERDVBT_H */

