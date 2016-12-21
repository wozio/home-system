#ifndef TRANSPONDERDVBS_H
#define	TRANSPONDERDVBS_H
#include "transponder.h"
#include "frontend_extended.h"
#include <linux/dvb/frontend.h>
#include <vector>

namespace home_system
{
namespace media
{

class transponder_dvbs
: public transponder
{
public:
  transponder_dvbs(const std::vector<std::string>& fields);
  ~transponder_dvbs();
  
  void tune(int fd);
  void print(std::ostream& str) const;
  bool isless(const transponder_t right);
  void save(std::ostream& str) const;
protected:
  unsigned long frequency_;
  polarization polarization_;
  int symbol_rate_;
  fe_code_rate fec_;
  void print_dvbs_params(std::ostream& str) const;
  
  struct lnb_type_st
  {
    const char *name;
    unsigned long	low_val;
    unsigned long	high_val;	/* zero indicates no hiband */
    unsigned long	switch_val;	/* zero indicates no hiband */
  };
  static lnb_type_st lnbs_[];
  
  lnb_type_st lnb_type_;
  
  void clear_prepare(int fd, int& hiband, int& ifreq);
  void diseqc(int fd, int hiband);
};

}
}

#endif	/* TRANSPONDERDVBS_H */

