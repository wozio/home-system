#ifndef TRANSPONDER_CONFIGURATION_EXCEPTION_H
#define	TRANSPONDER_CONFIGURATION_EXCEPTION_H

namespace home_system
{
namespace media
{

class transponder_configuration_exception : public std::exception
{
public:
  transponder_configuration_exception(const std::string& reason)
  : reason_(reason)
  {
  }
  
  const char* what() const throw()
  {
    return reason_.c_str();
  }
  
private:
  std::string reason_;

};

}
}

#endif	/* TRANSPONDER_CONFIGURATION_EXCEPTION_H */

