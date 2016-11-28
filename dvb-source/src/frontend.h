#ifndef FRONTEND_H
#define	FRONTEND_H

#include "timer.h"
#include "transponder.h"

namespace home_system
{
namespace media
{

enum class frontend_state
{
  closed,
  opened,
  tunning,
  tuned
};

std::ostream& operator << (std::ostream& os, const frontend_state& fs);

class frontend
{
public:
  frontend(int adapter, int frontend);
  ~frontend();
  
  frontend_state get_state();
  
  void set_transponder(std::shared_ptr<transponder> transponder);
  std::shared_ptr<transponder> get_transponder();
  
  typedef std::function<void (frontend_state)> state_callback_t;
  int register_state_callback(state_callback_t callback);
  void unregister_state_callback(int callback_id);
  
private:
  frontend(const frontend&){};
  
  int adapter_, frontend_;
  int fe_;

  frontend_state state_;
  void change_state(frontend_state new_state);
  
  std::map<int, state_callback_t> state_callbacks_;
  
  std::shared_ptr<transponder> transponder_;
  
  timer timer_;
  void set_timer(int duration);
  
  timer check_signal_timer_;
  
  void check();
  void open_adapter();
  void check_tunning();
  void check_signal();
};

}
}

#endif	/* FRONTEND_H */

