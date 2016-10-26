#pragma once

#define DATA_CALLBACK_FUNC void(int id, const std::vector<char>& data)
#define EVENT_CALLBACK_FUNC void(int it, event_t event)

namespace home_system
{
  class client_binary_session
  {
  public:
    enum class event_t {
      created,
      destroyed,
      stopped,
      started
    };
    client_binary_session(const std::string& target, const std::string& target_endpoint, yami::parameters& creation_params);

    boost::signals2::signal<DATA_CALLBACK_FUNC> data_;
    boost::signals2::signal<EVENT_CALLBACK_FUNC> event_;

    int get_id();

    void operator()(yami::incoming_message & im);

  private:
    std::string target_;
    std::string target_endpoint_;
    yami::parameters creation_params_;
    int id_;

    yami::agent agent_;
  };
}
