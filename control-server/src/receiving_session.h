#pragma once

namespace home_system
{
  class receiving_session
  {
  public:
    typedef std::function<void(int id, const std::vector<char>& data)> data_callback_t;
    enum class event_t {
      created,
      destroyed,
      stopped,
      started
    };
    typedef std::function<void(int it, event_t event)> event_callback_t;

    receiving_session(const std::string& target, const std::string& target_endpoint, yami::parameters& creation_params,
      data_callback_t data_callback, event_callback_t event_callback);

    int get_id();

    void operator()(yami::incoming_message & im);

  private:
    std::string target_;
    std::string target_endpoint_;
    yami::parameters creation_params_;
    data_callback_t data_callback_;
    event_callback_t event_callback_;
    int id_;

    yami::agent agent_;
  };
}
