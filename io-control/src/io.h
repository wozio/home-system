#pragma once

namespace home_system
{
class io;

typedef std::shared_ptr<io> io_t;

class io
{
public:
    static io_t create(const std::string& type, const std::string& name, const std::string& service, long long id);

    io(const std::string& type, const std::string& name, const std::string& service, long long id);
    virtual ~io();

    // in params 'value' field in proper type must be present
    virtual void on_value_state_change(const yami::parameters& params) = 0;

    // 'value' field in proper type will be written into params
    virtual void write_value_state(yami::parameters& params) = 0;

    enum class state
    {
        unknown, // there is no communication with remote service
        ok,
        failed // remote service reports io as failed
    };
private:

    const std::string type_;
    const std::string name_;
    const std::string service_;
    const long long id_;
    state state_;
};

}