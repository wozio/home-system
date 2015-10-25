#include "http.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include "rapidjson/reader.h"
#include <iostream>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/URI.h>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <stack>

using Poco::Net::HTTPRequestHandler;
using Poco::Net::HTTPServerRequest;
using Poco::Net::HTTPServerResponse;

using namespace boost::filesystem;
using namespace std;
using namespace Poco;

namespace home_system
{
namespace control_server
{

using namespace rapidjson;
using namespace std;

struct Handler
{
  Handler()
  {
    params_.push(root_);
  }
  
  enum class state
  {
    none,
    parameter,
    array
  } state_ = state::none;

  yami::parameters root_;
  std::stack<yami::parameters> params_;
  std::string name_;

  bool StartObject()
  {
    cout << "StartObject()" << endl;
    switch (state_)
    {
      case state::none:
        // root object, do nothing
        break;
      case state::parameter:
      {
        // nested parameters
        yami::parameters params;
        params_.push(params);
        state_ = state::none;
        break;
      }
      case state::array:
        // array of nested objects
        break;
    }
    return true;
  }

  bool EndObject(SizeType memberCount)
  {
    cout << "EndObject(" << memberCount << ")" << endl;
    switch (state_)
    {
      case state::none:
        params_.top().set_nested_parameters(name_, params);
        params_.pop();
        break;
      case state::parameter:
      {
        // error
        return false;
      }
      case state::array:
        // array of nested objects
        break;
    }
    return true;
  }

  bool Key(const char* str, SizeType length, bool copy)
  {
    cout << "Key(" << str << ", " << length << ", " << boolalpha << copy << ")" << endl;
    name_.assign(str, length);
    state_ = state::parameter;
    return true;
  }

  bool StartArray()
  {
    cout << "StartArray()" << endl;
    return true;
  }

  bool EndArray(SizeType elementCount)
  {
    cout << "EndArray(" << elementCount << ")" << endl;
    return true;
  }

  bool Null()
  {
    state_ = state::none;
    return true;
  }

  bool Bool(bool b)
  {
    cout << "Bool(" << boolalpha << b << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_boolean(name_, b);
      state_ = state::none;
      return true;
    }
    return false;
  }

  bool Int(int i)
  {
    cout << "Int(" << i << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_long_long(name_, i);
      state_ = state::none;
      return true;
    }
    return false;
  }

  bool Uint(unsigned u)
  {
    cout << "Uint(" << u << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_long_long(name_, u);
      state_ = state::none;
      return true;
    }
    return false;
  }

  bool Int64(int64_t i)
  {
    cout << "Int64(" << i << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_long_long(name_, i);
      state_ = state::none;
      return true;
    }
    return false;
  }

  bool Uint64(uint64_t u)
  {
    cout << "Uint64(" << u << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_long_long(name_, u);
      state_ = state::none;
      return true;
    }
    return false;
  }

  bool Double(double d)
  {
    cout << "Double(" << d << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_double_float(name_, d);
      state_ = state::none;
      return true;
    }
    return false;
  }

  bool String(const char* str, SizeType length, bool copy)
  {
    cout << "String(" << str << ", " << length << ", " << boolalpha << copy << ")" << endl;
    if (state_ == state::parameter)
    {
      params_.top().set_string(name_, str, length);
      state_ = state::none;
      return true;
    }
    return false;
  }
};

struct stream
{
  typedef char Ch;
  std::istream& is_;

  stream(std::istream& is)
  : is_(is)
  {
  }
  //! Read the current character from stream without moving the read cursor.

  Ch Peek() const
  {
    if (is_.eof())
      return '\0';
    return is_.peek();
  }

  //! Read the current character from stream and moving the read cursor to next character.

  Ch Take()
  {
    if (is_.eof())
      return '\0';
    Ch ch;
    is_.get(ch);
    return ch;
  }

  //! Get the current read cursor.
  //! \return Number of characters read from start.

  size_t Tell()
  {
    return is_.tellg();
  }
  // no need to implement those

  Ch* PutBegin()
  {
    return nullptr;
  }

  void Put(Ch c)
  {
  }

  void Flush()
  {
  }

  size_t PutEnd(Ch* begin)
  {
    return 0;
  }
};

class request_handler : public HTTPRequestHandler
{
public:

  request_handler()
  {
  }

  void process_parameters(yami::parameters* params, std::ostream& out)
  {
    out << '{';
    bool first = true;
    for (yami::parameters::iterator it = params->begin(); it != params->end(); ++it)
    {
      if (!first)
      {
        out << ',';
      }
      first = false;

      out << '"' << (*it).name() << '"' << ':';

      switch ((*it).type())
      {
        case yami::boolean:
          out << ((*it).get_boolean() ? "true" : "false");
          break;
        case yami::integer:
          out << boost::lexical_cast<string>((*it).get_integer());
          break;
        case yami::long_long:
          out << boost::lexical_cast<string>((*it).get_long_long());
          break;
        case yami::double_float:
          out << boost::lexical_cast<string>((*it).get_double_float());
          break;
        case yami::string:
          out << '"' << (*it).get_string() << '"';
          break;
        case yami::integer_array:
        {
          out << "[";
          size_t ybs;
          int* yb = (*it).get_integer_array(ybs);
          if (ybs > 0)
          {
            for (size_t i = 0; i < ybs - 1; ++i)
            {
              out << boost::lexical_cast<string>(yb[i]) << ',';
            }
            out << boost::lexical_cast<string>(yb[ybs - 1]);
          }
          out << ']';
          break;
        }
        case yami::boolean_array:
        {
          out << "[";
          size_t ybs;
          bool* yb = (*it).get_boolean_array(ybs);
          if (ybs > 0)
          {
            for (size_t i = 0; i < ybs - 1; ++i)
            {
              out << (yb[i] ? "true" : "false") << ',';
            }
            out << (yb[ybs - 1] ? "true" : "false");
          }
          out << ']';
          break;
        }
        case yami::long_long_array:
        {
          out << "[";
          size_t ybs;
          long long* yb = (*it).get_long_long_array(ybs);
          if (ybs > 0)
          {
            for (size_t i = 0; i < ybs - 1; ++i)
            {
              out << boost::lexical_cast<string>(yb[i]) << ',';
            }
            out << boost::lexical_cast<string>(yb[ybs - 1]);
          }
          out << ']';
          break;
        }
        case yami::double_float_array:
        {
          out << "[";
          size_t ybs;
          double* yb = (*it).get_double_float_array(ybs);
          if (ybs > 0)
          {
            for (size_t i = 0; i < ybs - 1; ++i)
            {
              out << boost::lexical_cast<string>(yb[i]) << ',';
            }
            out << boost::lexical_cast<string>(yb[ybs - 1]);
          }
          out << ']';
          break;
        }
        case yami::string_array:
        {
          out << "[";
          size_t ybs = (*it).get_string_array_length();
          if (ybs > 0)
          {
            for (size_t i = 0; i < ybs - 1; ++i)
            {
              out << '"' << (*it).get_string_in_array(i) << '"' << ',';
            }
            out << '"' << (*it).get_string_in_array(ybs - 1) << '"';
          }
          out << ']';
          break;
        }
        case yami::nested_parameters:
        {
          yami::parameters nested((*it).get_nested_parameters());
          process_parameters(&nested, out);
          break;
        }
        case yami::nested_parameters_array:
        {
          out << '[';
          size_t ybs = params->get_nested_array_length((*it).name());
          if (ybs > 0)
          {
            for (size_t i = 0; i < ybs - 1; ++i)
            {
              yami::parameters nested(params->get_nested_in_array((*it).name(), i));
              process_parameters(&nested, out);
              out << ',';
            }
            yami::parameters nested(params->get_nested_in_array((*it).name(), ybs - 1));
            process_parameters(&nested, out);
          }
          out << ']';
          break;
        }
        case yami::unused:
          break;
        default:
          break;
      }
    }
    out << '}';
  }

  void handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
  {
    try
    {
      // parsing query
      // we should have execute?name=<service_name>&msg=<msg>
      // and in request body we may have json with parameters of the message
      URI uri(request.getURI());
      string query = uri.getQuery();
      if (query.size() == 0)
        throw bad_request();

      string name;
      string msg;

      size_t pos1 = 0;
      size_t pos2;
      do
      {
        pos2 = query.find_first_of("&", pos1);
        size_t epos = query.find_first_of("=", pos1);
        if (epos == string::npos || epos > pos2 || epos == pos1 || epos == pos2)
          throw bad_request();
        string parname = query.substr(pos1, epos - pos1);
        string value = query.substr(epos + 1, pos2 - epos - 1);
        if (parname == "name")
        {
          name = value;
        }
        else if (parname == "msg")
        {
          msg = value;
        }
        pos1 = pos2 + 1;
      }
      while (pos2 != string::npos);

      if (name.size() == 0 || msg.size() == 0)
      {
        LOGWARN("Bad query: " << query);
        throw bad_request();
      }

      // getting service from service discovery
      string ye = DISCOVERY.get(name);

      LOG("Service: " << name << " (" << ye << ")");

      yami::parameters params;

      try
      {
        Handler handler(params);
        Reader reader;
        stream ss(request.stream());
        reader.Parse(ss, handler);
      }
      catch (const std::exception& e)
      {
        LOG("EXCEPTION during XML parsing: " << e.what());
      }

      // sending message to service
      auto_ptr<yami::outgoing_message> message(
        AGENT.send(ye, name, msg, params));

      message->wait_for_completion(1000);

      switch (message->get_state())
      {
        case yami::replied:
        {
          LOG("Replied");
          response.setChunkedTransferEncoding(true);
          response.setContentType("application/json");
          response.add("Expires", "-1");
          response.add("Cache-control", "no-cache");

          // converting yami output to xml
          // yami binary values are not supported

          ostream& resp_str = response.send();
          yami::parameters* reply = message->extract_reply();
          process_parameters(reply, resp_str);
          delete reply;

          break;
        }

        case yami::posted:
        case yami::transmitted:
          LOG("Posted/transmitted");
          response.send();
          break;

        case yami::abandoned:
          LOG("Abandoned");
          throw service_unavailable("Message was abandoned");
          break;

        case yami::rejected:
          LOG("Rejected: " + message->get_exception_msg());
          throw service_unavailable("Message was rejected: " + message->get_exception_msg());
          break;
      }
    }
    catch (const service_not_found& e)
    {
      LOGWARN("EXCEPTION: service_not_found: " << e.what());
      response.setStatus(HTTPServerResponse::HTTP_SERVICE_UNAVAILABLE);
      response.send() << "service_not_found: " << e.what();
    }
    catch (const service_unavailable& e)
    {
      LOGWARN("EXCEPTION: service_unavailable: " << e.what());
      response.setStatus(HTTPServerResponse::HTTP_SERVICE_UNAVAILABLE);
      response.send() << "service_unavailable: " << e.what();
    }
    catch (const yami::yami_runtime_error& e)
    {
      LOGWARN("EXCEPTION: yami_runtime_error: " << e.what());
      response.setStatus(HTTPServerResponse::HTTP_SERVICE_UNAVAILABLE);
      response.send() << "yami_runtime_error: " << e.what();
    }
    catch (const runtime_error& e)
    {
      LOGWARN("EXCEPTION: runtime_error: " << e.what());
      response.setStatus(HTTPServerResponse::HTTP_BAD_REQUEST);
      response.send() << "runtime_error: " << e.what();
    }
    catch (const bad_request& e)
    {
      LOGWARN("EXCEPTION: bad_request: " << e.what());
      response.setStatus(HTTPServerResponse::HTTP_BAD_REQUEST);
      response.send() << "bad_request: " << e.what();
    }
    catch (const Poco::Exception& e)
    {
      LOGWARN("EXCEPTION: " << e.displayText());
      response.setStatus(HTTPServerResponse::HTTP_INTERNAL_SERVER_ERROR);
      response.send() << e.displayText();
    }
    catch (const std::exception& e)
    {
      LOGWARN("EXCEPTION: " << e.what() << " request: " << request.getURI());
      response.setStatus(HTTPServerResponse::HTTP_INTERNAL_SERVER_ERROR);
      response.send() << e.what();
    }
  }

private:
  // exceptions

  class bad_request : public std::exception
  {
  };

  class service_unavailable : public std::exception
  {
  public:

    service_unavailable(const string& reason) throw ()
    : reason_(reason)
    {
    }

    ~service_unavailable() throw ()
    {
    }

    const char* what() const throw ()
    {
      return reason_.c_str();
    }
  private:
    string reason_;
  };
};

request_handler_factory::request_handler_factory()
{
}

HTTPRequestHandler* request_handler_factory::createRequestHandler(const HTTPServerRequest& request)
{
  string uri = request.getURI();
  LOG("Request: " << request.clientAddress().toString() << uri);
  return new request_handler();
}

}
}

