#include "json_converter.h"
#include "rapidjson/document.h"
#include <boost/lexical_cast.hpp>
#include <boost/interprocess/streams/bufferstream.hpp>
//#include <iostream>
#include <sstream>

namespace home_system
{

using namespace rapidjson;
using namespace std;

void process_json(data_t data, std::string& service, std::string& message,
  bool& expect_reply, yami::parameters& params)
{
  Document d;
  d.Parse(data->data());
  
  if (d.IsObject())
  {
    Value::ConstMemberIterator itr = d.FindMember("message");
    if (itr != d.MemberEnd())
    {
      message = itr->value.GetString();
    }
    else
      throw incorrect_message();
    itr = d.FindMember("service");
    if (itr != d.MemberEnd())
    {
      service = itr->value.GetString();
    }
    else
      throw incorrect_message();
    itr = d.FindMember("expect_reply");
    if (itr != d.MemberEnd())
    {
      expect_reply = itr->value.GetBool();
    }
    else
      expect_reply = false;
  }
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

void process_parameters(yami::parameters* params, std::string& outstr)
{
  ostringstream out;
  process_parameters(params, out);
  outstr = out.str();
}

void process_parameters(yami::parameters* params, data_t data, size_t& data_size)
{
  boost::interprocess::bufferstream out(data->data(), DATA_SIZE);
  process_parameters(params, out);
  data_size = out.tellp();
}

}
