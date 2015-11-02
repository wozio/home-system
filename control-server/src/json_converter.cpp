#include "json_converter.h"
#include <boost/lexical_cast.hpp>
#include <iostream>

namespace home_system
{

using namespace rapidjson;
using namespace std;

Handler::Handler(std::string& service, std::string& message, yami::parameters& params)
: params_(params)
{
}

bool Handler::StartObject()
{
  cout << "StartObject()" << endl;
  return true;
}

bool Handler::EndObject(SizeType memberCount)
{
  cout << "EndObject(" << memberCount << ")" << endl;
  return true;
}

bool Handler::Key(const char* str, SizeType length, bool copy)
{
  cout << "Key(" << str << ", " << length << ", " << boolalpha << copy << ")" << endl;
  name_.assign(str, length);
  state_ = state::parameter;
  return true;
}

bool Handler::StartArray()
{
  cout << "StartArray()" << endl;
  return true;
}

bool Handler::EndArray(SizeType elementCount)
{
  cout << "EndArray(" << elementCount << ")" << endl;
  return true;
}

bool Handler::Null()
{
  return true;
}

bool Handler::Bool(bool b)
{
  cout << "Bool(" << boolalpha << b << ")" << endl;
  if (state_ == state::parameter)
  {
    params_.set_boolean(name_, b);
    state_ = state::none;
    return true;
  }
  return false;
}

bool Handler::Int(int i)
{
  cout << "Int(" << i << ")" << endl;
  return Uint64(i);
}

bool Handler::Uint(unsigned u)
{
  cout << "Uint(" << u << ")" << endl;
  return Uint64(u);
}

bool Handler::Int64(int64_t i)
{
  cout << "Int64(" << i << ")" << endl;
  return Uint64(i);
}

bool Handler::Uint64(uint64_t u)
{
  cout << "Uint64(" << u << ")" << endl;
  if (state_ == state::parameter)
  {
    params_.set_long_long(name_, u);
    state_ = state::none;
    return true;
  }
  return false;
}

bool Handler::Double(double d)
{
  cout << "Double(" << d << ")" << endl;
  if (state_ == state::parameter)
  {
    params_.set_double_float(name_, d);
    state_ = state::none;
    return true;
  }
  return false;
}

bool Handler::String(const char* str, SizeType length, bool copy)
{
  cout << "String(" << str << ", " << length << ", " << boolalpha << copy << ")" << endl;
  if (state_ == state::parameter)
  {
    params_.set_string(name_, string(str, length));
    state_ = state::none;
    return true;
  }
  return false;
}

stream::stream(std::istream& is)
: is_(is)
{
}
//! Read the current character from stream without moving the read cursor.

char stream::Peek() const
{
  if (is_.eof())
    return '\0';
  return is_.peek();
}

//! Read the current character from stream and moving the read cursor to next character.

char stream::Take()
{
  if (is_.eof())
    return '\0';
  char ch;
  is_.get(ch);
  return ch;
}

//! Get the current read cursor.
//! \return Number of characters read from start.

size_t stream::Tell()
{
  return is_.tellg();
}
// no need to implement those

char* stream::PutBegin()
{
  return nullptr;
}

void stream::Put(char c)
{
}

void stream::Flush()
{
}

size_t stream::PutEnd(char* begin)
{
  return 0;
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

}
