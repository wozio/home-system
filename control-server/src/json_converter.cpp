#include "json_converter.h"
#include "logger.h"
#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"

namespace home_system
{

using namespace rapidjson;
using namespace std;

// json to YAMI conversion

void parse_parameters(const Value& value, yami::parameters& params)
{
  for (auto itr = value.MemberBegin(); itr != value.MemberEnd(); ++itr)
  {
    switch (itr->value.GetType())
    {
    case kNullType: // ignore
    case kObjectType:
    case kArrayType:
      break;
    case kFalseType:
    case kTrueType:
      params.set_boolean(itr->name.GetString(), itr->value.GetBool());
      break;
    case kStringType:
      params.set_string(itr->name.GetString(), itr->value.GetString());
      break;
    case kNumberType:
      params.set_long_long(itr->name.GetString(), itr->value.GetInt64());
      break;
    }
  }
}

msg_type_t from_json(data_t data, std::string& source, std::string& target, std::string& message,
    long long& sequence_number, yami::parameters& params)
{
  Document d;
  d.Parse(data->data());

  msg_type_t type;

  if (d.IsObject())
  {
    Value::ConstMemberIterator itr = d.FindMember("message");
    if (itr != d.MemberEnd())
    {
      message = itr->value.GetString();
      type = msg_type_t::one_way;
    }
    else
    {
      type = msg_type_t::reply;
    }

    itr = d.FindMember("target");
    if (itr != d.MemberEnd())
    {
      target = itr->value.GetString();
    }

    itr = d.FindMember("source");
    if (itr != d.MemberEnd())
    {
      source = itr->value.GetString();
    }

    itr = d.FindMember("sequence_number");
    if (itr != d.MemberEnd())
    {
      sequence_number = itr->value.GetInt64();
      if (type == msg_type_t::one_way)
      {
        type = msg_type_t::for_reply;
      }
    }
    else
    {
      if (type == msg_type_t::reply)
      {
        LOG(ERROR) << "Message without 'message' element must have 'sequence_number' element";
        throw runtime_error("Incorrect message");
      }
    }

    itr = d.FindMember("parameters");
    if (itr != d.MemberEnd())
    {
      if (itr->value.IsObject())
      {
        const auto& v = itr->value;
        parse_parameters(v, params);
      }
    }
    return type;
  }
  LOG(ERROR) << "Root JSON element not an object";
  throw runtime_error("Incorrect message");
}

// YAMI to JSON string

void process_parameters(const yami::parameters& params, Writer<StringBuffer>& writer)
{
  writer.StartObject();
  for (yami::parameters::iterator it = params.begin(); it != params.end();
      ++it)
  {
    writer.Key(StringRef((*it).name().c_str()));

    switch ((*it).type())
    {
    case yami::boolean:
      writer.Bool((*it).get_boolean());
      break;
    case yami::integer:
      writer.Int((*it).get_integer());
      break;
    case yami::long_long:
      writer.Int64((*it).get_long_long());
      break;
    case yami::double_float:
      writer.Double((*it).get_double_float());
      break;
    case yami::string:
      writer.String(StringRef((*it).get_string().c_str()));
      break;
    case yami::integer_array:
    {
      writer.StartArray();
      size_t ybs;
      int* yb = (*it).get_integer_array(ybs);
      for (size_t i = 0; i < ybs; ++i)
      {
        writer.Int(yb[i]);
      }
      writer.EndArray();
      break;
    }
    case yami::boolean_array:
    {
      writer.StartArray();
      size_t ybs;
      bool* yb = (*it).get_boolean_array(ybs);
      for (size_t i = 0; i < ybs; ++i)
      {
        writer.Bool(yb[i]);
      }
      writer.EndArray();
      break;
    }
    case yami::long_long_array:
    {
      writer.StartArray();
      size_t ybs;
      long long* yb = (*it).get_long_long_array(ybs);
      for (size_t i = 0; i < ybs; ++i)
      {
        writer.Int64(yb[i]);
      }
      writer.EndArray();
      break;
    }
    case yami::double_float_array:
    {
      writer.StartArray();
      size_t ybs;
      double* yb = (*it).get_double_float_array(ybs);
      for (size_t i = 0; i < ybs; ++i)
      {
        writer.Double(yb[i]);
      }
      writer.EndArray();
      break;
    }
    case yami::string_array:
    {
      writer.StartArray();
      size_t ybs = (*it).get_string_array_length();
      for (size_t i = 0; i < ybs; ++i)
      {
        writer.String(StringRef((*it).get_string_in_array(i).c_str()));
      }
      writer.EndArray();
      break;
    }
    case yami::nested_parameters:
    {
      yami::parameters nested((*it).get_nested_parameters());
      process_parameters(nested, writer);
      break;
    }
    case yami::nested_parameters_array:
    {
      writer.StartArray();
      size_t ybs = params.get_nested_array_length((*it).name());
      for (size_t i = 0; i < ybs; ++i)
      {
        yami::parameters nested(params.get_nested_in_array((*it).name(), i));
        process_parameters(nested, writer);
      }
      writer.EndArray();
      break;
    }
    case yami::unused:
      break;
    default:
      break;
    }
  }
  writer.EndObject();
}

void reply_to_json(const std::string& target, const std::string& result, const std::string& reason,
        long long sequence_number, const yami::parameters& params,
        buffer_t buffer)
{
  Writer<StringBuffer> writer(*buffer);
  
  writer.StartObject();
  
  writer.Key("target");
  writer.String(StringRef(target.c_str()));
  
  writer.Key("sequence_number");
  writer.Uint(sequence_number);
  
  writer.Key("result");
  writer.String(StringRef(result.c_str()));
  
  writer.Key("reason");
  writer.String(StringRef(reason.c_str()));
  
  writer.Key("params");
  
  process_parameters(params, writer);
  
  writer.EndObject();
}

void reply_to_json(const std::string& target, const std::string& result, const std::string& reason,
        long long sequence_number,
        buffer_t buffer)
{
  Writer<StringBuffer> writer(*buffer);
  
  writer.StartObject();
  
  writer.Key("target");
  writer.String(StringRef(target.c_str()));
  
  writer.Key("sequence_number");
  writer.Uint(sequence_number);
  
  writer.Key("result");
  writer.String(StringRef(result.c_str()));
  
  writer.Key("reason");
  writer.String(StringRef(reason.c_str()));
  
  writer.EndObject();
}

void msg_to_json(const std::string& target, const std::string& message,
        long long sequence_number, const yami::parameters& params,
        buffer_t buffer)
{
  Writer<StringBuffer> writer(*buffer);
  
  writer.StartObject();
  
  writer.Key("target");
  writer.String(StringRef(target.c_str()));
  
  writer.Key("message");
  writer.String(StringRef(message.c_str()));
  
  writer.Key("sequence_number");
  writer.Uint(sequence_number);
  
  writer.Key("params");
  
  process_parameters(params, writer);
  
  writer.EndObject();  
}

void msg_to_json(const std::string& target, const std::string& message,
        const yami::parameters& params,
        buffer_t buffer)
{
  Writer<StringBuffer> writer(*buffer);
  
  writer.StartObject();
  
  writer.Key("target");
  writer.String(StringRef(target.c_str()));
  
  writer.Key("message");
  writer.String(StringRef(message.c_str()));
  
  writer.Key("params");
  
  process_parameters(params, writer);
  
  writer.EndObject();  
}

}
