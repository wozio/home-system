#ifndef JSON_CONVERTER_H
#define	JSON_CONVERTER_H

#include "rapidjson/reader.h"
#include <yami4-cpp/parameters.h>
#include <string>
#include <ostream>
#include <istream>

namespace home_system
{

struct Handler
{
  Handler(yami::parameters& params);
  enum class state
  {
    none,
    parameter,
    array
  } state_ = state::none;

  yami::parameters& params_;
  std::string name_;

  bool StartObject();
  bool EndObject(rapidjson::SizeType memberCount);
  bool Key(const char* str, rapidjson::SizeType length, bool copy);
  bool StartArray();
  bool EndArray(rapidjson::SizeType elementCount);
  bool Null();
  bool Bool(bool b);
  bool Int(int i);
  bool Uint(unsigned u);
  bool Int64(int64_t i);
  bool Uint64(uint64_t u);
  bool Double(double d);
  bool String(const char* str, rapidjson::SizeType length, bool copy);
};

struct stream
{
  typedef char Ch;
  std::istream& is_;

  stream(std::istream& is);
  char Peek() const;
  char Take();
  size_t Tell();
  char* PutBegin();
  void Put(char c);
  void Flush();
  size_t PutEnd(char* begin);
};

void process_parameters(yami::parameters* params, std::ostream& out);

}

#endif	/* JSON_CONVERTER_H */

