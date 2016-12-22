#include "transponders.h"
#include "transponder_dvbt.h"
#include "transponder_dvbs.h"
#include "transponder_dvbs2.h"
#include "transponder_configuration_exception.h"
#include <logger.h>
#include <boost/algorithm/string.hpp>
#include <fstream>
#include <iostream>
#include <stdexcept>

using namespace std;

namespace home_system
{
namespace media
{

transponders::transponders(const std::string& transponder_file)
: current_(transponders_.begin())
{
  // parsing transponders file for creating transponder for each entry
  LOG(DEBUG) << "Transponder definition file: " << transponder_file;
  
  ifstream f(transponder_file);
  string line;
  while (f.good())
  {
    getline(f, line);
    boost::trim(line);
    if (line.size() == 0)
      continue;
    if (line[0] == '#')
      continue;
    try
    {
      find_or_create(line);
    }
    catch (const exception& e)
    {
      LOG(WARNING) << "Creating transponder failed: " << e.what();
    }
    catch(...)
    {
      LOG(ERROR) << "Unknown exception";
    }
  }
  if (!transponders_.empty())
    current_ = transponders_.begin();
  
  LOG(INFO) << "Number of defined transponders: " << transponders_.size();
}

transponders::~transponders()
{
}

void transponders::print()
{
  for (auto c : transponders_)
  {
    c->print(cout);
    cout << endl;
  }
}

size_t transponders::size()
{
  return transponders_.size();
}

transponder_t transponders::next()
{
  if (!transponders_.empty())
  {
    current_++;
    
    if (current_ == transponders_.end())
      current_ = transponders_.begin();
    
    return current();
  }
  throw runtime_error("Next transponder requested while transponder list is empty");
}

transponder_t transponders::current()
{
  if (!transponders_.empty())
  {
    return *current_;
  }
  throw runtime_error("Current transponder requested while transponder list is empty");
}

shared_ptr<transponder> transponders::find_or_create(std::string& definition)
{
  // removing comments at the end
  definition = definition.substr(0, definition.find_first_of("#"));
  boost::trim(definition);
  if (definition.size() == 0)
    throw runtime_error("Incorrect definition string");
 
  boost::to_upper(definition);
  
  LOG(DEBUG) << "Find or create transponder from definition: " << definition;

  vector<string> fields;
  boost::split(fields, definition, boost::is_any_of("\t\xA "), boost::token_compress_on);
  
  if (fields.size() < 1)
  {
    throw runtime_error("It should be at least 1 parameter for definition");
  }
  
  shared_ptr<transponder> nt;

  if (fields[0] == "T")
  {
    nt = make_shared<transponder_dvbt>(fields);
  }
  else if (fields[0] == "S")
  {
    nt = make_shared<transponder_dvbs>(fields);
  }
  else if (fields[0] == "S2")
  {
    nt = make_shared<transponder_dvbs2>(fields);
  }
  auto tit = transponders_.find(nt);
  if (tit == transponders_.end())
  {
    LOG(DEBUG) << "Created transponder: " << *nt;
    transponders_.insert(nt);
    return nt;
  }
  else
  {
    LOG(DEBUG) << "Found transponder: " << **tit;
    return *tit;
  }
}

transponder_t transponders::set(transponder_t t)
{
  auto tit = transponders_.find(t);
  if (tit == transponders_.end())
  {
    throw runtime_error("Trying to set to not known transponder");
  }
  
  current_ = tit;
  return t;
}

bool operator<(const transponder_t& left, const transponder_t& right)
{
  return left->isless(right);
}

}
}
