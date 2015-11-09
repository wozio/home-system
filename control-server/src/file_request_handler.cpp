#include "file_request_handler.h"

#include "logger.h"
#include <Poco/Net/HTTPServerRequest.h>
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/URI.h>
#include <boost/algorithm/string.hpp>
#include <map>
#include <fstream>
#include <vector>

using namespace Poco::Net;
using namespace Poco;
using namespace std;
using namespace boost;

namespace home_system
{

std::map<std::string, std::string> media_types_;

void fill_media_types()
{
  ifstream f("mime.types");
  string line;
  while (f.good())
  {
    getline(f, line);
    trim(line);
    if (line.size() > 0 && line[0] != '#') // ignore comments
    {
      vector<string> split_str;
      split(split_str, line, is_any_of(" \t"), token_compress_on);
      if (split_str.size() > 1)
      {
        for (size_t i = 1; i < split_str.size(); ++i)
        {
          media_types_[split_str[i]] = split_str[0];
        }
      }
    }
  }
}

file_request_handler::file_request_handler(const std::string& root)
: root_(root)
{
}

void file_request_handler::handleRequest(HTTPServerRequest& request, HTTPServerResponse& response)
{
  try
  {
    std::string path = URI(request.getURI()).getPath();

    response.setChunkedTransferEncoding(true);
    response.add("Expires", "-1");
    response.add("Cache-control", "no-cache");

    if (path.back() == '/')
    {
      path.append("index.html");
    }

    path.insert(0, root_);

    //LOG("Path: " << path);

    // selecting mime type according to extension
    std::string ext = path.substr(path.find_last_of(".") + 1);
    std::string type;
    try
    {
      type = media_types_.at(ext);
    }
    catch (std::out_of_range& e)
    {
      type = "text/plain";
    }

    // serve file from path
    response.sendFile(path, type);
  }
  catch (const FileNotFoundException& e)
  {
    LOGWARN("EXCEPTION: " << e.displayText());
    response.setStatus(HTTPServerResponse::HTTP_NOT_FOUND);
    response.send();
  }
}

}
