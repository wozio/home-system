#include "http.h"
#include "yamicontainer.h"
#include "logger.h"
#include "discovery.h"
#include <Poco/Net/HTTPServerResponse.h>
#include <Poco/URI.h>
#include <Poco/SAX/SAXParser.h>
#include <Poco/SAX/AttributesImpl.h>
#include "Poco/SAX/ContentHandler.h"
#include <Poco/SAX/InputSource.h>
#include <Poco/XML/XMLWriter.h>
#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>

using Poco::Net::HTTPRequestHandler;
using Poco::Net::HTTPServerRequest;
using Poco::Net::HTTPServerResponse;

using namespace boost::filesystem;
using namespace std;
using namespace Poco;
using namespace Poco::XML;

namespace home_system
{
namespace control_server
{

class content_handler: public ContentHandler
{
public:
  content_handler(yami::parameters& params)
  : params_(params),
    root_ok_(false),
    root_given_(false),
    param_found_(false),
    array_found_(false),
    element_found_(false)
  {
  }
  
  void setDocumentLocator(const Locator* loc){};
  void startDocument(){};
  void endDocument(){};
  
  void startElement(const XMLString& namespaceURI, const XMLString& localName,
    const XMLString& qname, const Attributes& attributes)
  {
    if (!root_given_)
    {
      root_given_ = true;
      if (localName == "hsd")
        root_ok_ = true;
    }
    else if (root_ok_)
    {
      if (localName == "p")
      {
        size_t as = attributes.getLength();
        for (size_t i = 0; i < as; ++i)
        {
          if (attributes.getLocalName(i) == "t")
          {
            if (attributes.getValue(i) == "i")
              type_ = yami::integer;
            else if (attributes.getValue(i) == "b")
              type_ = yami::boolean;
            else if (attributes.getValue(i) == "l")
              type_ = yami::long_long;
            else if (attributes.getValue(i) == "d")
              type_ = yami::double_float;
            else if (attributes.getValue(i) == "s")
              type_ = yami::string;
          }
          else if (attributes.getLocalName(i) == "n")
          {
            name_ = attributes.getValue(i);
          }
        }
        if (type_ != yami::unused && name_.size() > 0)
          param_found_ = true;
      }
      else if (localName == "a")
      {
        size_t as = attributes.getLength();
        for (size_t i = 0; i < as; ++i)
        {
          if (attributes.getLocalName(i) == "t")
          {
            if (attributes.getValue(i) == "i")
              type_ = yami::integer_array;
            else if (attributes.getValue(i) == "b")
              type_ = yami::boolean_array;
            else if (attributes.getValue(i) == "l")
              type_ = yami::long_long_array;
            else if (attributes.getValue(i) == "d")
              type_ = yami::double_float_array;
            else if (attributes.getValue(i) == "s")
              type_ = yami::string_array;
          }
          else if (attributes.getLocalName(i) == "n")
          {
            name_ = attributes.getValue(i);
          }
          else if (attributes.getLocalName(i) == "s")
          {
            array_data_.reserve(boost::lexical_cast<int>(attributes.getValue(i)));
          }
        }
        if (type_ != yami::unused && name_.size() > 0)
          array_found_ = true;
      }
      else if (localName == "e")
      {
        if (array_found_)
          element_found_ = true;
      }
      else if (localName == "n")
      {
        // TODO if needed
      }
    }
  }
  
  void characters(const XMLChar ch[], int start, int length)
  {
    if (param_found_ || element_found_)
    {
      data_.append(ch + start, length);
    }
  }
  
  void endElement(const XMLString& uri, const XMLString& localName, const XMLString& qname)
  {
    if (localName == "p")
    {
      if (param_found_)
      {
        switch (type_)
        {
        case yami::integer:
          params_.set_integer(name_, boost::lexical_cast<int>(data_));
          break;
        case yami::boolean:
          params_.set_boolean(name_, data_ == "true" ? true : false);
          break;
        case yami::long_long:
          params_.set_long_long(name_, boost::lexical_cast<long long>(data_));
          break;
        case yami::double_float:
          params_.set_double_float(name_, boost::lexical_cast<double>(data_));
          break;
        case yami::string:
          params_.set_string(name_, data_);
          break;
        default:
          break;
        }
      }
      data_.clear();
      name_.clear();
      type_ = yami::unused;
      param_found_ = false;
    }
    else if (localName == "a")
    {
      if (array_found_)
      {
        switch (type_)
        {
        case yami::integer_array:
        {
          vector<int> data;
          data.reserve(array_data_.size());
          for (size_t i = 0; i < array_data_.size(); ++i)
          {
            data.push_back(boost::lexical_cast<int>(array_data_[i]));
          }
          params_.set_integer_array(name_, &data[0], array_data_.size());
          break;
        }
        case yami::boolean_array:
        {
          bool* data = new bool[array_data_.size()];
          for (size_t i = 0; i < array_data_.size(); ++i)
          {
            data[i] = array_data_[i] == "true" ? true : false;
          }
          params_.set_boolean_array(name_, &data[0], array_data_.size());
          delete [] data;
          break;
        }
        case yami::long_long_array:
        {
          vector<long long> data;
          data.reserve(array_data_.size());
          for (size_t i = 0; i < array_data_.size(); ++i)
          {
            data.push_back(boost::lexical_cast<long long>(array_data_[i]));
          }
          params_.set_long_long_array(name_, &data[0], array_data_.size());
          break;
        }
        case yami::double_float_array:
        {
          vector<double> data;
          data.reserve(array_data_.size());
          for (size_t i = 0; i < array_data_.size(); ++i)
          {
            data.push_back(boost::lexical_cast<double>(array_data_[i]));
          }
          params_.set_double_float_array(name_, &data[0], array_data_.size());
          break;
        }
        case yami::string_array:
          params_.create_string_array(name_, array_data_.size());
          for (size_t i = 0; i < array_data_.size(); ++i)
          {
            params_.set_string_in_array(name_, i, array_data_[i]);
          }
          break;
        default:
          break;
        }
        array_data_.clear();
        array_found_ = false;
        name_.clear();
        type_ = yami::unused;
      }
    }
    else if (localName == "e")
    {
      if (array_found_ && element_found_)
      {
        array_data_.push_back(data_);
        data_.clear();
        element_found_ = false;
      }
    }
  }
  
  void ignorableWhitespace(const XMLChar ch[], int start, int len){};
  void processingInstruction(const XMLString& target, const XMLString& data){};
  void startPrefixMapping(const XMLString& prefix,const XMLString& uri){};
  void endPrefixMapping(const XMLString& prefix){};
  void skippedEntity(const XMLString& name){};
  
private:
  yami::parameters& params_;
  bool root_ok_, root_given_, param_found_, array_found_, element_found_;
  string name_;
  yami::parameter_type type_;
  string data_;
  vector<string> array_data_;
};

class request_handler: public HTTPRequestHandler
{
public:
  request_handler()
  {
  }
  
  void process_parameters(yami::parameters* params, XMLWriter& xw)
  {
    for (yami::parameters::iterator it = params->begin(); it != params->end(); ++it)
    {
      AttributesImpl attrs;
      attrs.addAttribute("", "", "n", "", (*it).name());
      switch ((*it).type())
      {
      case yami::boolean:
        attrs.addAttribute("", "", "t", "", "b");
        xw.startElement("", "", "p", attrs);
        xw.characters((*it).get_boolean() ? "true" : "false");
        xw.endElement("", "", "p");
        break;
      case yami::integer:
        attrs.addAttribute("", "", "t", "", "i");
        xw.startElement("", "", "p", attrs);
        xw.characters(boost::lexical_cast<string>((*it).get_integer()));
        xw.endElement("", "", "p");
        break;
      case yami::long_long:
        attrs.addAttribute("", "", "t", "", "l");
        xw.startElement("", "", "p", attrs);
        xw.characters(boost::lexical_cast<string>((*it).get_long_long()));
        xw.endElement("", "", "p");
        break;
      case yami::double_float:
        attrs.addAttribute("", "", "t", "", "d");
        xw.startElement("", "", "p", attrs);
        xw.characters(boost::lexical_cast<string>((*it).get_double_float()));
        xw.endElement("", "", "p");
        break;
      case yami::string:
        attrs.addAttribute("", "", "t", "", "s");
        xw.startElement("", "", "p", attrs);
        xw.characters((*it).get_string());
        xw.endElement("", "", "p");
        break;
      case yami::integer_array:
      {
        attrs.addAttribute("", "", "t", "", "i");
        size_t ybs;
        int* yb = (*it).get_integer_array(ybs);
        attrs.addAttribute("", "", "s", "", boost::lexical_cast<string>(ybs));
        xw.startElement("", "", "a", attrs);
        for (size_t i = 0; i < ybs; ++i)
        {
          xw.startElement("", "", "e");
          xw.characters(boost::lexical_cast<string>(yb[i]));
          xw.endElement("", "", "e");
        }
        xw.endElement("", "", "a");
        break;
      }
      case yami::boolean_array:
      {
        attrs.addAttribute("", "", "t", "", "b");
        size_t ybs;
        bool* yb = (*it).get_boolean_array(ybs);
        attrs.addAttribute("", "", "s", "", boost::lexical_cast<string>(ybs));
        xw.startElement("", "", "a", attrs);
        for (size_t i = 0; i < ybs; ++i)
        {
          xw.startElement("", "", "e");
          xw.characters(yb[i] ? "true" : "false");
          xw.endElement("", "", "e");
        }
        xw.endElement("", "", "a");
        break;
      }
      case yami::long_long_array:
      {
        attrs.addAttribute("", "", "t", "", "l");
        size_t ybs;
        long long* yb = (*it).get_long_long_array(ybs);
        attrs.addAttribute("", "", "s", "", boost::lexical_cast<string>(ybs));
        xw.startElement("", "", "a", attrs);
        for (size_t i = 0; i < ybs; ++i)
        {
          xw.startElement("", "", "e");
          xw.characters(boost::lexical_cast<string>(yb[i]));
          xw.endElement("", "", "e");
        }
        xw.endElement("", "", "a");
        break;
      }
      case yami::double_float_array:
      {
        attrs.addAttribute("", "", "t", "", "d");
        size_t ybs;
        double* yb = (*it).get_double_float_array(ybs);
        attrs.addAttribute("", "", "s", "", boost::lexical_cast<string>(ybs));
        xw.startElement("", "", "a", attrs);
        for (size_t i = 0; i < ybs; ++i)
        {
          xw.startElement("", "", "e");
          xw.characters(boost::lexical_cast<string>(yb[i]));
          xw.endElement("", "", "e");
        }
        xw.endElement("", "", "a");
        break;
      }
      case yami::string_array:
      {
        attrs.addAttribute("", "", "t", "", "s");
        size_t ybs = (*it).get_string_array_length();
        attrs.addAttribute("", "", "s", "", boost::lexical_cast<string>(ybs));
        xw.startElement("", "", "a", attrs);
        for (size_t i = 0; i < ybs; ++i)
        {
          xw.startElement("", "", "e");
          xw.characters((*it).get_string_in_array(i));
          xw.endElement("", "", "e");
        }
        xw.endElement("", "", "a");
        break;
      }
      case yami::nested_parameters:
      {
        xw.startElement("", "", "n", attrs);
        yami::parameters nested((*it).get_nested_parameters());
        process_parameters(&nested, xw);
        xw.endElement("", "", "n");
        break;
      }
      case yami::nested_parameters_array:
      {
        attrs.addAttribute("", "", "t", "", "n");
        size_t ybs = params->get_nested_array_length((*it).name());
        attrs.addAttribute("", "", "s", "", boost::lexical_cast<string>(ybs));
        xw.startElement("", "", "a", attrs);
        for (size_t i = 0; i < ybs; ++i)
        {
          xw.startElement("", "", "e");
          yami::parameters nested(params->get_nested_in_array((*it).name(), i));
          process_parameters(&nested, xw);
          xw.endElement("", "", "e");
        }
        xw.endElement("", "", "a");
        break;
      }
      case yami::unused:
        break;
      default:
        break;
      }
    }
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
      
      if (name.size() ==0 || msg.size() == 0)
      {
        LOGWARN("Bad query: " << query);
        throw bad_request();
      }

      // getting service from service discovery
      string ye = DISCOVERY.get(name);

      LOG("Service: " << name << " (" << ye << ")");
      
      yami::parameters params;

      // parsing xml body if exists
      SAXParser parser;

      try
      {
        content_handler ch(params);
        parser.setContentHandler(&ch);
        InputSource source(request.stream());
        parser.parse(&source);

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
        response.setContentType("text/xml");
        response.add("Expires", "-1");
        response.add("Cache-control", "no-cache");

        // converting yami output to xml
        // yami binary values are not supported

        XMLWriter xw(response.send(), XMLWriter::WRITE_XML_DECLARATION);
        xw.setNewLine("\n");
        xw.startDocument();
        xw.startElement("", "", "hsd");
        yami::parameters* reply = message->extract_reply();
        process_parameters(reply, xw);
        delete reply;
        xw.endElement("", "", "hsd");
        xw.endDocument();
        
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
    service_unavailable(const string& reason) throw()
    : reason_(reason)
    {
    }
    
    ~service_unavailable() throw()
    {
    }
    
    const char* what() const throw()
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

