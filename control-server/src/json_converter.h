#ifndef JSON_CONVERTER_H
#define	JSON_CONVERTER_H

#include <yami4-cpp/parameters.h>
#include <string>
#include <exception>

namespace home_system
{

/**
 * Extracts from json service, message and yami::parameters
 * @param json Input JSON
 * @param service Output service
 * @param message Output message
 * @param expect_reply Output reply expectation
 * @param params Output yami parameters
 */
void process_json(const char* json, std::string& service, std::string& message,
  bool& expect_reply, yami::parameters& params);

void process_parameters(yami::parameters* params, std::string& outstr);

class incorrect_message
: public std::exception
{};

}

#endif	/* JSON_CONVERTER_H */

