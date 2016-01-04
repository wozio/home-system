#ifndef JSON_CONVERTER_H
#define	JSON_CONVERTER_H

#include "handler.h"
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
void process_json(data_t data, std::string& service, std::string& message,
  bool& expect_reply, long long& sequence_number, yami::parameters& params);

void yami_to_json(yami::parameters* params, long long sequence_number, data_t data, size_t& data_size);
void yami_to_json(std::string& result, std::string& reason, long long sequence_number, data_t data, size_t& data_size);

class incorrect_message
: public std::exception
{};

}

#endif	/* JSON_CONVERTER_H */

