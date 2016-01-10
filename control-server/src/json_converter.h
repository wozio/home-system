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
 * @param source Source service
 * @param service Target service
 * @param message Message
 * @param expect_reply Reply expectation
 * @param params Output yami parameters
 */
void process_json(data_t data, std::string& source, std::string& service, std::string& message,
  bool& expect_reply, long long& sequence_number, yami::parameters& params);

void yami_to_json(const std::string& destination, yami::parameters* params, long long sequence_number, data_t data, size_t& data_size);
void yami_to_json(const std::string& destination, const std::string& result, const std::string& reason, long long sequence_number, data_t data, size_t& data_size);

}

#endif	/* JSON_CONVERTER_H */

