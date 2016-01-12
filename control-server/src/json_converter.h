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
 * @param data Input null terminated JSON string
 * @param source Source
 * @param target Target
 * @param message Message
 * @param expect_reply Reply expectation
 * @param sequence_number Sequence number of the message
 * @param params Output yami parameters
 */
void from_json(data_t data, std::string& source, std::string& target, std::string& message,
  bool& expect_reply, long long& sequence_number, yami::parameters& params);

/**
 * Convert successful YAMI reply into JSON string
 * @param source Source
 * @param target Target
 * @param params YAMI parameters
 * @param sequence_number Sequence number of the message
 * @param data Output JSON string
 * @param data_size Output JSON string size
 */
void to_json(const std::string& source, const std::string& target, const yami::parameters& params, long long sequence_number,
    data_t data, size_t& data_size);

/**
 * Convert failed YAMI reply into JSON string
 * @param source Source
 * @param target Target
 * @param reason Reason of failure
 * @param sequence_number Sequence number of the message
 * @param data Output JSON string
 * @param data_size Output JSON string size
 */
void to_json(const std::string& source, const std::string& target, const std::string& reason, long long sequence_number,
    data_t data, size_t& data_size);

}

#endif	/* JSON_CONVERTER_H */

